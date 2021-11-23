# to run model-inference of 4 VOCs (Alpha, Beta, Gamma, Delta) using data from the UK, S.Africa, Brazil, and India
# 6/10/2021
# 10/26/21 update Indian study, consider impact of prior immunity on VE
# 11/9/21 update India study, use more recent weather data as there is a noticeable shift


num_runs = 2  # number of runs
tno = 1:num_runs

num_ens = 500 # number of ensemble members
epi.model = 'SEIRSV' # susceptible-exposed-infectious-recovered-susc
stoch = T # run the model stochastically

# tag.evals = c('eq','rank', 'obs.more', 'obs.most', 'obs.only') # 'eq','rank', 'obs.only' - not good
tag.evals = c('obs.more', 'obs.most','obs.comb')

date.tag = '_2021-10-28'
date.stop = '2021/7/1'  # Stop model-inference at the end of June 2021

seasonality = T; # include seasonality
useRecentWeaData = T # whether to use more recent weather data for computing seasonal trends
doFcast = T # whether to do projection 
doCounterFact = T # whether the project is counterfactual, assuming no vax after June 2021


if(T){
  dir_data = '../data/'
  dir_code = '../scripts/'
  
  dir_res = paste0('../results/')
  
  
  library(data.table)
  library(magrittr)
  library(ggplot2)
  library(xlsx)
  library(MMWRweek)
  library(readr); library(readxl); library(writexl); library(stringi)
  library(lemon)
  library(tgp)
  library(scales) # for transparency
  library(R0)
  library(msm) # for the deltamethod function to compute variance
  
}

source(paste0(dir_code,'setSRparm.R'))
source(paste0(dir_code,'SEIRS.R'))
source(paste0(dir_code,'EAKF.R'))
source(paste0(dir_code,'Projection.R'))
source(paste0(dir_code,'get_relR0.R'))
source(paste0(dir_code,'set_tm2event.R'))


if(F){
  # if you're using a cluster for computing
  args=commandArgs(TRUE)
  tno=as.integer(args[1])
  
}

if(! file.exists(dir_res))  dir.create(dir_res,recursive = T)


da = read.csv(paste0(dir_data,'da_case.death.mob_uk.sa.br.in', date.tag, '.csv'), stringsAsFactors = F)  %>% data.table()
da = da[!is.na(case.uk)]
da[date < as.Date('2020-02-09') & is.na(da)] = 0
da.full = da
N = 1e6; # per 1 M
num_gr = num_obs = length(N); # no age structure



# parms from flu hk paper, AH/T model, from Yuan et al. 2021
Rwea_parm.bounds = rbind(
  c(2.34, 2.93), # R0max
  c(.86,1.18), # R0diff - this determines the magnitude of seasonality
  c(2.2,4.0)/1000, # qmin
  c(17,20)/1000, # qmax
  c(10.2,11.3)/1000, # qmid
  c(20.2,24), # Tc
  c(.4, 5.1), # Tdiff
  c(.95,1.54) # Texp
)
# use the mean instead to reduce uncertainty?
Rwea_parm.bounds = cbind(rowMeans(Rwea_parm.bounds),rowMeans(Rwea_parm.bounds))
rownames(Rwea_parm.bounds) = c('R0max','R0diff','qmin','qmax','qmid','Tc','Tdiff','Texp')


source(paste0(dir_code,'set_tm2event.R'))


seed = .1

# SR
doSR = T
percSRmajor = .1
SR.perc = .05
SR.perc.local = .03;
SR.perc.full = 0.03
SR.var.local= c('beta','Tei','Tir','Trs','Td.mean','Td.sd','p.mob','alpha','ifr')
SR.var.full= c('beta','Tir','ifr') # ,'alpha','ifr'
SR.var.tx = c('beta','Tir')
# do three diff categories: all combined, ms/mh, cumc
donotUpdateS1stWave = T # do not allow the filter to update S during first wave
rednUpdateEI = T # do not allow or reduce the level allowed, the filter to update E or I - it takes OEVr all efforts

{
  fn_getWkLowIFR = function(idx.all){
    consecutive =  idx.all[-1] - idx.all[-length(idx.all)]
    i.div = which(consecutive >1)
    if(length(i.div) > 0){
      w.div = idx.all[which(consecutive >1)]
      grps = list()
      if(length(i.div) == 1){
        grps[[1]] = idx.all[1]: w.div[1]
        grps[[2]] =idx.all[i.div[1]+1]: tail(idx.all,1)
      } else {
        for(id in 1: length(i.div)){
          if(id == 1){
            grps[[id]] = idx.all[1]: w.div[id]
          } else if (id == length(i.div)){
            # both before and after
            
            grps[[id]] = idx.all[(i.div[id-1]+1): i.div[id]]
            grps[[id+1]] = idx.all[i.div[id]+1]: tail(idx.all,1)
          } else {
            grps[[id]] = idx.all[(i.div[id-1]+1): i.div[id]] # (idx.all[i.div[id-1]+1]) : w.div[id]
          }
          
        }
      }
      
      dur = lapply(grps, length) %>% unlist
      igrs = which(dur > 5)
      
      WkLowIFR = NULL
      if(length(igrs) > 0){
        for(igr in igrs){
          WkLowIFR = c(WkLowIFR, grps[[igr]])
        }
      }
    } else {
      WkLowIFR = idx.all
    }
    
    WkLowIFR
  }
}

locs = 'in' # for India, if to include other countries, use c('uk', 'sa','br','in')
for(loc.t in locs){  
  print(loc.t)
  
  
  da.t = da[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
    setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))
  da.t = da.t[case > 2 | death > 0]
  da.t = da.t[complete.cases(da.t)]
  
  da.full.t = da.t 
  da.t = da.t[date < as.Date(date.stop)]  # stop at the end of Aug and save the rest for validate
  
  
  if(loc.t %in% c('in', 'uk','sa', 'br')){
    da.vacc = read.csv(paste0(dir_data,'vx.lagged.per1Mpop_',loc.t, date.tag,'.csv')) %>% data.table()
    da.vacc$date = da.vacc$date %>% as.Date
  }  else {
    da.vacc = data.table(date = as.Date('2021/6/1'), n.v1=0, n.v2=0)
  }
  
  
  
  if(loc.t == 'uk'){
    IsLargeCountry = F
    excludeLockDown = T
    # 3rd lockdown, starting 1/6/21. some reopening in March
    # https://www.legislation.gov.uk/uksi/2021/8/pdfs/uksi_20210008_en.pdf
    wkLockDown = which(as.Date(da.t$date) %in% seq(as.Date('2021/1/6')+14, as.Date('2021/3/1'), by = 'day'))
    # for vaccination
    vax.start = as.Date('2020/12/08')
    massvax.start = vax.start
    VE1 = .85
    VE2 = .95  # higher b/c we are using mortality data too
    
    
    redn.priority = runif(num_ens, 1/3, 1/2) # lower risk of infection for priority groups?
    
    seed_max = da.t$case[1] * 100 # 10000
    end1stWave = which(da.t$date == as.Date("2020-08-16")) # 30 - 5; # end of first wave, after which S can be updated by the filter
    main2ndWave = which(da.t$date == as.Date('2020-11-15')) # 38
    
    wk.WkLowIFR = which(as.Date(da.t$date) %in% seq(as.Date('2020/6/1'), as.Date('2020/10/1'), by = 'day'))
    wk.2strtHighIFR = which(as.Date(da.t$date) %in% seq(as.Date('2020/10/1'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHighIFR]
    wk.2strtHigherIFR = which(as.Date(da.t$date) %in% seq(as.Date('2021/1/1'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHigherIFR]
    
    tm_rednUpdateEI = which(da.t$date == as.Date('2020-12-06')) + 0:3 #  41:45 # main2ndWave + 0: 2 # 
    tm_largerVar = 5 # number of initial weeks to have larger OEV
    pOEV = 1
    
    ifr_bounds = c(.1, 1.5) / 100
    # increasing detection
    alpha_bounds = c(.01, .15);# c(.01, .2); # reporting rate
    alpha_bounds2 = c(.1, .3); # reporting rate, later wave to reflect increase in detection rate
    
    DAalpha_bounds2 = c(.05, .5)  
    SRalpha_bounds2 = c(.1, .3) # fall - started to increase
    SRalpha_bounds3 = c(.15, .3)
    wk.summer = which(as.Date(da.t$date) %in% seq(as.Date('2020/06/30'), as.Date('2020/9/1'),by='day'))
    SRalpha_bounds.summer = c(0.05, .2) # summer lower detection
    DAalpha_bounds3 = c(.1, .5)
    
    SRifr_bounds1 = c(.01, 1) / 100 
    SRifr_bounds2 = c(.01, .5) / 100 #  early phase
    SRifr_bounds3 = c(.01, .6) / 100 
    SRifr_bounds.WkLowIFR = c(.01, .3) / 100 
    
    beta_bounds = c(.5, .8) # c(.5, 1) #  
    p.mob_bounds = c(.5, 1.5); # scaling for mobility
    Td.mean_bounds =  c(5,7) # mean Td: reporting delay
    Td.sd_bounds = c(1,3) # Td, sd: reporting delay sd
    
  } else if(loc.t == 'br'){
    
    IsLargeCountry = T # allow slower changes in S b/c likely traveling wave
    
    # for vaccination
    vax.start = as.Date('2021/1/18')
    massvax.start = as.Date('2021/6/1') # very slow rollout
    VE1 = .45
    VE2 = .55
    
    
    seed_max = da.t$case[1] * 50 # 10000
    end1stWave = which(da.t$date == as.Date("2020-10-04")) # 30; # end of first wave, after which S can be updated by the filter
    main2ndWave = which(da.t$date == as.Date('2020-11-8')) # 38
    
    c2d = da.t$death[-c(nrow(da.t)-0:1)] / da.t$case[-c(1,2)]
    c2d[is.infinite(c2d)] = max(c2d[!is.infinite(c2d)])
    idx.all = which(c2d[6:main2ndWave] < quantile(c2d[6: (main2ndWave)], probs = .5))
    idx.all = min(idx.all) : max(idx.all)
    # are they consecutive change
    # fn_getWkLowIFR(idx.all)
    wk.WkLowIFR = 5 + fn_getWkLowIFR(idx.all)
    da.t$date[wk.WkLowIFR]
    
    # get the week with increasing IFR during 2nd wave
    wk.2strtHighIFR = which(as.Date(da.t$date) %in% seq(as.Date('2021/1/1'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHighIFR]
    
    # even higher IFR when healthcare systems collapse
    wk.2strtHigherIFR = which(as.Date(da.t$date) %in% seq(as.Date('2021/2/1'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHigherIFR]
    
    tm_rednUpdateEI = main2ndWave + 0: 3 # 35:37
    tm_largerVar = 5 # number of initial weeks to have larger OEV
    pOEV = 2 # LARGER OEV B/C NOISY DATA
    
    ifr_bounds = c(.01, 1) / 100  
    SRifr_bounds1 = c(.01, .7) / 100 
    SRifr_bounds2 = c(.01, .6) / 100  # early for 2nd wave
    SRifr_bounds3 = c(.01, 1) / 100  # later for 2nd wave
    SRifr_bounds.WkLowIFR = c(.01, .35) / 100
    
    
    # 4/18/21
    alpha_bounds = c(.01, .1); # reporting rate
    alpha_bounds2 = c(.02, .1); # reporting rate, later wave to reflect increase in detection rate
    
    
    DAalpha_bounds2 = c(.02, .3)
    SRalpha_bounds2 = c(.02, .1)
    
    # beta_bounds = c(.5, .8)  - .1 # too low ?
    beta_bounds = c(.5 - .1, .8)
    p.mob_bounds = c(.5, 1.5); # scaling for mobility
    Td.mean_bounds =  c(5,8) # mean Td: reporting delay
    Td.sd_bounds = c(1,3) # Td, sd: reporting delay sd
    
  } else if(loc.t == 'sa'){
    IsLargeCountry = F
    vax.start = as.Date('2021/2/18')  # mid Feb 2021, adding the lag
    massvax.start = as.Date('2021/6/1') # very slow rollout
    VE1 = .6
    VE2 = .7
    
    
    seed_max = da.t$case[1] * 20 # 10000
    end1stWave = which(da.t$date == as.Date("2020-08-30")) # 25; # end of first wave, after which S can be updated by the filter
    main2ndWave = which(da.t$date == as.Date('2020-11-1')) # 38
    
    c2d = da.t$death[-c(nrow(da.t)-0:1)] / da.t$case[-c(1,2)]
    c2d[is.infinite(c2d)] = max(c2d[!is.infinite(c2d)])
    c2d[is.na(c2d)] = 0
    idx.all = which(c2d[6:main2ndWave] < quantile(c2d[6: (main2ndWave)], probs = .33))
    idx.all = min(idx.all) : max(idx.all)
    # are they consecutive change
    # fn_getWkLowIFR(idx.all)
    wk.WkLowIFR = 5 + fn_getWkLowIFR(idx.all)
    da.t$date[wk.WkLowIFR]
    
    # get the week with increasing IFR during 2nd wave
    wk.2strtHighIFR = which(as.Date(da.t$date) %in% seq(as.Date('2020/12/10'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHighIFR]
    
    tm_rednUpdateEI = which(da.t$date == as.Date('2020-11-22')) + 0: 3 # 39:41
    tm_largerVar = 10 # number of initial weeks to have larger OEV - low numbers in the first ~10 wks
    pOEV = 1.5
    
    
    # ifr_bounds = c(.01, 1) / 100  # over estimate early deaths
    ifr_bounds = c(.01, .3) / 100
    SRifr_bounds1 = c(.01, .2) / 100  # lower for 1st wave, later weeks
    SRifr_bounds2 = c(.01, .2) / 100  # lower for 2nd wave
    SRifr_bounds.WkLowIFR = c(.01, .15) / 100
    
    alpha_bounds = c(.01, .06); # reporting rate
    alpha_bounds2 = c(.02, .08); # reporting rate, later wave to reflect increase in detection rate
    
    DAalpha_bounds2 = c(.02, .3)
    SRalpha_bounds2 = c(.02, .08)
    
    beta_bounds = c(.5, .8) - .1 
    p.mob_bounds = c(.5, 1.5); # scaling for mobility
    Td.mean_bounds =  c(5,8) # mean Td: reporting delay
    Td.sd_bounds = c(1,3) # Td, sd: reporting delay sd
    
  } else if(loc.t == 'in'){  # India
    
    IsLargeCountry = F # large country
    vax.start = as.Date('2021/1/30')  # Feb 2021, adding the lag
    massvax.start = as.Date('2021/6/1') # slow rollout
    VE1 = .3 # .33 # reduction due to b.1.617.2
    VE2 = .67 # for AZ vaccine, more used in India
    # source: https://www.medrxiv.org/content/10.1101/2021.05.22.21257658v1.full.pdf
    # Bernal JL, Andrews N, Gower C, Gallagher E, Simmons R, Thelwall S, Stowe J, Tessier E, Groves N, Dabrera G, Myers R, Campbell CNJ, Amirthalingam G, Edmunds M, Zambon M, Brown KE, Hopkins S, Chand M, Ramsay M. Effectiveness of Covid-19 Vaccines against the B.1.617.2 (Delta) Variant. New England Journal of Medicine. 2021;385(7):585-94. doi: 10.1056/NEJMoa2108891. PubMed PMID: WOS:000675554200001.
    
    
    redn.priority = 1
    
    seed_max = da.t$case[1] * 2000 # higher if started on 3/22/20
    end1stWave = which(da.t$date == as.Date("2021-01-17")) # end of first wave, after which S can be updated by the filter
    main2ndWave = which(da.t$date == as.Date('2021-03-21')) #
    
    c2d = da.t$death[-c(nrow(da.t)-0:1)] / da.t$case[-c(1,2)]
    c2d[is.infinite(c2d)] = max(c2d[!is.infinite(c2d)])
    c2d[is.na(c2d)] = 0
    idx.all = which(c2d[6:main2ndWave] < quantile(c2d[6: (main2ndWave)], probs = .33))
    idx.all = min(idx.all) : max(idx.all)
    # are they consecutive change
    # fn_getWkLowIFR(idx.all)
    wk.WkLowIFR = 5 + fn_getWkLowIFR(idx.all)
    da.t$date[wk.WkLowIFR]
    
    # get the week with increasing IFR during 2nd wave
    wk.2strtHighIFR = which(as.Date(da.t$date) %in% seq(as.Date('2021-04-01'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHighIFR]
    
    wk.2strtHigherIFR = which(as.Date(da.t$date) %in% seq(as.Date('2021-05-01'), length.out = 7, by = 'day'))
    da.t$date[wk.2strtHigherIFR]
    
    
    tm_rednUpdateEI = which(da.t$date == as.Date('2021-03-07')) + 0: 3 # 39:41
    tm_largerVar = 10 # number of initial weeks to have larger OEV - low numbers in the first ~10 wks
    pOEV = 1.5
    
    
    ifr_bounds = c(.01, .3) / 100 # lower than others, due to later outbreak?
    
    SRifr_bounds1 = c(.01, .15) / 100  # lower for 1st wave, later weeks
    SRifr_bounds2 = c(.01, .15) / 100  # early for 2nd wave, higher severity for b1.617.2
    SRifr_bounds3 = c(.01, .3) / 100  # later for 2nd wave, when healthcare system overwhelmed
    SRifr_boundsPostVac = c(.01, .05) / 100  # return to ~normal after the 2nd wave, and with vx to lower mortality
    
    SRifr_bounds.WkLowIFR = c(.01, .08) / 100
    
    alpha_bounds = c(.01, .06); # reporting rate # too high
    alpha_bounds = c(.0001, .02); # reporting rate
    alpha_bounds2 = c(.02, .08); # reporting rate, later wave to reflect increase in detection rate
    SRalpha_bounds2 = c(.02, .08)
    
    # 10/27/21
    DAalpha_bounds2 = c(.015, .3) # lower the lower bound
    DAalpha_bounds2 = c(.01, .3) # lower the lower bound - test 11
    DAalpha_boundsPostVac = c(.01, .3)
    
    alpha_bounds2 = c(.015, .08); # reporting rate, later wave to reflect increase in detection rate, used for transitional weeks
    SRalpha_bounds2 = c(.01, .08)
    
    wk2lowerAlphaPostVac = which(as.Date(da.t$date) %in% seq(as.Date('2021-08-15'), length.out = 7, by = 'day'))
    da.vacc$date = da.vacc$date %>% as.Date()
    da.t$date = da.t$date %>% as.Date()
    estcumI = merge(da.t, da.vacc, all.x = T, by = 'date')
    estcumI$Itot = estcumI$case / .04 # mean(alpha_bounds2)
    estcumI$estV = estcumI$n.v1 * mean(c(VE2))  # VE2
    estcumI$Imm = rowSums(estcumI[,c('Itot', 'estV')], na.rm = T) / N
    wk2lowerAlphaPostVac = which(cumsum(estcumI$Imm) > .5) %>% head(1)
    SRalpha_boundsPostVac = c(.01, .05) # reporting rate, after higher vacc uptake and/or higher prior infection rate -> milder disease -> lower detection rate
    # use the same timing to lower mortality risk
    wk2lowerIFRPostVac = wk2lowerAlphaPostVac + 2 # with some delay
    da.t$date[wk2lowerAlphaPostVac]  # 2021/5/23
    da.t$date[wk2lowerIFRPostVac]
    
    beta_bounds = c(.5, .8)  - .1  # works better with the prior and posterior closer
    # it is lower than one would expect, but likely b/c intense transmission does not occur everywhere in the entire, very large country
    # beta_bounds = c(.5, .8)  # prior constantly higher than posterior -> larger diff b/w S and cumI
    # beta_bounds = c(.5, .8) - .05  # 
    p.mob_bounds = c(.5, 1.5); # scaling for mobility
    Td.mean_bounds =  c(5,8) # mean Td: reporting delay
    Td.sd_bounds = c(1,3) # Td, sd: reporting delay sd
  }
  
  
  
  # 10/27/21
  # diff setting for VE
  # 1) higher VE1 but not VE2, 2) same VE1, higher VE2, 3) higher VE1 and VE2
  
  VEpriorInf_vec = data.table(VE1priorInf = c(VE1, seq(.4, .60, by = .1), rep(VE1, 3), seq(.5, .9, by = .1)),
                              VE2priorInf = c(VE2, rep(VE2, 3),
                                              seq(.75, .95, by = .1),
                                              seq(.75, length.out = length(seq(.5, .9, by = .1)), by = .05) %>% pmin(.95))
  )
  
  VE1priorInf = VEpriorInf_vec[dummy_ive]$VE1priorInf
  VE2priorInf = VEpriorInf_vec[dummy_ive]$VE2priorInf
  
  sce.t = paste0('VEpriorInf', paste0(VEpriorInf_vec[dummy_ive],collapse = '-')) 
  
  
  
  
  obs_i = (da.t$case) %>% as.matrix() 
  obs_vars_i = obs_i
  for(j in 1:num_obs){
    tmp=rep(0,nrow(da.t))
    for (i in 3:nrow(da.t)){
      tmp[i]=mean(obs_i[(i-2):(i-0),j]);
    }
    obs_vars_i[,j]= (c(rep(N/1000,tm_largerVar),rep(N/1e3,nrow(da.t)-tm_largerVar)) + (tmp^2)/50) * pOEV;
    
    # larger for br
    
  }
  
  obs_d = (da.t$death) %>% as.matrix() 
  obs_vars_d = obs_d
  for(j in 1:num_obs){
    tmp=rep(0,nrow(da.t))
    for (i in 3:nrow(da.t)){
      tmp[i]=mean(obs_d[(i-2):(i-0),j]);
    }
    
    
    if(loc.t == 'uk'){
      
      obs_vars_d[,j]= (c(rep(N/1e4,tm_largerVar),rep(N/5e4,nrow(da.t)-tm_largerVar)) + 
                         pmin((tmp^2)/10, tmp*5)
      ) * pOEV;
    } else {
      obs_vars_d[,j]= (c(rep(N/1e4,tm_largerVar),rep(N/1e4,nrow(da.t)-tm_largerVar)) + 
                         pmin((tmp^2)/10, tmp*20)
      ) * pOEV;
    }
  }
  
  # get relative mobility
  rel.mob = lhs(num_ens, rect = cbind(1 + da.t$mob.bus/100, 1+da.t$mob.bus/100))  %>% t # as.matrix() 
  tmp = cbind(1 + da.t$mob.bus/100, 1+da.t$mob.full/100, (1+pmax(da.t$mob.full * 1.5, da.t$mob.bus)/100))
  
  # get relative R0 for seasonality
  if(useRecentWeaData){
    print('Use recent weather data')
    if(loc.t == 'in')
      da.wea = read.csv(paste0(dir_data, 'wea.wk_india_Jan2020_Nov2021.csv')) 
    
    relR0 = fn_getRelR0.loc(da.t = da.wea, ref.wk, Rwea_parm.bounds=Rwea_parm.bounds, smooth = T)
    
  } else {
    # use historical weather data (2000 - 2020)
    relR0 = fn_getRelR0(loc.t, ref.wk = da.t$week[1], Rwea_parm.bounds=Rwea_parm.bounds)
    
  }
  
  
  tmpMob = cbind(date = da.t$date, rel.mob = rel.mob %>% rowMeans)
  tmpR0 = cbind(week = 1:53, relR0 = relR0 %>% rowMeans)
  tda2 = merge(da.t, tmpR0, all = T, by = 'week')
  tda2 = merge(tda2, tmpMob, by = 'date', all = T)
  tda2 = tda2[complete.cases(tda2)]
  
  
  
  weeks = da.t$week # for seasonality if applicable
  Week.starts = da.t$date
  
  # do 300 runs - set tno 
  for(ir in tno){ # tno
    
    print(paste('run', ir))
    
    
    So=t(lhs(num_ens,rect = rbind(cbind(.99, 1) * N, # S0
                                  cbind(seed_max/20,seed_max/2), # E0
                                  cbind(seed_max/20,seed_max/2), # I0
                                  cbind(0,seed_max/100) # deaths0
    )))
    
    
    S0 = So[1:num_gr,,drop=F]
    E0 = So[1:num_gr+num_gr,,drop=F]
    I0 = So[1:num_gr+num_gr*2,,drop=F]
    D0 = So[1:num_gr+num_gr*3,,drop=F]
    
    newItot = I0; newIobs = I0; 
    rownames(S0)=paste0('S',1:num_gr); 
    rownames(E0)=paste0('E',1:num_gr); 
    rownames(I0)=paste0('I',1:num_gr); 
    rownames(D0)=paste0('death',1:num_gr);
    rownames(newItot)=paste0('newItot',1:num_gr); 
    rownames(newIobs)=paste0('newIobs',1:num_gr); 
    
    
    imm_bounds = c(2, 3) * 365
    
    
    
    
    parm.bounds = rbind(beta_bounds, # beta for all loc's
                        c(2,5), # Tei: time from exposed to infectious: incubation time mean = 4
                        c(2,5), # Tir: time from infectous to not (remOEVd)
                        imm_bounds, # immunity period, Trs
                        Td.mean_bounds,  # mean Td: reporting delay
                        Td.sd_bounds, # Td, sd: reporting delay sd
                        p.mob_bounds, # scaling for mobility
                        alpha_bounds, # reporting rate
                        ifr_bounds # infection fatality risk
    )
    parm.names = c('beta','Tei','Tir','Trs', 'Td.mean', 'Td.sd', 'p.mob','alpha', 'ifr')
    
    rownames(parm.bounds) = parm.names
    parm.bounds
    
    parm0=t(lhs(num_ens,parm.bounds)); rownames(parm0)=rownames(parm.bounds)
    
    
    STATE0=rbind(S0, E0, I0, D0, newIobs, newItot, parm0)
    state.names=rownames(STATE0)
    idx.obs_i= which(state.names == 'newIobs1')  
    idx.obs_d= which(state.names == 'death1') 
    idx.newItot = which(state.names == 'newItot1') 
    idx.e = which(state.names == 'E1') 
    idx.i = which(state.names == 'I1') 
    
    num_state = 4 + 2
    
    DA.bounds = rbind(matrix(c(rep(0,num_state * num_gr), rep(N,num_state)),num_state * num_gr,2),
                      cbind(parm.bounds[,1]*.5, parm.bounds[,2]*1.5)) 
    rownames(DA.bounds)=state.names
    DA.bounds[c('E1','I1'),2] = N * .15
    DA.bounds['death1',2] = N / 100 
    DA.bounds['S1',1] = N / 10
    DA.bounds[c('newItot1','newIobs1'),2] = N * .2
    
    DA.bounds['Td.mean',] = parm.bounds['Td.mean',]
    DA.bounds['Trs',1] = 200
    
    DA.bounds['beta',2] = parm.bounds['beta',2]*2
    DA.bounds['alpha',1] = parm.bounds['alpha',1]*.75
    DA.bounds['alpha',2] = pmax(parm.bounds['alpha',2]*2, .1)
    DA.bounds['ifr',] = c(parm.bounds['ifr',1]*.25, parm.bounds['ifr',2]*1.25)
    DA.bounds['ifr',1] = pmin(DA.bounds['ifr',1], 1e-4)

    SR.bounds = parm.bounds 
    rownames(SR.bounds)=parm.names
    SR.bounds['alpha',] = parm.bounds['alpha',] 
    SR.bounds['ifr',] = parm.bounds['ifr',] 
    SR.bounds0 = SR.bounds
    
    # FOR TESTING DIFF HYPOTHESES
    SR.bounds.wider = cbind(parm.bounds[,1] * 1.2, parm.bounds[,2]*1.2) # higher than normal, for increase
    rownames(SR.bounds.wider)=parm.names
    SR.bounds.wider['alpha',] = SR.bounds['alpha',] # * 1.2 #  + .05
    SR.bounds.wider['p.mob',] = SR.bounds['p.mob',] # c(.7, 1.3); # 
    SR.bounds.wider['Tei',] = parm.bounds['Tei',]
    # SR.bounds.wider['Tir',2] = parm.bounds['Tir',2] * 1.2
    SR.bounds.wider['Tir',1] = parm.bounds['Tir',1] #  * 1.2
    SR.bounds.wider['Tir',2] = parm.bounds['Tir',2] * 1.1
    SR.bounds.wider['ifr',] = SR.bounds['ifr',]
    
    SR.bounds.wider2 = cbind(parm.bounds[,1] * 1.3, parm.bounds[,2]*1.4) # higher than normal, for increase
    rownames(SR.bounds.wider2)=parm.names
    SR.bounds.wider2['alpha',] = SR.bounds['alpha',] # * 1.2 # + .05
    SR.bounds.wider2['p.mob',] = SR.bounds['p.mob',] # c(.7, 1.3); # 
    SR.bounds.wider2['Tei',] = parm.bounds['Tei',]
    # SR.bounds.wider2['Tir',1] = parm.bounds['Tir',1] 
    # SR.bounds.wider2['Tir',2] = parm.bounds['Tir',2] * 1.25
    SR.bounds.wider2['Tir',1] = parm.bounds['Tir',1] # * 1.2
    SR.bounds.wider2['Tir',2] = parm.bounds['Tir',2]  * 1.2
    SR.bounds.wider2['ifr',] = SR.bounds['ifr',]
    
    tm.ini=1; tmstep=7; newI.previous = NULL; inflat=1.03; state0=STATE0
    
    severity['death',] = STATE0['ifr',]
    
    # model training
    # tmp = try(load(paste0(dir_res, loc.t,'_train_r',ir,'.RData')))
    tmp = try(load(paste0(dir_res, loc.t,'_train_',sce.t,'_r',ir,'.RData')))
    if(class(tmp) == 'try-error'){  # model haven't been trained, do it first  
      print('training')
      res.train = EAKF(epi.model=epi.model, num_ens=num_ens,inflat=1.03, 
                       obs_i=obs_i, obs_vars_i=obs_vars_i, # case
                       obs_d=obs_d, obs_vars_d=obs_vars_d,
                       weeks=weeks,Week.starts=Week.starts,
                       parm.bounds=parm.bounds, DA.bounds=DA.bounds, SR.bounds=SR.bounds, 
                       parm.names = rownames(parm.bounds), rel.mob = rel.mob,
                       state0=STATE0, state.names=rownames(STATE0),
                       severity = severity,
                       tm.ini=1, tmstep=7,
                       newI.previous = NULL,
                       SRparms = SRparms
      )
      
      # save(res.train, file = paste0(dir_res, loc.t,'_train_r',ir,'.RData'))
      save(res.train, file = paste0(dir_res, loc.t,'_train_',sce.t,'_r',ir,'.RData'))
      
    } 
    
    
    if(doFcast){
      print('retrospective projection')
      
      eval.t = 'obs.more'

      nfcast = as.numeric(as.Date(tail(da.full.t$date, 1)) - as.Date(tail(Week.starts,1))) / 7
      fcast.wk.starts = as.Date(tail(Week.starts,1)) + seq(7, length.out = nfcast, by = 7)
      tmp.mob = (1 + da.full.t$mob.bus/100) %>% tail(nfcast) # use actual mobility
      tmp.x = data.table(week = 1:length(tmp.mob), rel.mob = tmp.mob)
      fcast.mob = data.table(week = nrow(tmp.x)+1:nfcast, rel.mob = tmp.mob)
      fcast.mob = lhs(num_ens, rect = cbind(fcast.mob$rel.mob, fcast.mob$rel.mob))  %>% t # as.matrix() 
      
      
      weeks.fcast = MMWRweek(fcast.wk.starts)['MMWRweek'] %>% unlist
      state.fcast0 = res.train$xpost.last
      state.fcast0 = state.fcast0[eval == eval.t]
      state.fcast0$eval = NULL
      state.fcast0 = state.fcast0 %>% as.matrix()
      rownames(state.fcast0) = state.names
      
      # time series recording waning immunity lost
      xpost_mean.t = res.train$xpost_mean[eval == eval.t]
      ts.ImmLoss.t = (N - xpost_mean.t[,'S1'] - xpost_mean.t[,'E1'] - xpost_mean.t[,'I1']) / xpost_mean.t[,'Trs'] * 7
      
      
      # is there a need to adjust det rate and ifr, given continued vx? 
      # look at if in the last 6 weeks, did alpha/ifr changed by more than 10%?
      # if so, generate projected value, otherwise no additional adjustment
      # cut point to use projected estimates
      p.cut = ifelse(date.stop < as.Date('2021/08/01'), 1.1, 1.2)
      p.cut2 = ifelse(date.stop < as.Date('2021/08/01'), 1.05, 1.1)
      
      n.wk.proj = 4 # just do 4 weeks
      n.wk.tr = 6 # 6 wk for this simple model
      
      
      p.alpha.proj = rep(1, nfcast)  # do not adjust detection rate - set all to 1
      
      
      # IFR
      # n.wk.tr = 8 # 6 wk for this simple model
      # n.wk.proj = 6 # ifelse(as.Date(date.stop) < as.Date('2021/07/15'), 8, 4) # just do 4 weeks 
      if(date.stop >= as.Date('2021/08/01')){
        p.ifr.proj = rep(1, nfcast)  # now adjustment
      } else if(date.stop > as.Date('2021/07/01') & date.stop <= as.Date('2021/07/15')){
        ifr.proj = cbind(week=n.wk.tr+1:nfcast, ifr = NA) %>% as.data.frame()
        p.ifr.proj = rep(1, n.wk.proj)
        # ifr.recent = xpost_mean.t %>% .$ifr %>% tail(n.wk.tr)
        ifr.recent = xpost_mean.t %>% .$ifr %>% stats::filter(filter=rep(1/3,3)) %>% .[complete.cases(.)] %>% tail(n.wk.tr)  # use moving average to reduce fluctuation
        week= 1:n.wk.tr
        if(mean(ifr.recent[1:2]) / mean(tail(ifr.recent,2)) > p.cut){
          f = lm(ifr.recent ~ week)
          ifr.proj$ifr = predict(f, ifr.proj)
          p.ifr.proj = (ifr.proj$ifr %>% head(n.wk.proj)) / mean(tail(ifr.recent, 2))  # only do it for 6 weeks 
        }
        p.ifr.proj = c(p.ifr.proj, rep(tail(p.ifr.proj, 1), nfcast - n.wk.proj))
        p.ifr.proj = p.ifr.proj %>% pmax(.5)
        p.ifr.proj
      } else {
        # stop too early, no signals, need another method
        # assume it could go back to low vax once imm > 60%
        da.full.t$date = da.full.t$date %>% as.Date()
        da.vacc$date = da.vacc$date %>% as.Date()
        estcumI = merge(da.full.t, da.vacc, all.x = T, by = 'date')
        estcumI$Itot = estcumI$case / .04 # mean(alpha_bounds2)
        estcumI$estV = estcumI$n.v1 # * mean(c(VE2))  # VE2
        estcumI$Imm = rowSums(estcumI[,c('Itot', 'estV')], na.rm = T) / N
        wk.low.ifr = which(fcast.wk.starts == estcumI$date[which(cumsum(estcumI$Imm) > .6) %>% head(1)])
        low.ifr = (xpost_mean.t %>% .$ifr %>% min()) #  * .6  # similar to filter estimates when run the model up to 8/1/21
        
        # from wk 1 to wk.low.ifr
        d.ifr = (tail(xpost_mean.t$ifr, 1) - low.ifr) / wk.low.ifr
        ifr.proj = c(tail(xpost_mean.t$ifr, 1) - (1: wk.low.ifr) * d.ifr,
                     rep(low.ifr, nfcast - wk.low.ifr)) 
        p.ifr.proj = ifr.proj /  mean(tail(xpost_mean.t$ifr, 2))
        # p.ifr.proj = p.ifr.proj %>% pmax(.5)
        p.ifr.proj
      }
      
      
      # beta? 
      beta.proj = cbind(week=n.wk.tr+1:nfcast, beta = NA) %>% as.data.frame()
      p.beta.proj = rep(1, n.wk.proj)
      # beta.recent = xpost_mean.t %>% .$beta %>% tail(n.wk.tr)
      beta.recent = xpost_mean.t %>% .$beta %>% stats::filter(filter=rep(1/3,3)) %>% .[complete.cases(.)] %>% tail(n.wk.tr)  # use moving average to reduce fluctuation
      week= 1:n.wk.tr
      if(mean(beta.recent[1:2]) / mean(tail(beta.recent,2)) > p.cut2){
        f = lm(beta.recent ~ week)
        beta.proj$beta = predict(f, beta.proj)
        p.beta.proj = (beta.proj$beta %>% head(n.wk.proj)) / mean(tail(beta.recent, 2))  # only do it for 6 weeks 
      }
      p.beta.proj = c(p.beta.proj, rep(tail(p.beta.proj, 1), nfcast - n.wk.proj))
      p.beta.proj = p.beta.proj %>% pmax(.5)
      p.beta.proj
      
      # Tir? 
      Tir.proj = cbind(week=n.wk.tr+1:nfcast, Tir = NA) %>% as.data.frame()
      p.Tir.proj = rep(1, n.wk.proj)
      # Tir.recent = xpost_mean.t %>% .$Tir %>% tail(n.wk.tr)
      Tir.recent = xpost_mean.t %>% .$Tir %>% stats::filter(filter=rep(1/3,3)) %>% .[complete.cases(.)] %>% tail(n.wk.tr)  # use moving average to reduce fluctuation
      week= 1:n.wk.tr
      if(mean(Tir.recent[1:2]) / mean(tail(Tir.recent,2)) > p.cut2){
        f = lm(Tir.recent ~ week)
        Tir.proj$Tir = predict(f, Tir.proj)
        p.Tir.proj = (Tir.proj$Tir %>% head(n.wk.proj)) / mean(tail(Tir.recent, 2))  # only do it for 6 weeks 
      }
      p.Tir.proj = c(p.Tir.proj, rep(tail(p.Tir.proj, 1), nfcast - n.wk.proj))
      p.Tir.proj = p.Tir.proj %>% pmax(.5)
      p.Tir.proj
      
      newI.previous.t= res.train$newI.previous
      if(is.null(newI.previous.t)){
        # use the weekly mean instead
        tmp = res.train$states_stats[eval == eval.t & state == 'newItot1']
        newI.previous.t = matrix(0, nrow(tmp) * 7, num_ens)
        for(ii in 1:nrow(tmp)){
          newI.previous.t[7 * (ii - 1)+1:7,] = matrix(runif(num_ens,tmp[ii]$iqr.lwr, tmp[ii]$iqr.upr) / 7, nrow = 7, ncol=num_ens, byrow = T)
        }
      }
      
      severity.t = severity
      severity.t['death',] = state.fcast0['ifr',]
      
      
      
      seed.t = 1/30
      
      # do projection here
      {
        
        print(sce.t)
        
        # use actual data for mobility and vacc
        fcast.mob.t = fcast.mob
        fda.vacc.t = da.vacc
        # get the number of people lost immunity due to immune evasion
        cum.dS.po.t = res.train$newVstat[eval == eval.t]$cum.dS
        VE1priorInf.t = VE1priorInf #  VEpriorInf_vec[iv]$VE1priorInf
        VE2priorInf.t = VE2priorInf # VEpriorInf_vec[iv]$VE2priorInf
        
        # generate counter factual - no further vaccination after June 2021
        if(doCounterFact){  
          fda.vacc.t$n.v1 = 0;
          fda.vacc.t$n.v2 = 0; 
        }
        
        
        proj = doProjection(state0 = state.fcast0, epi.model = epi.model, tmstep=7, 
                            newI.previous = newI.previous.t, # prior case/death numbers
                            weeks.fcast = weeks.fcast, # week of the year to get seasonality 
                            fcast.wk.starts = fcast.wk.starts,
                            fcast.mob =fcast.mob.t,  # projected mobility/npi
                            fda.vacc = fda.vacc.t, # projected vaccination rate
                            seasonality=seasonality,
                            relR0=relR0,
                            seed = seed.t,
                            cum.dS.po = cum.dS.po.t, 
                            ts.ImmLoss = ts.ImmLoss.t,
                            VE1priorInf = VE1priorInf.t,
                            VE2priorInf = VE2priorInf.t, 
                            p.alpha.proj = p.alpha.proj, 
                            p.ifr.proj = p.ifr.proj, # for adjusting alpha and ifr if necessary
                            adj.tx = F, # do not adjust for changes in transmission rate
                            p.beta.proj = p.beta.proj, # for beta, suppose reinfection and vax are less infectious
                            p.Tir.proj = p.Tir.proj
        )
        
        
        save(proj, file = paste0(dir_res, loc.t,'_proj_',ifelse(doCounterFact,'novx',''),sce.t,'_r',ir,'.RData'))
        
      }
      
      
    }
    
    
    if(ir == 1)
      save(SRparms, file = paste0(dir_res, 'SRparms.RData'))
  }
  
} # loc


