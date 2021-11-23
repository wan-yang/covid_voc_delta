# do projection, assuming the same transmission rate etc.

doProjection = function(state0, epi.model, tmstep=7, 
                        newI.previous, # prior case/death numbers
                       weeks.fcast, # week of the year to get seasonality 
                       fcast.wk.starts = NULL,
                       fcast.mob =fcast.mob,  # projected mobility/npi
                       fda.vacc = fda.vacc, # projected vaccination rate
                       seasonality=seasonality,relR0=relR0,
                       seed = seed,
                       cum.dS.po = cum.dS.po.t,
                       ts.ImmLoss = ts.ImmLoss.t,
                       VE1priorInf = VE1priorInf.t,
                       VE2priorInf = VE2priorInf.t,
                       p.alpha.proj, p.ifr.proj,  # for adjusting alpha and ifr if necessary
                       adj.tx = F, 
                       p.beta.proj = p.beta.proj, # for beta, suppose reinfection and vax are less infectious
                       p.Tir.proj = p.Tir.proj
                       ){
  
  # print(paste0('VE for those w prior inf: VE1=', VE1priorInf, '; VE2=', VE2priorInf))
  
  {
    dist_tm.to.detect = NULL
    for(ii in 1:num_ens){
      tmp = generation.time(dist_tm.to.detect.name,c(state0['Td.mean',ii],state0['Td.sd',ii]),truncate = tm.to.detect.max)
      dist_tm.to.detect=cbind(dist_tm.to.detect,tmp$GT[-1]); 
    }
    
    
    dist_tm.to.death = NULL  # time to death
    for(ii in 1:num_ens){
      # tmp = generation.time(dist_tm.to.death.name,c(state0['Td.mean',ii]+diff.dd,state0['Td.sd',ii]+diff.sd2),truncate = tm.to.death.max)
      # do not link it to Td
      tmp = generation.time(dist_tm.to.death.name,c(tm.to.outcome.mn['tm.to.death',ii], tm.to.outcome.sd['tm.to.death',ii]),truncate = tm.to.death.max)
      dist_tm.to.death=cbind(dist_tm.to.death,tmp$GT[-1]); 
    }
    if(tm.to.deathFromDiag){ # if the distribution of time to death is from diagnosis, not infectious
      # add time from infectious to diagnosis
      dist_tm.to.death = rbind(matrix(0,round(tm.to.diag,0),num_ens),dist_tm.to.death)
    }
  }
  
  # integrate forward 1 step to get the prior
  num_wk.fast = length(weeks.fcast)
  fcast.inf=matrix(0,num_wk.fast,num_ens)
  fcast.case=matrix(0,num_wk.fast,num_ens)
  fcast.death=matrix(0,num_wk.fast,num_ens)
  seed0 = seed
  newI.previous00 = newI.previous
  state00 = state0
  
  # compute lost of immunity due to waning
  tmp = fn_getImmLoss(N, S.t=mean(state0['S1',]), E.t=mean(state0['E1',]), I.t=mean(state0['I1',]), tmstep=tmstep, Trs.t=mean(state0['Trs',]), 
                      ts.ImmLoss.t=ts.ImmLoss)
  ts.ImmLoss = tmp$ts.ImmLoss
  cum.ImmLoss = tmp$cum.ImmLoss

  for(iwk in 1:num_wk.fast){
    cur.wk = weeks.fcast[iwk]
    tm_strt = 1; tm_end = tmstep;
    vdate.t = fcast.wk.starts[iwk];
    
    if(!is.null(newI.previous) & vdate.t >= vax.start){
      tm.t = nrow(newI.previous)
      # cumI.t = apply(newI.previous00[,,1:(dim.t[3]-14),drop=F],c(1,2),sum) #  %>% apply(1, median) # excl last two weeks and get the median
      # t1 = (as.Date('2020/12/14') - as.Date('2020/3/1')) %>% as.numeric() # 1st day of vaccination
      # 2/5/21 set t1 to 1 yr given the slow rollout
      t1 = 365
      cumI.t = colSums(newI.previous[pmax(1,tm.t-t1) : (tm.t),]) #  %>% apply(1, median) # excl last two weeks and get the median
      # only count the last 12 months? so as the epidemic unfold, you don't over count cum infect?
      # higher infection rate for the priority group
      # tm.t = pmax(1, tm.t - t1 + 1) # re-aline timing with start of vac
      tm.imm = 365* 2.5 # assume 3 yr immunity
      p.imm.wane.max = .8; k = .015  # 1/tm.imm  
      # p.imm.wane = 1 - p.imm.wane.max / (1+exp(-k*(tm.t + 60 - tm.imm/2))) # not all infected from day 1
      # since only the last year is included, should be:
      p.imm.wane = 1 - p.imm.wane.max / (1+exp(-k*(pmin(t1, tm.t) + 60 - tm.imm/2))) # not all infected from day 1
      # earlier infections are likely to be in the high priority groups 
      p.imm = 1 *  p.imm.wane * redn.priority # assume 100% prior infection provide immunity, but wane over time
      # and multiple by % excluded if there is prior testing before vax: p.prior.test
      percSmax.t = 1 - cumI.t / N * p.imm
      # also compare to current susceptibility, in case of immune evasion that increases susceptiblity
      percSmax.t = pmax(percSmax.t, state0[paste0('S',1:num_gr),] / N)
      # no lower than 50%, in case of outliers
      # percSmax.t = pmax(percSmax.t, .5)
      # print(c('cohort %S:',round(summary(mean(percSmax.t)),2)), quote = F)
    } else {
      percSmax.t = 0
      # print('no vax yet')
    }
    
    beta_tt = state0['beta',] * fn_getRelMob(fcast.mob[iwk,],state0['p.mob',]) 
    if(seasonality) {
      # beta_tt = state0['beta',] * seasonal.cycle[seasonal.cycle$week==cur.wk,]$relR0 # * rel.mob[tt]
      beta_tt = beta_tt * relR0[cur.wk,] # * rel.mob[tt]
    } 
    
    # adjust beta? suppose reinfection and vaccinated are less infectious
    if(adj.tx){
      beta_tt = beta_tt *  p.beta.proj[iwk]
      state0['Tir', ] = state00['Tir', ] * p.Tir.proj[iwk]
    }
    
    # adjust alpha and ifr if needed
    state0['alpha', ] = state00['alpha', ] * p.alpha.proj[iwk]
    state0['ifr', ] = state00['ifr', ] * p.ifr.proj[iwk]
    severity.t['death', ]  = state00['ifr', ] * p.ifr.proj[iwk]
    
    if(epi.model == 'SEIRS'){
      
      simEpi=SEIRS(tm_strt, tm_end, tm_step=1, # 1 day time-step
                   tmstep = tmstep,
                   state0 = state0,
                   S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                   I0=state0[paste0('I',1:num_gr),], 
                   beta=beta_tt, 
                   Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                   seed=seed, stoch=stoch,
                   severity = severity.t,
                   newI.previous = newI.previous,
                   dist_tm.to.detect = dist_tm.to.detect,
                   dist_tm.to.death = dist_tm.to.death)
      
    } else if(epi.model == 'SEIRSV'){
      daVacc.t = fda.vacc[as.Date(date) >= as.Date(vdate.t) & as.Date(date) < as.Date(vdate.t)+tm_end-tm_strt+1] # vaccination data
      
      if(nrow(daVacc.t)<1){  # no data yet
        V1.t = V2.t = matrix(0, tm_end - tm_strt + 1, num_ens)
      } else { # yes data
        
        daVacc.t$date = daVacc.t$date %>% as.Date
        
        # make sure it includes a full week
        dates.t = data.table(date = seq(as.Date(vdate.t), length.out = tm_end-tm_strt+1, by='day'))
        daVacc.t = merge(daVacc.t, dates.t, all = T, by = 'date')
        daVacc.t[is.na(daVacc.t)] = 0
        V1.t = as.matrix(daVacc.t$n.v1, tmstep, num_ens)
        V2.t = as.matrix(daVacc.t$n.v2, tmstep, num_ens)
        
        # print('start vacc!')
        
      }
      simEpi=SEIRSV(tm_strt, tm_end, tm_step=1, # 1 day time-step
                    tmstep = tmstep,
                    state0 = state0,
                    S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                    I0=state0[paste0('I',1:num_gr),], 
                    beta=beta_tt, 
                    Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                    seed=seed, stoch=stoch, 
                    severity = severity.t,
                    newI.previous = newI.previous,
                    dist_tm.to.detect = dist_tm.to.detect,
                    dist_tm.to.death = dist_tm.to.death,
                    percSmax.t = percSmax.t,
                    V1 = V1.t, V2 = V2.t, # add vaccination for dose 1 and dose 2 -
                    # these are total number of vaccinees with unknown immunity
                    # but pre-ajust for time lag from vaccination to immune protection
                    VE1 = VE1, VE2=VE2, # Vaccine efficacy, need further adjustment by prior immunity 
                    diffVEforPriorInf = T, # whether to consider possible high ve after 1 dose for those with prior infection
                    VE1priorInf = VE1priorInf, VE2priorInf = VE2priorInf, # Vaccine efficacy, for those with prior infection
                    cum.dS.po = cum.dS.po, # number of people had prior infection but lost their immunity due to the immune evasive variant
                    cum.ImmLoss = cum.ImmLoss # number of people had prior infection but lost their immunity due to waning
       ) # include the delay reporting etc.
    }
    
    # re-assemble to the same order as the prior: state0
    n.end = tm_end - tm_strt + 2
    state.new = NULL
    for(i in 1:(length(simEpi)-1)){
      tmp = simEpi[[i]][n.end,,drop=F]; 
      rownames(tmp)=gsub('cumI','newI',paste0(names(simEpi)[i],1:num_gr))
      state.new = rbind(state.new,tmp)
    }
    
    state.new = rbind(state.new, state0[parm.names,])
    state.new = state.new[rownames(state0),] # make sure the order is the same
    
    state0 = state.new # update
    newi = simEpi$cumItot
    newi = newi[-1,] - newi[-nrow(newi),]
    newI.previous = rbind(newI.previous,newi) # include the most recent week
    fcast.inf[iwk,] = state.new[idx.newItot,]
    fcast.case[iwk,] = state.new[idx.obs_i,]
    fcast.death[iwk,] = state.new[idx.obs_d,]
    
    # compute lost of immunity due to waning
    tmp = fn_getImmLoss(N, S.t=mean(state0['S1',]), E.t=mean(state0['E1',]), I.t=mean(state0['I1',]), tmstep=tmstep, Trs.t=mean(state0['Trs',]), 
                        ts.ImmLoss.t=ts.ImmLoss)
    ts.ImmLoss = tmp$ts.ImmLoss
    cum.ImmLoss = tmp$cum.ImmLoss
    
  } # end week
  
  # get summary stats
  tmp.inf = fcast.inf %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.inf) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.case = fcast.case %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.case) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.death = fcast.death %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.death) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  fcast_stats = rbind(data.table(measure = 'Infections', Week.start=fcast.wk.starts,tmp.inf),
                      data.table(measure = 'Cases', Week.start=fcast.wk.starts,tmp.case),
                      data.table(measure = 'Deaths', Week.start=fcast.wk.starts,tmp.death)
  )
  
  # cumulative
  # get summary stats
  tmp.inf = fcast.inf %>% apply(2,cumsum) %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.inf) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.case = fcast.case %>% apply(2,cumsum) %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.case) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.death = fcast.death %>% apply(2,cumsum) %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.death) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  fcast_stats = rbind(fcast_stats, data.table(measure = 'Cumulative Infections', Week.start=fcast.wk.starts,tmp.inf),
                      data.table(measure = 'Cumulative Cases', Week.start=fcast.wk.starts,tmp.case),
                      data.table(measure = 'Cumulative Deaths', Week.start=fcast.wk.starts,tmp.death)
  )
  
  return(fcast_stats = fcast_stats)
}



doProjection_v0 = function(state0, epi.model, tmstep=7, 
                        newI.previous, # prior case/death numbers
                        weeks.fcast, # week of the year to get seasonality 
                        fcast.wk.starts = NULL,
                        fcast.mob =fcast.mob,  # projected mobility/npi
                        fda.vacc = fda.vacc, # projected vaccination rate
                        seasonality=seasonality,relR0=relR0,
                        seed = seed
){
  
  
  {
    dist_tm.to.detect = NULL
    for(ii in 1:num_ens){
      tmp = generation.time(dist_tm.to.detect.name,c(state0['Td.mean',ii],state0['Td.sd',ii]),truncate = tm.to.detect.max)
      dist_tm.to.detect=cbind(dist_tm.to.detect,tmp$GT[-1]); 
    }
    
    
    dist_tm.to.death = NULL  # time to death
    for(ii in 1:num_ens){
      # tmp = generation.time(dist_tm.to.death.name,c(state0['Td.mean',ii]+diff.dd,state0['Td.sd',ii]+diff.sd2),truncate = tm.to.death.max)
      # do not link it to Td
      tmp = generation.time(dist_tm.to.death.name,c(tm.to.outcome.mn['tm.to.death',ii], tm.to.outcome.sd['tm.to.death',ii]),truncate = tm.to.death.max)
      dist_tm.to.death=cbind(dist_tm.to.death,tmp$GT[-1]); 
    }
    if(tm.to.deathFromDiag){ # if the distribution of time to death is from diagnosis, not infectious
      # add time from infectious to diagnosis
      dist_tm.to.death = rbind(matrix(0,round(tm.to.diag,0),num_ens),dist_tm.to.death)
    }
  }
  
  # integrate forward 1 step to get the prior
  num_wk.fast = length(weeks.fcast)
  fcast.inf=matrix(0,num_wk.fast,num_ens)
  fcast.case=matrix(0,num_wk.fast,num_ens)
  fcast.death=matrix(0,num_wk.fast,num_ens)
  seed0 = seed
  newI.previous00 = newI.previous
  
  for(iwk in 1:num_wk.fast){
    cur.wk = weeks.fcast[iwk]
    tm_strt = 1; tm_end = tmstep;
    vdate.t = fcast.wk.starts[iwk];
    
    if(!is.null(newI.previous) & vdate.t >= vax.start){
      tm.t = nrow(newI.previous)
      # cumI.t = apply(newI.previous00[,,1:(dim.t[3]-14),drop=F],c(1,2),sum) #  %>% apply(1, median) # excl last two weeks and get the median
      # t1 = (as.Date('2020/12/14') - as.Date('2020/3/1')) %>% as.numeric() # 1st day of vaccination
      # 2/5/21 set t1 to 1 yr given the slow rollout
      t1 = 365
      cumI.t = colSums(newI.previous[pmax(1,tm.t-t1) : (tm.t),]) #  %>% apply(1, median) # excl last two weeks and get the median
      # only count the last 12 months? so as the epidemic unfold, you don't over count cum infect?
      # higher infection rate for the priority group
      # tm.t = pmax(1, tm.t - t1 + 1) # re-aline timing with start of vac
      tm.imm = 365* 2.5 # assume 3 yr immunity
      p.imm.wane.max = .8; k = .015  # 1/tm.imm  
      p.imm.wane = 1 - p.imm.wane.max / (1+exp(-k*(tm.t + 60 - tm.imm/2))) # not all infected from day 1
      # earlier infections are likely to be in the high priority groups 
      p.imm = 1 *  p.imm.wane * redn.priority # assume 100% prior infection provide immunity, but wane over time
      # and multiple by % excluded if there is prior testing before vax: p.prior.test
      percSmax.t = 1 - cumI.t / N * p.imm
      # also compare to current susceptibility, in case of immune evasion that increases susceptiblity
      percSmax.t = pmax(percSmax.t, state0[paste0('S',1:num_gr),] / N)
      # no lower than 50%, in case of outliers
      percSmax.t = pmax(percSmax.t, .5)
      # print(c('cohort %S:',round(summary(mean(percSmax.t)),2)), quote = F)
    } else {
      percSmax.t = 0
      # print('no vax yet')
    }
    
    beta_tt = state0['beta',] * fn_getRelMob(fcast.mob[iwk,],state0['p.mob',]) 
    if(seasonality) {
      # beta_tt = state0['beta',] * seasonal.cycle[seasonal.cycle$week==cur.wk,]$relR0 # * rel.mob[tt]
      beta_tt = beta_tt * relR0[cur.wk,] # * rel.mob[tt]
    } 
    
    
    
    if(epi.model == 'SEIRS'){
      
      simEpi=SEIRS(tm_strt, tm_end, tm_step=1, # 1 day time-step
                   tmstep = tmstep,
                   state0 = state0,
                   S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                   I0=state0[paste0('I',1:num_gr),], 
                   beta=beta_tt, 
                   Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                   seed=seed, stoch=stoch,
                   severity = severity.t,
                   newI.previous = newI.previous,
                   dist_tm.to.detect = dist_tm.to.detect,
                   dist_tm.to.death = dist_tm.to.death)
      
    } else if(epi.model == 'SEIRSV'){
      daVacc.t = fda.vacc[as.Date(date) >= as.Date(vdate.t) & as.Date(date) < as.Date(vdate.t)+tm_end-tm_strt+1] # vaccination data
      
      if(nrow(daVacc.t)<1){  # no data yet
        V1.t = V2.t = matrix(0, tm_end - tm_strt + 1, num_ens)
      } else { # yes data
        
        daVacc.t$date = daVacc.t$date %>% as.Date
        
        # make sure it includes a full week
        dates.t = data.table(date = seq(as.Date(vdate.t), length.out = tm_end-tm_strt+1, by='day'))
        daVacc.t = merge(daVacc.t, dates.t, all = T, by = 'date')
        daVacc.t[is.na(daVacc.t)] = 0
        V1.t = as.matrix(daVacc.t$n.v1, tmstep, num_ens)
        V2.t = as.matrix(daVacc.t$n.v2, tmstep, num_ens)
        
        # print('start vacc!')
        
      }
      simEpi=SEIRSV(tm_strt, tm_end, tm_step=1, # 1 day time-step
                    tmstep = tmstep,
                    state0 = state0,
                    S0=state0[paste0('S',1:num_gr),], E0=state0[paste0('E',1:num_gr),], 
                    I0=state0[paste0('I',1:num_gr),], 
                    beta=beta_tt, 
                    Tei=state0['Tei',], Tir=state0['Tir',], Trs = state0['Trs',],
                    seed=seed, stoch=stoch, 
                    severity = severity.t,
                    newI.previous = newI.previous,
                    dist_tm.to.detect = dist_tm.to.detect,
                    dist_tm.to.death = dist_tm.to.death,
                    percSmax.t = percSmax.t,
                    V1 = V1.t, V2 = V2.t, # add vaccination for dose 1 and dose 2 -
                    # these are total number of vaccinees with unknown immunity
                    # but pre-ajust for time lag from vaccination to immune protection
                    VE1 = VE1, VE2=VE2 # Vaccine efficacy, need further adjustment by prior immunity 
      ) # include the delay reporting etc.
    }
    
    # re-assemble to the same order as the prior: state0
    n.end = tm_end - tm_strt + 2
    state.new = NULL
    for(i in 1:(length(simEpi)-1)){
      tmp = simEpi[[i]][n.end,,drop=F]; 
      rownames(tmp)=gsub('cumI','newI',paste0(names(simEpi)[i],1:num_gr))
      state.new = rbind(state.new,tmp)
    }
    
    state.new = rbind(state.new, state0[parm.names,])
    state.new = state.new[rownames(state0),] # make sure the order is the same
    
    state0 = state.new # update
    newi = simEpi$cumItot
    newi = newi[-1,] - newi[-nrow(newi),]
    newI.previous = rbind(newI.previous,newi) # include the most recent week
    fcast.inf[iwk,] = state.new[idx.newItot,]
    fcast.case[iwk,] = state.new[idx.obs_i,]
    fcast.death[iwk,] = state.new[idx.obs_d,]
  } # end week
  
  # get summary stats
  tmp.inf = fcast.inf %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.inf) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.case = fcast.case %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.case) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.death = fcast.death %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.death) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  
  fcast_stats = rbind(data.table(measure = 'Infections', Week.start=fcast.wk.starts,tmp.inf),
                      data.table(measure = 'Cases', Week.start=fcast.wk.starts,tmp.case),
                      data.table(measure = 'Deaths', Week.start=fcast.wk.starts,tmp.death)
  )
  
  # cumulative
  # get summary stats
  tmp.inf = fcast.inf %>% apply(2,cumsum) %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.inf) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.case = fcast.case %>% apply(2,cumsum) %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.case) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  tmp.death = fcast.death %>% apply(2,cumsum) %>% apply(1, quantile, prob = c(.5, .25, .75, .025, .975)) %>% t
  colnames(tmp.death) = c('median', 'iqr.lwr','iqr.upr','ci95.lwr','ci95.upr')
  fcast_stats = rbind(fcast_stats, data.table(measure = 'Cumulative Infections', Week.start=fcast.wk.starts,tmp.inf),
                      data.table(measure = 'Cumulative Cases', Week.start=fcast.wk.starts,tmp.case),
                      data.table(measure = 'Cumulative Deaths', Week.start=fcast.wk.starts,tmp.death)
  )
  
  return(fcast_stats = fcast_stats)
}
