# to plot and compare the overall trends in the three countries
# for the MS
# 10/27/21 update 

# first set working directory to source file location

date.tag = '_2021-10-28'

date.stop = as.Date('2021/7/1');  

library(data.table)
library(magrittr)
library(ggplot2)
library(xlsx)
library(MMWRweek)
library(lemon)
library('readr')
library('readxl')
library('writexl')
library('stringi')
# library(tidyverse)
library(tgp)
library(gridExtra)
library(classInt)
library(RColorBrewer)

N = 1e6; # population size is set to 1M

eval.t = 'obs.more'
dir_data = '../data/'
dir_code =  './' 

dir_res = paste0('../results/')
dir_plot = paste0('../results/')

if(!file.exists(dir_plot)) dir.create(dir_plot)

source(paste0(dir_code,'getPlot.R'))
source(paste0(dir_code,'get_relR0.R'))
source(paste0(dir_code,'Fn_util.R'))

Rwea_parm.bounds = rbind(
  c(2.34, 2.93), # R0max
  c(.86,1.18), # R0diff - this determines the magnitude of seasonality
  # reduce the leve of seasonality, b/c cp flu corona virus seasonality is less profound
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

num_ens = 2
da = read.csv(paste0(dir_data,'da_case.death.mob_uk.sa.br.in',date.tag,'.csv'), stringsAsFactors = F)  %>% data.table()
da$date = da$date %>% as.Date
da[date < as.Date('2020-02-09') & is.na(da)] = 0
da = da[date >= as.Date('2020-03-01')]
da = da[complete.cases(da)]
da.full = da
da = da[date < as.Date(date.stop)]
loc.t = 'in'
cnty = c('(A) United Kingdom', "(B) South Africa", '(C) Brazil','(D) India')

useRecentWeaData = T # use more recent weather data for seasonal trend
da.wea.recent = read.csv(paste0(dir_data, 'wea.wk_india_Jan2020_Nov2021.csv'))



########################################################################################################################
# EVALUATION BASED ON ACCURACY OF RETROSPECTIVE PROJECTION
# first need to evaluate which scenario performs best
{
  fn_rrmse = function(obs.t, est.t){
    sqrt(mean((obs.t - est.t)^2)) / mean(obs.t)
  }
  
  fn_corr = function(obs.t, est.t){
    cor(obs.t, est.t)
  }
  
  fn_acc = function(obs.t, est.t.lwr, est.t.upr){
    if(obs.t >= est.t.lwr & obs.t <= est.t.upr){
      acc = 1  # if within the upper and lower bound, deemed accurate
    } else {
      acc = 0
    } 
    acc
  }
}
{
  theme.t = theme(plot.title = element_text(v=0, size = 10, margin=margin(0,0,3,0)), 
                  strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                  axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                  axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                  axis.text.x = element_text(size=8,angle = 0),
                  plot.margin=unit(c(c(.3, 1, .1, .5)), units="line"), # top, right, bottom, left
                  legend.title = element_text(size=8), legend.text=element_text(size=8),
                  legend.margin=margin(0,0,0,0),
                  legend.box.margin=margin(-10,-10,-10,-10),
                  legend.key.size = unit(.2, 'cm'), #change legend key size
                  legend.key.height = unit(.2, 'cm'), #change legend key height
                  legend.key.width = unit(.2, 'cm')) #change legend key width)
  
  theme.t2 = theme(plot.title = element_text(v=0, size = 10, margin=margin(2,0,3,0)), 
                   strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                   axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                   axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                   axis.text.x = element_text(size=8,angle = 45, hjust = .9),
                   plot.margin=unit(c(c(.3, 1, .1, .5)), units="line"), # top, right, bottom, left
                   legend.title = element_text(size=8), legend.text=element_text(size=8),
                   legend.margin=margin(0,0,0,0),
                   legend.box.margin=margin(-10,-10,-10,-10),
                   legend.key.size = unit(.2, 'cm'), #change legend key size
                   legend.key.height = unit(.2, 'cm'), #change legend key height
                   legend.key.width = unit(.2, 'cm')) #change legend key width)
  theme.t3 = theme(plot.title = element_text(v=0, size = 10, margin=margin(2,0,3,0)), 
                   strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0)),
                   axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                   axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                   axis.text.x = element_text(size=8,angle = 30, hjust = .9),
                   plot.margin=unit(c(c(.3, 1, -.7, .5)), units="line"), # top, right, bottom, left
                   legend.title = element_text(size=8), legend.text=element_text(size=8),
                   legend.margin=margin(0,0,0,0),
                   legend.box.margin=margin(-10,-10,-10,-10),
                   legend.key.size = unit(.2, 'cm'), #change legend key size
                   legend.key.height = unit(.2, 'cm'), #change legend key height
                   legend.key.width = unit(.2, 'cm')) #change legend key width)
  
  theme.t4 = theme(plot.title = element_text(v=0, size = 10, margin=margin(2,0,3,0)), 
                   strip.placement = "outside", strip.text = element_text(size = 11, margin=margin(1.5,0,1.5,0)),
                   axis.title = element_text(size =11, margin=margin(0,0.2,0,0)), 
                   axis.text.y = element_text(size=10, margin=margin(0,0.2,0,0)), 
                   axis.text.x = element_text(size=10,angle = 30, hjust = .9),
                   plot.margin=unit(c(c(.3, 1, .5, .5)), units="line"), # top, right, bottom, left
                   legend.title = element_text(size=8), legend.text=element_text(size=8),
                   legend.margin=margin(0,0,0,0),
                   legend.box.margin=margin(-10,-10,-10,-10),
                   legend.key.size = unit(.2, 'cm'), #change legend key size
                   legend.key.height = unit(.2, 'cm'), #change legend key height
                   legend.key.width = unit(.2, 'cm')) #change legend key width)
  
  getPlotProj = function(tda, obs, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
    
    dates.t = unique(tda$Week.start) %>% as.Date
    
    p = ggplot(tda)+
      geom_line(aes(x = Week.start, y = median, color = vx), size = 1) +  # no ctrl
      geom_point(data = obs, aes(x = Week.start, y = observed), size = 1) + 
      geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
      facet_rep_wrap(~ vx, # scales = 'free_y', 
                     repeat.tick.labels = T, ncol = 3) + 
      ggtitle(title.t) + labs(x = '', y = paste(y.lab, ''), color = 'Vaccination\n rate', fill = 'Vaccination\n rate') +
      scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 4)],
                   labels = format(dates.t[seq(1, length(dates.t), by = 4)],'%m/%d/%y')) +
      theme_minimal() + theme.t3
    
    p
  }
  
  getPlotTotals = function(tda, title.t, y.lab = 'Projections, per 1 M popuplation (median)', ncol.t = 9, col.set, theme.tt = theme.t2){
    p = ggplot(tda, aes(fill=variant, y=v.median, x=sce.npi)) + 
      geom_bar(position="stack", stat="identity", alpha = .6) +
      ggtitle(title.t) + labs(x = '', y = y.lab) +
      facet_wrap(~seeding + sce.ve, ncol = ncol.t) + 
      # facet_wrap(~seed.ve, ncol = ncol.t) + 
      # scale_fill_brewer(palette = col.set) +
      theme_minimal() + theme.tt
    
    p
  }
  
  getPlotProj1 = function(tda, obs, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
    
    dates.t = unique(tda$Week.start) %>% as.Date
    
    p = ggplot(tda)+
      geom_line(aes(x = Week.start, y = median, color = vx), size = 1, alpha = .7) +  # no ctrl
      geom_point(data = obs, aes(x = Week.start, y = observed, shape = 'Observed'), size = 1.5) + 
      geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
      ggtitle(title.t) + labs(x = 'Week start', y = paste(y.lab, '')) + 
      guides(shape =guide_legend(title = '', order =1), color = guide_legend(title = ''), fill = guide_legend(title = '')) + 
      scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 4)],
                   labels = format(dates.t[seq(1, length(dates.t), by = 4)],'%m/%d/%y')) +
      theme_minimal() + theme.t
    
    p
  }
  
  getPlotProjMedian = function(tda, obs, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
    
    dates.t = unique(tda$Week.start) %>% as.Date
    
    p = ggplot(tda)+
      geom_line(aes(x = Week.start, y = median, color = vx), size = .5, alpha = .7) +  # no ctrl
      geom_point(data = obs, aes(x = Week.start, y = observed, shape = 'Observed'), size = 1.5) + 
      # geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
      ggtitle(title.t) + labs(x = 'Week start', y = paste(y.lab, '')) + 
      guides(shape =guide_legend(title = '', order =1), color = guide_legend(title = 'VE1/VE2'), 
             fill = guide_legend(title = '')) + 
      scale_x_date(breaks = dates.t[seq(1, length(dates.t), by = 4)],
                   labels = format(dates.t[seq(1, length(dates.t), by = 4)],'%m/%d/%y')) +
      ylim(0, max(c(tda$median, obs$observed)) * 1.1) + 
      theme_minimal() + theme.t
    
    p
  }
  
}

load(paste0(dir_res, "res.proj",date.tag,".RData"))
scenarios = res.proj$scenario %>% unique %>% sort
da.t = da.full[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
  setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))
# only when it is > 10 cases
# da.t = da.t[case > 1]
da.t = da.t[case > 2 | death > 0]
da.t$date = da.t$date %>% as.Date
acc.proj = NULL
for(isce in 1:length(scenarios)){ # 1: length(senarios)
  
  
  # compute the rrmse for case, hosp, death, based on the median
  est0 = res.proj[measure %in% c('Cases', 'Deaths') & scenario == scenarios[isce]]
  est = dcast(est0[variable=='median'], Week.start ~ measure, value.var = 'value') %>% 
    setnames(c('Week.start','Cases', 'Deaths'),c('date','case', 'death'))
  est.lwr = dcast(est0[variable=='iqr.lwr'], Week.start ~ measure, value.var = 'value') %>% 
    setnames(c('Week.start','Cases', 'Deaths'),c('date','case', 'death'))
  est.upr = dcast(est0[variable=='iqr.upr'], Week.start ~ measure, value.var = 'value') %>% 
    setnames(c('Week.start','Cases', 'Deaths'),c('date','case', 'death'))
  
  est = merge(est, da.t, by = 'date', suffixes = c('.est','.obs'))
  est = merge(est.lwr, est, by = 'date')
  est = merge(est.upr, est, by = 'date', suffixes = c('.upr','.lwr'))
  
  
  rrmse.case = fn_rrmse(obs.t = est$case.obs, est.t = est$case.est)
  rrmse.death = fn_rrmse(obs.t = est$death.obs, est.t = est$death.est)
  
  corr.case = fn_corr(obs.t = est$case.obs, est.t = est$case.est)
  corr.death = fn_corr(obs.t = est$death.obs, est.t = est$death.est)
  
  # test the accuracy of the projection
  # if within the upper and lower bounds, deemed accurate
  acc.case = est[,fn_acc(obs.t = case.obs, est.t.lwr = case.lwr, est.t.upr = case.upr), by = 'date']
  acc.case = sum(acc.case$V1) / length(acc.case$V1)
  acc.death = est[,fn_acc(obs.t = death.obs, est.t.lwr = death.lwr, est.t.upr = death.upr), by = 'date']
  acc.death = sum(acc.death$V1) / length(acc.death$V1)
  
  
  acc.proj = rbind(acc.proj, 
              data.table(scenario = scenarios[isce],
                          rrmse.case = rrmse.case,
                         rrmse.death = rrmse.death,
                         corr.case = corr.case,
                         corr.death = corr.death,
                         acc.case = acc.case,
                         acc.death = acc.death
                         )
  )
  
}

acc.proj$rrmse.cb = acc.proj[,c('rrmse.case', 'rrmse.death'),with=F] %>% apply(1, mean) # take the mean of case and death
acc.proj$corr.cb = acc.proj[,c('corr.case', 'corr.death'),with=F] %>% apply(1, mean) # take the mean of case and death

acc.proj = acc.proj[order(rrmse.case, rrmse.death, -corr.case, -corr.death, -acc.case, -acc.death)]
# acc.proj = acc.proj[order(rrmse.cb, -corr.cb)]

best.model = acc.proj[1]
best.sce = best.model$scenario
sce.t = best.sce
ve.t = sce.t %>% stri_split(regex = 'VEpriorInf|-', perl = T) %>% unlist %>% as.numeric() %>% .[!is.na(.)]
ve.t = ve.t * 100
sce.t.label = paste0('VE:', ve.t[1],'/',ve.t[2],'%')
sce.t
sce.t.label
write.csv(acc.proj, paste0(dir_res, 'tab_acc.proj.csv'), row.names = F)
########################################################################################################################

# plot projections
VE1 = .3
VE2 = .67
sce.base = 'VEpriorInf0.3-0.67'; sce.base.label = c('Baseline VE:\n30/67%')
VEpriorInf_vec = data.table(VE1priorInf = c(VE1, seq(.4, .60, by = .1), rep(VE1, 3), seq(.5, .9, by = .1)),
                            VE2priorInf = c(VE2, rep(VE2, 3),
                                            seq(.75, .95, by = .1),
                                            seq(.75, length.out = length(seq(.5, .9, by = .1)), by = .05) %>% pmin(.95))
)

scenarios = VEpriorInf_vec %>% apply(1, paste,collapse = '-') %>% paste0('VEpriorInf', .)
acc.proj = acc.proj[!grepl('novx',scenario)]  # exclude the counterfactual - no vaccination after June 2021
acc.proj$VE1 = factor(acc.proj$scenario, levels = scenarios, labels = VEpriorInf_vec$VE1priorInf) #  %>% as.character() %>% as.numeric()
acc.proj$VE2 = factor(acc.proj$scenario, levels = scenarios, labels = VEpriorInf_vec$VE2priorInf) # %>% as.numeric()
# acc.proj$VE1 = factor(acc.proj$VE1, levels = VEpriorInf_vec$VE1priorInf %>% sort)
# acc.proj$VE2 = factor(acc.proj$VE2, levels = VEpriorInf_vec$VE2priorInf %>% sort)
acc.proj$VE1 = acc.proj$VE1 %>% as.character() %>% as.numeric()
acc.proj$VE2 = acc.proj$VE2 %>% as.character() %>% as.numeric()
acc.proj$VE1 = acc.proj$VE1 * 100
acc.proj$VE2 = acc.proj$VE2 * 100
nVE = acc.proj$VE2 %>% unique %>% length()

tda = dcast(res.proj[scenario %in% c(sce.t, sce.base)], loc + scenario + measure + Week.start ~ variable)
tda$vx = factor(tda$scenario, levels = c(sce.base, sce.t), labels = c(sce.base.label, sce.t.label))
dates.t = unique(tda$Week.start) %>% as.Date %>% sort
obs.case = da.full[as.Date(date) %in% as.Date(tda$Week.start), c('date', paste0('case.',loc.t)),with=F]
colnames(obs.case) = c('Week.start', 'observed') 

obs.death = da.full[as.Date(date) %in% as.Date(tda$Week.start), c('date', paste0('death.',loc.t)),with=F]
colnames(obs.death) = c('Week.start', 'observed') 

scenarios = VEpriorInf_vec %>% apply(1, paste,collapse = '-') %>% paste0('VEpriorInf', .)
sce.labs = (VEpriorInf_vec * 100) %>% apply(1, paste,collapse = '/') %>% paste0('%')
tda2 = dcast(res.proj, loc + scenario + measure + Week.start ~ variable)
tda2$vx = factor(tda2$scenario, levels = scenarios, labels = sce.labs)



#getting colours
colourPalette <- brewer.pal(nVE,'RdPu')

pt.cols = rep('transparent', nrow(acc.proj))   # add a outer circle to the one using baseline
pt.cols[which(acc.proj$scenario==sce.base)] = 'black'

theme.t = theme(plot.title = element_text(v=0, size = 10, margin=margin(0,0,3,0)), 
                strip.placement = "outside", strip.text = element_text(size = 9, margin=margin(1.5,0,1.5,0), hjust = 0),
                axis.title = element_text(size =9, margin=margin(0,0.2,0,0)), 
                axis.text.y = element_text(size=8, margin=margin(0,0.2,0,0)), 
                axis.text.x = element_text(size=8,angle = 0),
                plot.margin=unit(c(c(.3, 1, .1, .5)), units="line"), # top, right, bottom, left
                legend.title = element_text(size=8), legend.text=element_text(size=8),
                legend.margin=margin(0,0,0,0),
                legend.box.margin=margin(-10,-10,-10,-10),
                legend.key.size = unit(.2, 'cm'), #change legend key size
                legend.key.height = unit(.2, 'cm'), #change legend key height
                legend.key.width = unit(.2, 'cm')) #change legend key width)

p1 = ggplot(acc.proj) + 
  geom_point(aes(x = rrmse.case, y = rrmse.death, size = VE1, col = VE2)) +
  geom_point(aes(x = rrmse.case, y = rrmse.death, size = VE1), shape = 1, colour = pt.cols, stroke = .25) + 
  # geom_point(aes(x = rrmse.case, y = rrmse.death, size = VE1, fill = VE2), colour = 'black', pch = 21) +
  scale_color_stepsn(colours = colourPalette, breaks = acc.proj$VE2 %>% unique %>% sort) +
  # xlim(min(acc.proj$rrmse.case) * .999,max(acc.proj$rrmse.case) * 1.001) +
  ylim(min(acc.proj$rrmse.death) * .999,max(acc.proj$rrmse.death) * 1.001) +
  # scale_color_distiller(palette = 'RdPu', direction = 1) +
  # scale_color_continuous(breaks = seq(60, 100, by = 10)) +
  scale_size_continuous(breaks = acc.proj$VE1 %>% unique %>% sort, limits = c(VE1*100, 300)) +
  ggtitle('(A) Relative Root Mean Square Error (RRMSE)') + labs(x = 'Cases', y = 'Deaths') +
  guides(size = guide_legend(title = 'VE1 (%)',order =1, 
                             override.aes = list(size = (acc.proj$VE1/100*3) %>% unique %>% sort), pch = 20), # 
         color = guide_legend(title = 'VE2 (%)')) + 
  theme_minimal() + theme.t

p2 = ggplot(acc.proj) + 
  geom_point(aes(x = corr.case, y = corr.death, size = VE1, col = VE2)) +
  geom_point(aes(x = corr.case, y = corr.death, size = VE1), shape = 1, colour = pt.cols, stroke = .25) + 
  # geom_point(aes(x = corr.case, y = corr.death, size = VE1, fill = VE2), colour = 'black', pch = 21) +
  scale_color_stepsn(colours = colourPalette, breaks = acc.proj$VE2 %>% unique %>% sort) +
  # xlim(min(acc.proj$corr.case) * .999,max(acc.proj$corr.case) * 1.001) +
  ylim(min(acc.proj$corr.death) * .999,max(acc.proj$corr.death) * 1.001) +
  # scale_color_distiller(palette = 'RdPu', direction = 1) +
  # scale_color_continuous(breaks = seq(60, 100, by = 10)) +
  scale_size_continuous(breaks = acc.proj$VE1 %>% unique %>% sort, limits = c(VE1*100, 300)) +
  ggtitle('(B) Correlation') + labs(x = 'Cases', y = 'Deaths') +
  guides(size = guide_legend(title = 'VE1 (%)',order =1, 
                             override.aes = list(size = (acc.proj$VE1/100*3) %>% unique %>% sort), pch = 20), # 
         color = guide_legend(title = 'VE2 (%)')) + 
  theme_minimal() + theme.t


p3 = getPlotProjMedian(tda = tda2[measure == 'Cases' & scenario %in% scenarios], obs = obs.case, 
                  title.t = "(C) Projected median: reported cases per week")

p4 = getPlotProjMedian(tda = tda2[measure == 'Deaths' & scenario %in% scenarios], obs = obs.death, 
                  title.t = "(D) Projected median: reported deaths per week")


pdf(paste0(dir_plot, 'Fig3.pdf'),width = 7.5, height = 5)
grid.arrange(
  grobs = list(p1, p2, p3, p4),
  layout_matrix = rbind(c(1, 3),
                        c(2, 4))
)
dev.off()


################################################################################################################
## SENSITIVITY ANALYSIS
# plot and compare the model estimates under different VEpriorInf

VE1 = .30; VE2 = .67
VEpriorInf_vec = data.table(VE1priorInf = c(VE1, seq(.4, .60, by = .1), rep(VE1, 3), seq(.5, .9, by = .1)),
                            VE2priorInf = c(VE2, rep(VE2, 3),
                                            seq(.75, .95, by = .1),
                                            seq(.75, length.out = length(seq(.5, .9, by = .1)), by = .05) %>% pmin(.95))
)
ve_vec = VEpriorInf_vec %>% apply(1, paste0, collapse = '-') %>% sort
ve.lab_vec = (VEpriorInf_vec * 100) %>% apply(1, paste0, collapse = '/') %>% paste0(., '%')  %>% sort

# load results
load(paste0(dir_res, "res.summary",date.tag,".RData"))

res = newVstat[eval == eval.t] %>% data.table()
res = melt(res, id.vars = c('loc', 'batch', 'VEpriorInf','run','hyp.test','eval')) 
res = res[variable %in% c('perc.dImm.mn','perc.dRtx.mean')]
res$state = factor(res$variable, levels = c('perc.dRtx.mean','perc.dImm.mn'), 
                   labels =  c('Transmissibility','Immune escape potential'))
res$VE = factor(res$VEpriorInf, levels = ve_vec, labels = ve.lab_vec)
res = res[order(VE, run, state)]
res %>% dplyr::filter(VEpriorInf == gsub('VEpriorInf','', best.sce) & eval==eval.t & variable == 'perc.dRtx.mean') %>% .$value %>% mean
res %>% dplyr::filter(VEpriorInf == gsub('VEpriorInf','', best.sce) & eval==eval.t & variable == 'perc.dRtx.mean') %>% .$value %>% quantile(prob = c(.025,.975))
res %>% dplyr::filter(VEpriorInf == gsub('VEpriorInf','', best.sce) & eval==eval.t & variable == 'perc.dImm.mn') %>% .$value %>% mean
res %>% dplyr::filter(VEpriorInf == gsub('VEpriorInf','', best.sce) & eval==eval.t & variable == 'perc.dImm.mn') %>% .$value %>% quantile(prob = c(.025,.975))

p = ggplot(res, aes(y=value, x = VE)) + # aes(x=VE, y=value, fill = as.factor(state))
  geom_boxplot(outlier.size= .5, alpha = .3, lwd = .3, color = 'grey30') + 
  facet_rep_wrap(~state) +
  labs(x='Scenarios: VE for those with prior widetype infections + 1 dose / 2 doses of vaccine', y='Relative change (%)') +  
  # scale_fill_brewer(palette="Paired") + 
  stat_summary(fun="mean", shape = 4, colour = 'red', size = .2, position=position_dodge(0.75))+  # 
  theme_minimal() + theme.t4

pdf(paste0(dir_plot, 'FigS2.pdf'), width = 7, height = 3)
print(p)
dev.off()

################################################################################################################



################################################################################################################
## PLOT MODEL-INFERENCE ESTIMATES FROM THE BEST PERFORMING MODEL SETTING

res.train = res.train[eval == eval.t & VEpriorInf == gsub('VEpriorInf', '', sce.t)]
tda = res.train[loc == 'in']
tda1 = tda[state == 'Infectious']
locs = c('in')


events = read_xlsx(paste0(dir_data, 'events.xlsx'), sheet = 1) %>% data.table()
events$start = events$start %>% as.Date(format = '%m/%d/%y')
events$end = events$end %>% as.Date(format = '%m/%d/%y')

vda.in = read_xlsx(paste0(dir_data, 'in_serosurveys.xlsx'), sheet = 1) %>% data.table()
vda.in$start = vda.in$start %>% as.Date(format = '%m/%d/%y')
vda.in$end = vda.in$end %>% as.Date(format = '%m/%d/%y')
tmp = vda.in$wt.prev %>% as.character() %>% strsplit('%') %>% unlist %>% matrix(nrow=nrow(vda.in), ncol=4, byrow = T)
tmp[,1] = tmp[,1] %>% trimws()
tmp[,2] = gsub('\\(','',tmp[,2]) %>% trimws()
tmp[,3] = gsub(',','',tmp[,3]) %>% trimws()
tmp = tmp[,1:3,drop=F]; mode(tmp) = 'numeric'
vda.in$mean = tmp[,1]; vda.in$ci95.lwr = tmp[,2]; vda.in$ci95.upr = tmp[,3];

# first wave only
est.in = res.train[loc == 'in' & Week.start <= as.Date('2021/1/31') & state == 'Cumulative infection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
est.in = dcast(est.in, date ~ stat, value.var = 'value')
dates.in.t = unique(est.in$date) %>% as.Date %>% sort
# asign the data to the closest week as the model
obs.in = data.table(date = dates.in.t, mean = -1, ci95.lwr = -1, ci95.upr = -1)
for(i in 1:nrow(obs.in)){
  # if it's with the inmpling period, assign that obs
  dt = obs.in[i]$date; # week start
  for(j in 1:nrow(vda.in)){
    # account for delay in antibody generation, and the model date is week start
    if(dt %in% seq(as.Date(vda.in[j]$start-14 -6), as.Date(vda.in[j]$end -14 - 6), by = 'day')){
      obs.in[i, c('mean','ci95.lwr','ci95.upr')] = vda.in[j, c('mean','ci95.lwr','ci95.upr'),with=F]
      obs.in[i]$mean = vda.in[j]$mean
    }
  }
}
obs.in[obs.in<0] = NA
# seperate the two datasets
obs.in1 = obs.in[date <= as.Date(vda.in$end[1])-14]
obs.in2 = obs.in[date > as.Date(vda.in$end[1])-14]

p.titles = c('(A) Model fit', '(B) Model validation', '(C) Seasonality, NPI v. pandemic dynamics',
             '(D) Rt v. infection rate', '(E) Transmissibility','(F) Susceptibility')
# put model fit and validation together
pdf(paste0(dir_plot, 'Fig1.pdf'), width = 8, height = 7)
par(mfrow=c(3,2),mar=c(1.7,2.5,1.5,2),oma=c(0.1,.1, 0.1,0),mgp=c(1.0,.1,0),cex=.8,cex.axis=.85,cex.lab=.9,tck=-.015)
cnt=0
for(loc.t in locs){
  cnt=cnt+1
  
  obs = da[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
    setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))
  
  obs$date = obs$date %>% as.Date
  # setnames(obs,'case', 'value')
  obs = obs[case > 2 | death > 0]
  
  # ymax.case = max(da[,paste0('case.',locs),with=F],na.rm = T) * 1.2
  # ymax.death = max(da[,paste0('death.',locs),with=F],na.rm = T) * 1.2
  
  dates.t = unique(obs$date)  %>% as.Date %>% sort
  x=seq(1, length.out = length(dates.t)*2, by = 1)
  
  mm = res.train[loc == loc.t & state == 'case'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  
  # include both case and death
  mm.case = res.train[loc == loc.t & state == 'case'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm.death = res.train[loc == loc.t & state == 'death'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  mm.death$value = mm.death$value * 10
  ymax.case = max(obs$case, mm.case$value, mm.death$value, na.rm = T)*1.05
  x=seq(1, length.out = length(dates.t)*2, by = 1)
  stats = matrix(0, 5, length(dates.t)*2)
  id2 = 0
  for(id in seq(1, length.out = length(dates.t), by = 2)){
    id2 = id2 + 1
    tmp1 = mm.case[date==dates.t[id2]]
    tmp2 = mm.death[date==dates.t[id2]]
    stats[,id] = c(tmp1[stat=='ci95.lwr']$value, tmp1[stat=='iqr.lwr']$value, tmp1[stat=='mean']$value, tmp1[stat=='iqr.upr']$value, tmp1[stat=='ci95.upr']$value)
    stats[,id+1] = c(tmp2[stat=='ci95.lwr']$value, tmp2[stat=='iqr.lwr']$value, tmp2[stat=='mean']$value, tmp2[stat=='iqr.upr']$value, tmp2[stat=='ci95.upr']$value)
  }
  # colnames(stats) = dates.t
  x=seq(1, length.out = length(dates.t)*2, by = 1)
  summarydata=list(stats=stats,n=rep(dates.t, e=2),names=rep('',length(dates.t)*2))
  bxp(summarydata, box.width = .1, lwd=.5, ylab='', xaxt='n', yaxt = 'n', at=x, ylim = c(0, ymax.case), 
      xlim=c(0.5,length(dates.t)*2+.5), border = c('blue','red'), fill='transparent') # 
  points(x[seq(1,length.out = length(dates.t),by=2)], obs$case, pch = 8, cex = .4, col='blue')
  points(x[seq(2,length.out = length(dates.t),by=2)], obs$death*10, pch = 8, cex = .4, col='red')
  axis(1,at=x[seq(1.5,length.out = length(dates.t),by=2)],labels = format(dates.t,'%m/%d/%y'),mgp=c(1.0,.1,0),cex.axis=.85)
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Cases per million', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  axis(4,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'red', col.lab = 'red', col.axis='red')
  mtext('Deaths per 100,000', side=4, outer = F, line = .9, cex=.75, col = 'red')
  
  
  # add time lines
  ymax = ymax.case
  events.t = events[loc == loc.t]
  events.t[is.na(end)]$end = max(dates.t)
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .02; 
  e.t = events.t[type == 'npi']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))*2]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))*2];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
      # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
      text(d.t3,ymax-y.offset, tx.t, adj = .5, cex=.75,srt=0, font = 1); # pos=4,
    }
  }
  e.t = events.t[type == 'virus']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))*2]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))*2];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
      if(loc.t %in% c('uk', 'in')){
        arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
        text(d.t1+offset*4,ymax*(z-z.width*4), tx.t,  pos=4, cex=.75,srt=0, font = 1); # pos=4,
      } else {
        arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
        text(d.t1+offset*2,ymax*(z-z.width*.5), tx.t,  pos=4, cex=.75,srt=0, font = 1); # pos=4,
      }
      
    }
  }
  e.t = events.t[type == 'vx']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = x[which.min(abs(as.Date(dates.t) - e.t[i]$start))*2]; 
      d.t2 = x[which.min(abs(as.Date(dates.t) - e.t[i]$end))*2];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
      if(loc.t %in% c('uk', 'in')){
        arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
        text(d.t1+offset,ymax*(z-z.width*8), tx.t,  pos=4, cex=.75,srt=0, font = 1); # pos=4,
      } else {
        arrows(x0=d.t1, y0=ymax*(z-z.width*(4+2)), x1=d.t2, length=.05)
        text(d.t1+offset*2,ymax*(z-z.width*4.5), tx.t,  pos=4, cex=.75,srt=0, font = 1); # pos=4,
      }
      
    }
  }
  legend(c(1, 2500), c('Reported cases','Reported deaths','Model estimates'), seg.len = .8, cex = .8, lty = rep(NA, 3), pch = c(8,8,0), lwd = 2, col = c('blue','red','grey'), bty = 'n')
  
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
}
# validation
cnt = cnt + 1
{
  # for in
  x = 1: length(dates.in.t); ymax = max(est.in$ci95.upr, obs.in$ci95.upr, na.rm = T)*1.05
  plot(x,est.in$mean,ylab='Cumulative infection rate (%)', ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='',xaxt='n')
  lines(x,est.in$mean, col='blue',lwd=2)
  polygon(c(x,rev(x)),c(est.in$ci95.lwr,rev(est.in$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(est.in$iqr.lwr,rev(est.in$iqr.upr)),col=alpha('blue',.3),border='transparent')
  points(x, obs.in$mean, pch = '*', lwd = 2, cex = 1, col='red')
  arrows(x, obs.in$ci95.lwr, x, obs.in$ci95.upr, length=0.03, angle=90, code=3, col = 'red', lwd = 1)
  axis(1,at=x,labels = format(dates.in.t,'%m/%d/%y'),mgp=c(1.0,.1,0),cex.axis=.85)
  mtext(p.titles[cnt], line = .1, adj = 0, cex = .85)
  legend('topleft', c('Model estimates','Data from 3 nationwide surveys\n(Murhekar et al. 2020; 2021a; 2021b)'), seg.len = .8, cex = .8, lty = 1, lwd = 2, col = c('blue','red'), bty = 'n')
  
}
# estimated seasonality
for(loc.t in locs){
  cnt = cnt + 1
  da.t = da[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
    setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))
  # only when it is > 10 cases
  # da.t = da.t[case > 1]
  da.t = da.t[case > 2 | death > 0]
  obs = obs[case > 2 | death > 0]
  da.t$date = da.t$date %>% as.Date
  
  rel.mob = 1 + da.t$mob.bus/100# as.matrix() 
  
  # get relative R0 for seasonality
  if(useRecentWeaData){
    relR0 = data.table(week = 1:53, rel.R0 = fn_getRelR0.loc(da.t = da.wea.recent, ref.wk, Rwea_parm.bounds=Rwea_parm.bounds, smooth = T) %>% rowMeans ) 
    
  } else {
    # use histroical data
    relR0 = data.table(week = 1:53, rel.R0 = fn_getRelR0(loc.t, ref.wk = da.t$week[1], Rwea_parm.bounds=Rwea_parm.bounds) %>% rowMeans ) 
  }
  
  
  da.t$rel.mob = rel.mob
  da.t = merge(da.t, relR0, by = 'week')
  da.t = da.t[order(date)]
  da2.t = da.t; da2.t$death = da2.t$death * 10
  da2.t = melt(da2.t[,c('date','case','death')], id.vars = 'date')
  
  ymin = min(da.t$rel.mob, da.t$rel.R0) * .9; ymax = max(da.t$rel.mob, da.t$rel.R0) * 1.1
  xx = barplot(value ~ variable + date, ylim = c(0, max(da.t$case) * 1.1), data = da2.t, xaxt = 'n', 
               ylab =  'Case per million / Death per 100,000', xlab = '', beside = T, border = 'transparent', col = c('grey50', 'red'))
  # xx = barplot(da.t$case, ylim = c(0, max(da.t$case) * 1.1), ylab = '', border = 'grey50', col = 'transparent')
  # barplot(da.t$death, ylim = c(0, max(da.t$case) * 1.1), add =T, beside = T, col = 'red', border = NA)
  # mtext(cnty[cnt], side = 3, line = -1.2, cex = .8, adj = 0.01 , outer = F)
  
  par(new = T)
  xx = colMeans(xx)
  plot(xx, da.t$rel.mob, ylim = c(ymin, ymax), type = 'l', ylab = '', xlab = '', xaxt = 'n', yaxt = 'n', col = 'blue', lwd = 1.5)
  lines(xx, da.t$rel.R0, ylim = c(ymin, ymax), col = 'orange', lwd = 1.5)
  abline(h = 1, col = 'grey50', lty=2)
  axis(4)
  
  # add time lines
  events.t = events[loc == loc.t]
  events.t[is.na(end)]$end = max(dates.t)
  offset= -2; y.offset = ymax*.006
  z=1; z.width = .02; 
  e.t = events.t[type == 'npi']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      d.t3 = mean(c(d.t1, d.t2))
      tx.t = e.t[i]$event
      rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
      # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
      text(d.t3, ifelse(grepl('Lockdown', tx.t), .46, .6), tx.t, adj = .5, cex=.75,srt=0, font = 2, col='blue'); # pos=4,
    }
  }
  e.t = events.t[type == 'seasonality']
  if(nrow(e.t)>0){
    for(i in 1:nrow(e.t)){
      d.t1 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$start))]; 
      d.t2 = xx[which.min(abs(as.Date(dates.t) - e.t[i]$end))];
      tx.t = e.t[i]$event
      d.t3 = mean(c(d.t1, d.t2)) + ifelse(grepl('Winter',tx.t), offset*1.5, -offset*1.5)
      
      # rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
      # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
      text(d.t3, ifelse(grepl('Winter',tx.t) | grepl('Monsoon',tx.t), 1.04, .96), tx.t, adj = .5, cex=.75,srt=0, font = 2, col = 'orange'); # pos=4,
    }
  }
  
  legend('topleft', legend = c('case', 'death', 'mobility', 'seasonality'), pch = c(15, 15, NA, NA), seg.len = .8, 
         ncol=4, x.intersp = .25, text.width = 32,
         lty = c(NA, NA, 1,1), col = c('grey50', 'red', 'blue', 'orange'), cex = 1, lwd = 2, bty = 'n')
  
  axis(1, at = xx, labels = da.t$date %>% format('%m/%d/%y'))
  mtext('Mobility / Seasonal trend', side = 4, cex = .85, outer = F, line = 1.1)
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
}
# key parameters
for(loc.t in locs){
  
  # do estimated infections and Rt
  cnt=cnt+1
  
  mm = res.train[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  dates.t = unique(as.Date(mm$date)) %>% sort
  x=1:length(dates.t)
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.1;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent') # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  if(T){
    # add time lines
    events.t = events[loc == loc.t]
    events.t[is.na(end)]$end = max(dates.t)
    offset= -2; y.offset = ymax*.006
    z=1; z.width = .02; 
    e.t = events.t[type == 'npi']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey',.2), border = 'transparent') # ymax*(z-z.width*2)
        rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
        # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
        text(d.t3,ymax-y.offset, tx.t, adj = .5, cex=.7,srt=0, font = 1); # pos=4,
      }
    }
    e.t = events.t[type == 'virus']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
    e.t = events.t[type == 'vx']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
          text(d.t1,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+2)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
  }
  
  # overlay with cumulative infection rate
  par(new=T)
  tda=res.train[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.1; 
  
  plot(x,tda$mean,ylab='', yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='',xaxt='n')
  abline(h = 1, lty = 1, col = 'grey')
  lines(x,tda$mean, col='blue',lwd=2)
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Rt', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  # midle panel
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  mm = res.train[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent') # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  if(T){
    # add time lines
    events.t = events[loc == loc.t]
    events.t[is.na(end)]$end = max(dates.t)
    offset= -2; y.offset = ymax*.006
    z=1; z.width = .02; 
    e.t = events.t[type == 'npi']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey',.2), border = 'transparent') # ymax*(z-z.width*2)
        rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
        # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
        text(d.t3,ymax-y.offset, tx.t, adj = .5, cex=.7,srt=0, font = 1); # pos=4,
      }
    }
    e.t = events.t[type == 'virus']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
    e.t = events.t[type == 'vx']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
          text(d.t1,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+2)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
  }
  
  # overlay with cumulative infection rate
  par(new=T)
  tda=res.train[loc == loc.t & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(-.5, ymax),type='l',col='blue',lwd=2,xlab='',xaxt='n')
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated transmissibility', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  # right panel
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  mm = res.train[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent') # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  if(T){
    # add time lines
    events.t = events[loc == loc.t]
    events.t[is.na(end)]$end = max(dates.t)
    offset= -2; y.offset = ymax*.006
    z=1; z.width = .02; 
    e.t = events.t[type == 'npi']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey',.2), border = 'transparent') # ymax*(z-z.width*2)
        rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
        # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
        text(d.t3,ymax-y.offset * ifelse(i == 1, 10, 0), tx.t, adj = .5, cex=.7,srt=0, font = 1); # pos=4,
      }
    }
    e.t = events.t[type == 'virus']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
    e.t = events.t[type == 'vx']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
          text(d.t1,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+2)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
  }
  
  # overlay with cumulative infection rate
  par(new=T)
  tda=res.train[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',yaxt='n',ylim=c(0, 100),type='l',col='blue',lwd=2,xlab='',xaxt='n')
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Estimated susceptibility (%)', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  
  mtext(p.titles[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
}
dev.off()
########################################################################################################################


########################################################################################################################
# OTHER KEY PARAM
p.titles5 = c('(A) Infection-detection rate', '(B) Infection-fatality risk', # v. infection rate
              '(C) South Africa: Infection-detection rate', '(D) South Africa: Infection-fatality risk',
              '(E) Brazil: Infection-detection rate', '(F) Brazil: Infection-fatality risk'
)
pdf(paste0(dir_plot, 'FigS1.pdf'), width = 7, height = 2.5)
par(mfrow=c(1,2),mar=c(1.7,2.1,1.3,2.1),oma=c(0,.1, 0,.1),mgp=c(.9,.1,0),cex=.8,cex.axis=.85,cex.lab=.9,tck=-.01)
cnt=0
for(loc.t in locs){
  
  # do estimated infections and Rt
  cnt=cnt+1
  
  mm = res.train[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  dates.t = unique(as.Date(mm$date)) %>% sort
  x=1:length(dates.t)
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.1;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent') # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  
  if(T){
    # add time lines
    events.t = events[loc == loc.t]
    events.t[is.na(end)]$end = max(dates.t)
    offset= -2; y.offset = ymax*.006
    z=1; z.width = .02; 
    e.t = events.t[type == 'npi']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey',.2), border = 'transparent') # ymax*(z-z.width*2)
        rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
        # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
        text(d.t3,ymax-y.offset, tx.t, adj = .5, cex=.7,srt=0, font = 1); # pos=4,
      }
    }
    e.t = events.t[type == 'virus']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
          text(d.t1+offset*4,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
    e.t = events.t[type == 'vx']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
          text(d.t1,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+2)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
  }
  
  # overlay with cumulative infection rate
  par(new=T)
  tda=res.train[loc == loc.t & state == 'infection detection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda$value = tda$value * 100
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.1; 
  
  plot(x,tda$mean,ylab='', yaxt='n',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='',xaxt='n')
  abline(h = 1, lty = 1, col = 'grey')
  lines(x,tda$mean, col='blue',lwd=2)
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Infection-detection rate (%)', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  mtext(p.titles5[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
  
  # midle panel
  # do estimated infections and susceptibility
  cnt=cnt+1
  x=1:length(dates.t)
  mm = res.train[loc == loc.t & state == 'infection'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  stats = matrix(0, 5, length(dates.t))
  for(id in 1:length(dates.t)){
    tmp = mm[date==dates.t[id]]
    stats[,id] = c(tmp[stat=='ci95.lwr']$value, tmp[stat=='iqr.lwr']$value, tmp[stat=='mean']$value, tmp[stat=='iqr.upr']$value, tmp[stat=='ci95.upr']$value)
  }
  colnames(stats) = dates.t
  ymax=max(mm$value, na.rm = T)*1.05;
  summarydata=list(stats=stats,n=dates.t,names=rep('',length(dates.t)))
  bxp(summarydata, ylab='', yaxt='n', xaxt='n', at=x, ylim = c(0, ymax), border = 'grey50', fill='transparent') # xlim=c(0.5,length(dates.t)+.5),
  axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(4,mgp=c(.9,.1,0),cex.axis=.85, col.ticks = 'grey30', col.lab = 'grey30', col.axis='grey30')
  mtext('Estimated infections per million', col = 'grey30',side=4,line = .9, outer = F,cex=.75)
  if(T){
    # add time lines
    events.t = events[loc == loc.t]
    events.t[is.na(end)]$end = max(dates.t)
    offset= -2; y.offset = ymax*.006
    z=1; z.width = .02; 
    e.t = events.t[type == 'npi']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey',.2), border = 'transparent') # ymax*(z-z.width*2)
        rect(xleft=d.t1, ybottom=-1000, xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey', ifelse(grepl('Lockdown', tx.t), .2, .05)), border = 'transparent') # ymax*(z-z.width*2)
        # arrows(x0=d.t, y0=ymax*(z-z.width*(i*2+1)), x1=max(x), length=.05)
        text(d.t3,ymax-y.offset, tx.t, adj = .5, cex=.7,srt=0, font = 1); # pos=4,
      }
    }
    e.t = events.t[type == 'virus']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+1.5)), x1=d.t2, length=.05)
          text(d.t1+offset*4,ymax*(z-z.width*4), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*2), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
    e.t = events.t[type == 'vx']
    if(nrow(e.t)>0){
      for(i in 1:nrow(e.t)){
        d.t1 = which.min(abs(as.Date(dates.t) - e.t[i]$start)); 
        d.t2 = which.min(abs(as.Date(dates.t) - e.t[i]$end));
        d.t3 = mean(c(d.t1, d.t2))
        tx.t = e.t[i]$event
        # rect(xleft=d.t1, ybottom=ymax*(z-z.width*2), xright=d.t2, ytop=ymax*(z+z.width*2), angle = 45,col = alpha('grey50',.3), border = 'transparent')
        # arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
        # text(d.t1+offset*2,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        if(loc.t %in% c('uk','in')){
          arrows(x0=d.t1, y0=ymax*(z-z.width*(8+1.5)), x1=d.t2, length=.05)
          text(d.t1,ymax*(z-z.width*8), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        } else {
          arrows(x0=d.t1, y0=ymax*(z-z.width*(4+2)), x1=d.t2, length=.05)
          text(d.t1+offset*2,ymax*(z-z.width*4.5), tx.t,  pos=4, cex=.7,srt=0, font = 1); # pos=4,
        }
      }
    }
  }
  
  # overlay with cumulative infection rate
  par(new=T)
  tda=res.train[loc == loc.t & state == 'IFR'] %>% setnames(c('variable','Week.start'),c('stat','date'))
  tda$value = tda$value * 100
  tda=dcast(tda, date ~ stat, value.var = 'value')
  ymax=max(tda$ci95.upr)*1.05; 
  
  plot(x,tda$mean,ylab='',ylim=c(0, ymax),type='l',col='blue',lwd=2,xlab='',xaxt='n',yaxt='n')
  polygon(c(x,rev(x)),c(tda$ci95.lwr,rev(tda$ci95.upr)),col=alpha('blue',.15),border='transparent')
  polygon(c(x,rev(x)),c(tda$iqr.lwr,rev(tda$iqr.upr)),col=alpha('blue',.3),border='transparent')
  # axis(1,at=x,labels = format(dates.t,'%m/%d/%y'),mgp=c(.9,.1,0),cex.axis=.85)
  axis(2,mgp=c(1.0,.1,0),cex.axis=.85, col.ticks = 'blue', col.lab = 'blue', col.axis='blue')
  mtext('Infection-fatality risk (%)', side=2, outer = F, line = .9, cex=.75, col = 'blue')
  
  mtext(p.titles5[cnt],cex=.85,side=3,outer = F,line=.1,adj=0)
  
}
dev.off()
########################################################################################################################


########################################################################################################################
# PLOT COUNTRERFACTUAL MODELING RESULTS
# COUNTERFACUTAL - ASSUMING NO VACCINATION AFTTER JUNE 2021
# PROJECTION - FROM THE BEST-PERFORMING MODEL + REPORTED VACCINATION DATA
# OBSERVATIONS - REPORTED CASES AND DEATHS


loc.t = 'in'
da.vacc = read.csv(paste0(dir_data,'vx.nolag.per1Mpop_',loc.t, '_2021-11-08','.csv')) %>% data.table()
da.vacc$date = da.vacc$date %>% as.Date
da.vacc$cum.v1 = cumsum(da.vacc$n.v1) / N 
da.vacc$cum.v2 = cumsum(da.vacc$n.v2) / N 

load(paste0(dir_res.sub, "res.proj",date.tag,".RData"))
col.proj = 'cornflowerblue' # 'darkblue' # 'darkcyan' # 
col.novx = 'red'
{
  sce.counter = paste0('novx', best.sce)  #  'VEpriorInf0-0'
  sce.counter = paste0('novx', sce.base)
  tda.proj = dcast(res.proj[scenario == best.sce], loc + scenario + measure + Week.start ~ variable)
  dates.t = unique(tda.proj$Week.start) %>% as.Date %>% sort
  tda.proj$date = tda.proj$Week.start %>% as.Date
  tda.proj$loc = NULL; tda.proj$scenario = NULL; tda.proj$Week.start = NULL
  tda.train = dcast(res.train[state %in% c('case','death')], state + Week.start ~ variable, value.var = 'value')
  tda.train$date = tda.train$Week.start  %>% as.Date
  tda.train$measure = factor(tda.train$state, levels = c('case','death'), labels = c('Cases', 'Deaths'))
  tda.est = rbind(tda.train[,colnames(tda.proj),with=F], tda.proj)
  
  da.t = da.full[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
    setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))
  da.t = da.t[case > 2 | death > 0]
  da.t$date = da.t$date %>% as.Date
  rel.mob = 1 + da.t$mob.bus/100# as.matrix() 
  # get relative R0 for seasonality
  # get relative R0 for seasonality
  if(useRecentWeaData){
    relR0 = data.table(week = 1:53, rel.R0 = fn_getRelR0.loc(da.t = da.wea.recent, ref.wk, Rwea_parm.bounds=Rwea_parm.bounds, smooth = T) %>% rowMeans ) 
    
  } else {
    # use histroical data
    relR0 = data.table(week = 1:53, rel.R0 = fn_getRelR0(loc.t, ref.wk = da.t$week[1], Rwea_parm.bounds=Rwea_parm.bounds) %>% rowMeans ) 
  }
  
  da.t$rel.mob = rel.mob
  da.t = merge(da.t, relR0, by = 'week')
  da.t = da.t[order(date)]
  da.t = merge(da.t, da.vacc, by = 'date')
  # add projection
  da.t = merge(da.t, tda.est[measure == 'Cases'], all=T, by = 'date')
  da.t = merge(da.t, tda.est[measure == 'Deaths'], all=T, by = 'date', suffixes = c('.case','.death'))
  da.t = da.t[date > as.Date('2021/07/1')]
  n.tr = da.t[date <= max(tda.train$date)] %>% nrow()
  
  tda.counter = dcast(res.proj[scenario == sce.counter], loc + scenario + measure + Week.start ~ variable)
  dates.t = unique(tda.proj$Week.start) %>% as.Date %>% sort
  tda.counter$date = tda.counter$Week.start %>% as.Date
  tda.counter$loc = NULL; tda.counter$scenario = NULL; tda.counter$Week.start = NULL
  
  tda.counter.est = rbind(tda.train[,colnames(tda.counter),with=F], tda.counter) %>% 
    setnames(c("ci95.lwr", "ci95.upr", "iqr.lwr", "iqr.upr",  "median"), 
             paste0(c("ci95.lwr", "ci95.upr", "iqr.lwr", "iqr.upr",  "median"), '.counter'))
  # add counter factual
  da.t = merge(da.t, tda.counter.est[measure == 'Cases'], all.x =T, by = 'date')
  da.t = merge(da.t, tda.counter.est[measure == 'Deaths'], all.x =T, by = 'date', suffixes = c('.case','.death'))
  
  
  tda.counter[date == max(date) & measure == 'Cumulative Deaths']
  tda.proj[date == max(date) & measure == 'Cumulative Deaths']
  tda.counter[date == max(date) & measure == 'Cumulative Infections'] 
  tda.proj[date == max(date) & measure == 'Cumulative Infections']
  
  tda.counter[date == max(date) & measure == 'Cumulative Infections']$median /
    tda.proj[date == max(date) & measure == 'Cumulative Infections']$median 
  
  
  pdf(paste0(dir_plot, 'Fig2.pdf'), width = 7, height = 3)
  par(mfrow=c(1,2),mar=c(2.5,2.5,1.5,2.5),oma=c(0.1,.1, 0.1,0),mgp=c(1.0,.1,0),cex=.8,cex.axis=.85,cex.lab=.9,tck=-.015)
  # cases
  xx = barplot(da.t$cum.v1, ylim = c(0, 1.25),  ylab='', xlab='', yaxt='n', xaxt = 'n', col = 'transparent', border = 'grey')
  barplot(da.t$cum.v2, add = T, ylim = c(0, 1.25),  ylab='', xlab='', yaxt='n', xaxt = 'n', col = alpha('grey',.3), border = 'grey')
  lines(xx, da.t$rel.mob, col = 'blue', lwd = 1.5)
  lines(xx, da.t$rel.R0, col = 'orange', lwd = 1.5)
  abline(h=1, col='grey', lty = 5)
  # arrows(x0= xx[n.tr+1], y0= .45, x1= xx[23], length=.05, lwd = .5, col = col.novx)
  # text(x = xx[n.tr+1], y = .49, 'projection', adj = 0, cex=.75, col = col.novx)
  # abline(v = 1+n.tr, col = 'grey', lty = 3)
  text(x = xx[nrow(da.t)-1], y = 1.19, 'seasonality', adj = .5, cex=.75,srt=0, font = 2, col = 'orange'); # pos=4,
  text(x = xx[nrow(da.t)-1], y = .99, 'mobility', adj = .5, cex=.75,srt=0, font = 2, col = 'blue'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v1)+.06, 'vacciation:\n1st dose', adj = 1, cex=.75,srt=0, font =1, col = 'grey20'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v2)+.06, 'vacciation:\n2nd dose', adj = 1, cex=.75,srt=0, font =1, col = 'grey20'); # pos=4,
  
  axis(4)
  mtext("Vaccination rate / Mobility / Seasonality", side = 4, outer = F, line = 1.1, cex = .75)
  # overlay with cumulative vx rate, seasonality, mobility
  par(new = T)
  # xx = 1:nrow(da.t)
  
  plot(xx, da.t$median.case, ylim = c(0, max(c(da.t$case, da.t$iqr.upr.counter.case))*1.01), type = 'l', col = 'transparent', lwd = 1.5, lty = 5,
       ylab = 'Weekly number of cases per million', xlab = 'Week start (mm/dd/yy)', xaxt = 'n')
  lines(xx[1:n.tr], da.t$median.case[1:n.tr], lwd=1.5, col = 'grey')
  lines(xx[(n.tr+1):nrow(da.t)], da.t$median.case[(n.tr+1):nrow(da.t)], lwd=1.5, lty = 5, col = col.proj)
  polygon(x = c(xx[(n.tr+1):nrow(da.t)], rev(xx[(n.tr+1):nrow(da.t)])), y = c(da.t[(n.tr+1):nrow(da.t)]$iqr.lwr.case, rev(da.t[(n.tr+1):nrow(da.t)]$iqr.upr.case)),
          col = alpha(col.proj,.2), border = 'transparent')
  # counterfactual
  lines(xx[(n.tr+1):nrow(da.t)], da.t$median.counter.case[(n.tr+1):nrow(da.t)], lwd=1.5, lty = 5, col = col.novx)
  polygon(x = c(xx[(n.tr+1):nrow(da.t)], rev(xx[(n.tr+1):nrow(da.t)])), y = c(da.t[(n.tr+1):nrow(da.t)]$iqr.lwr.counter.case, rev(da.t[(n.tr+1):nrow(da.t)]$iqr.upr.counter.case)),
          col = alpha(col.novx,.1), border = 'transparent')
  points(xx, da.t$case, pch = 'x', cex = .8, col = 'black')
  
  axis(1, at = xx, labels = format(da.t$date,'%m/%d/%y'))
  mtext('(A) Trends in cases vs projection', line = .1, outer = F, cex = .85, adj = 0)
  
  # deaths
  xx = barplot(da.t$cum.v1, ylim = c(0, 1.25),  ylab='', xlab='', yaxt='n', xaxt = 'n', col = 'transparent', border = 'grey')
  barplot(da.t$cum.v2, add = T, ylim = c(0, 1.25),  ylab='', xlab='', yaxt='n', xaxt = 'n', col = alpha('grey',.3), border = 'grey')
  lines(xx, da.t$rel.mob, col = 'blue', lwd = 1.5)
  lines(xx, da.t$rel.R0, col = 'orange', lwd = 1.5)
  abline(h=1, col='grey', lty = 5)
  # arrows(x0= xx[n.tr+1], y0= .45, x1= xx[23], length=.05, lwd = .5, col = col.novx)
  # text(x = xx[n.tr+1], y = .49, 'projection', adj = 0, cex=.75, col = col.novx)
  text(x = xx[nrow(da.t)-1], y = 1.19, 'seasonality', adj = .5, cex=.75,srt=0, font = 2, col = 'orange'); # pos=4,
  text(x = xx[nrow(da.t)-1], y = .99, 'mobility', adj = .5, cex=.75,srt=0, font = 2, col = 'blue'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v1)+.06, 'vacciation:\n1st dose', adj = 1, cex=.75,srt=0, font =1, col = 'grey20'); # pos=4,
  text(x = max(xx)+1, y = max(da.t$cum.v2)+.06, 'vacciation:\n2nd dose', adj = 1, cex=.75,srt=0, font =1, col = 'grey20'); # pos=4,
  axis(4)
  mtext("Vaccination rate / Mobility / Seasonality", side = 4, outer = F, line = 1.1, cex = .75)
  # overlay with cumulative vx rate, seasonality, mobility
  par(new = T)
  # xx = 1:nrow(da.t)
  plot(xx, da.t$median.death, ylim = c(0, max(c(da.t$death, da.t$iqr.upr.counter.death))*1.01), type = 'l', col = 'transparent', lwd = 1.5, lty = 5,
       ylab = 'Weekly number of deaths per million', xlab = 'Week start (mm/dd/yy)', xaxt = 'n')
  lines(xx[1:n.tr], da.t$median.death[1:n.tr], lwd=1.5, col = 'grey')
  lines(xx[(n.tr+1):nrow(da.t)], da.t$median.death[(n.tr+1):nrow(da.t)], lwd=1.5, lty = 5, col = col.proj)
  polygon(x = c(xx[(n.tr+1):nrow(da.t)], rev(xx[(n.tr+1):nrow(da.t)])), y = c(da.t[(n.tr+1):nrow(da.t)]$iqr.lwr.death, rev(da.t[(n.tr+1):nrow(da.t)]$iqr.upr.death)),
          col = alpha(col.proj,.2), border = 'transparent')
  # counterfactual
  lines(xx[(n.tr+1):nrow(da.t)], da.t$median.counter.death[(n.tr+1):nrow(da.t)], lwd=1.5, lty = 5, col = col.novx)
  polygon(x = c(xx[(n.tr+1):nrow(da.t)], rev(xx[(n.tr+1):nrow(da.t)])), y = c(da.t[(n.tr+1):nrow(da.t)]$iqr.lwr.counter.death, rev(da.t[(n.tr+1):nrow(da.t)]$iqr.upr.counter.death)),
          col = alpha(col.novx,.1), border = 'transparent')
  points(xx, da.t$death, pch = 'x', cex = .8, col = 'black')

  axis(1, at = xx, labels = format(da.t$date,'%m/%d/%y'))
  mtext('(B) Trends in deaths vs projection', line = .1, outer = F, cex = .85, adj = 0)
  dev.off()
  
}
########################################################################################################################



# look at the numbers
loc.t = 'in'
da.vacc.nolag = read.csv(paste0(dir_data,'vx.nolag.per1Mpop_',loc.t, '_2021-11-08','.csv')) %>% data.table()
da.vacc.nolag$date = da.vacc.nolag$date %>% as.Date
da.vacc.nolag$cum.v1 = cumsum(da.vacc.nolag$n.v1) / N 
da.vacc.nolag$cum.v2 = cumsum(da.vacc.nolag$n.v2) / N 
da.vacc.nolag[date == as.Date('2021/06/30')]
da.vacc.nolag[date == as.Date('2021/10/15')]

da.vacc = read.csv(paste0(dir_data,'vx.lagged.per1Mpop_',loc.t, date.tag,'.csv')) %>% data.table()
da.vacc$date = da.vacc$date %>% as.Date
colSums(da.vacc[,c('n.v1','n.v2')])
colSums(da.vacc[date < as.Date('2021/07/1'),c('n.v1','n.v2')]) / N * 100
# time lag
Rt.t=res.train[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
Rt.t=dcast(Rt.t, date ~ stat, value.var = 'value')
cumI.t=res.train[loc == loc.t & state == 'Cumulative infection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
cumI.t=dcast(cumI.t, date ~ stat, value.var = 'value')
S.t=res.train[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
S.t=dcast(S.t, date ~ stat, value.var = 'value')
obs.t = da[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
  setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))

summary(Rt.t[date %in% seq(as.Date('2020/06/01'), as.Date('2020/09/30'), by = 'day')]$mean)

tail(S.t, 1)

# compute cumulative infection rate for each period of time
load(paste0(dir_res, 'res.cumIperc_ens',date.tag,'.RData'))
sce.t = gsub('VEpriorInf','',best.sce)
tmp = cumIperc_ens[eval == 'obs.more' & date == '2020-05-17' & VEpriorInf == sce.t]
summary(tmp[,-c(1:4)]%>% unlist)
(tmp[,-c(1:4)]%>% unlist) %>% quantile(prob=c(.025,.975))
(tmp[,-c(1:4)]%>% unlist) %>% mean
tmp = cumIperc_ens[eval == 'obs.more' & date == '2020-08-16' & VEpriorInf == sce.t]
summary(tmp[,-c(1:4)]%>% unlist)
tmp = cumIperc_ens[eval == 'obs.more' & date == '2020-12-27' & VEpriorInf == sce.t]
summary(tmp[,-c(1:4)]%>% unlist)
tmp = cumIperc_ens[eval == 'obs.more' & date == '2021-01-31' & VEpriorInf == sce.t]
(tmp[,-c(1:4)]%>% unlist) %>% quantile(prob=c(.025,.975))
(tmp[,-c(1:4)]%>% unlist) %>% mean

cumIperc_ens$date = cumIperc_ens$date %>% as.Date
fn_getCumI = function(tda, sce.t, eval.t, loc.t, t.start, t.end){
  tda = tda[VEpriorInf == sce.t & eval == eval.t & loc == loc.t & (date == as.Date(t.start) | date == as.Date(t.end))]
  tda2 = tda[, -c(1:4)] 
  tda2 = (tda2[2] - tda2[1]) %>% unlist
  data.table(mean = tda2 %>% mean,
             (tda2 %>% quantile(probs = c(.5, .25, .75, .025, .975))) %>% t)
}

fn_getCumI(tda = cumIperc_ens, sce.t = sce.t, eval.t = eval.t, loc.t = 'in',t.start = as.Date('2021-02-28'), t.end = as.Date('2021-5-30'))

fn_getCumI(tda = cumIperc_ens, sce.t = sce.t, eval.t = eval.t, loc.t = 'in',t.start = as.Date('2021-03-21'), t.end = as.Date('2021-6-27'))


tda=res.train[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
tda=dcast(tda, date ~ stat, value.var = 'value')

tda=res.train[loc == loc.t & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
tda=dcast(tda, date ~ stat, value.var = 'value')

tda=res.train[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
tda=dcast(tda, date ~ stat, value.var = 'value')


parms = read.csv(paste0(dir_res, 'tab_voc.b1617.summary.est', date.tag,'.csv')) %>% data.table()
parms.best = parms[eval == eval.t & loc == loc.t & VEpriorInf == gsub('VEpriorInf','', sce.t)]
parms.best[,c('mean','ci95.lwr','ci95.upr')] %>% apply(1, fn_format, roundigt = 2)


# cp delta and alpha
dRtx.alpha = c(46.6, 32.3, 54.6) /100
dRtx.delta = (parms.best[state=='dRtx',c('mean','ci95.lwr','ci95.upr')] / 100) %>% unlist
dImm.delta = (parms.best[state=='dImm',c('mean','ci95.lwr','ci95.upr')] / 100) %>% unlist
(dRtx.delta + 1) / (dRtx.alpha + 1) * (dImm.delta + 1) - 1





