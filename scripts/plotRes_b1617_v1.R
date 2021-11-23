# to plot and summarize results for delta

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

N = 1e6; # population size is set to 1M

eval.t = 'obs.more'
dir_data = '../data/'
dir_code =  './' 

dir_res = paste0('../results/')
dir_plot = paste0('../results/')

if(!file.exists(dir_plot)) dir.create(dir_plot)

source(paste0(dir_code,'fn_util.R'))
source(paste0(dir_code,'getPlot.R'))
source(paste0(dir_code,'get_relR0.R'))


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
da = read.csv(paste0(dir_data,'da_case.death.mob_uk.sa.br.in.csv'), stringsAsFactors = F)  %>% data.table()
da[date < as.Date('2020-02-09') & is.na(da)] = 0
da = da[date >= as.Date('2020-03-01')]
da = da[complete.cases(da)]
loc.t = 'in'


# load results
load(paste0(dir_res, "res.summary.RData"))
res.train = res.train[eval == eval.t]
tda = res.train[loc == 'in']
tda1 = tda[state == 'Infectious']
locs = c('in')
# plot model fit and overlay with estimated cumulative infection rate?

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
pdf(paste0(dir_plot, 'Fig1_model_fit_validation_keyest.pdf'), width = 8, height = 7)
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
  relR0 = data.table(week = 1:53, rel.R0 = fn_getRelR0(loc.t, ref.wk = da.t$week[1], Rwea_parm.bounds=Rwea_parm.bounds) %>% rowMeans ) 
  
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


# OTHER KEY PARAM
p.titles5 = c('(A) Infection-detection rate', '(B) Infection-fatality risk', # v. infection rate
              '(C) South Africa: Infection-detection rate', '(D) South Africa: Infection-fatality risk',
              '(E) Brazil: Infection-detection rate', '(F) Brazil: Infection-fatality risk'
)
pdf(paste0(dir_plot, 'FigS_other.parm.est.pdf'), width = 7, height = 2.5)
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
  
  getPlotProj = function(tda, title.t, y.lab = 'Number per 1 M people', ncol.t = 3, withObs = F, col.set){
    
    dates.t = unique(tda$Week.start) %>% as.Date
    
    p = ggplot(tda)+
      geom_line(aes(x = Week.start, y = median, color = vx), size = 1) +  # no ctrl
      geom_ribbon(aes(x = Week.start, ymin = iqr.lwr, ymax = iqr.upr, fill = vx),  alpha = .2) +
      facet_rep_wrap(~ npi, # scales = 'free_y', 
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
  
  
  
}

# plot the projections
load(paste0(dir_res, "res.proj.RData"))
tda = dcast(res.proj, loc + scenario + measure + Week.start ~ variable)
tda$npi = factor(tda$scenario, levels = c('openNowcurVx','openNow2xVx','openNow4xVx',
                                          'delay4wkcurVx','delay4wk2xVx','delay4wk4xVx',
                                          'delay8wkcurVx','delay8wk2xVx','delay8wk4xVx'),
                 labels = c(rep('Reopen now',3),rep('Delay by 4 weeks',3),rep('Delay by 8 weeks',3)))
tda$vx = factor(tda$scenario, levels = c('openNowcurVx','openNow2xVx','openNow4xVx',
                                          'delay4wkcurVx','delay4wk2xVx','delay4wk4xVx',
                                          'delay8wkcurVx','delay8wk2xVx','delay8wk4xVx'),
                 labels = rep(c('Current','2x higher','4x higher'),3))
dates.t = unique(tda$Week.start) %>% as.Date %>% sort

p1 = getPlotProj(tda = tda[measure == 'Infections'], title.t = "(A) Projected number of infections per week")
p2 = getPlotProj(tda = tda[measure == 'Cases'], title.t = "(B) Projected number of reported cases per week")
p3 = getPlotProj(tda = tda[measure == 'Deaths'], title.t = "(C) Projected number of reported deaths per week")

pdf(paste0(dir_plot, 'Fig2_projections.pdf'),width = 7, height = 6)
grid.arrange(
  grobs = list(p1, p2, p3),
  layout_matrix = rbind(c(1),
                        c(2),
                        c(3))
)
dev.off()


# generate table

# look at the numbers
loc.t = 'in'
Rt.t=res.train[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
Rt.t=dcast(Rt.t, date ~ stat, value.var = 'value')
cumI.t=res.train[loc == loc.t & state == 'Cumulative infection rate'] %>% setnames(c('variable','Week.start'),c('stat','date'))
cumI.t=dcast(cumI.t, date ~ stat, value.var = 'value')
S.t=res.train[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
S.t=dcast(S.t, date ~ stat, value.var = 'value')
obs.t = da[,c('date','year','week',paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t)),with=F] %>% 
  setnames(paste0(c('case.','death.','mob.bus.','mob.full.'),loc.t), c('case','death','mob.bus','mob.full'))

# compute cumulative infection rate for each period of time
load(paste0(dir_res, 'res.cumIperc_ens.RData'))
cumIperc_ens$date = cumIperc_ens$date %>% as.Date
fn_getCumI = function(tda, eval.t, loc.t, t.start, t.end){
  tda = tda[eval == eval.t & loc == loc.t & (date == as.Date(t.start) | date == as.Date(t.end))]
  tda2 = tda[, -c(1:3)] 
  tda2 = (tda2[2] - tda2[1]) %>% unlist
  data.table(mean = tda2 %>% mean,
             (tda2 %>% quantile(probs = c(.5, .25, .75, .025, .975))) %>% t)
}

fn_getCumI(tda = cumIperc_ens, eval.t = eval.t, loc.t = 'in',t.start = as.Date('2021-01-31'), t.end = as.Date('2021-5-30'))

tda=res.train[loc == loc.t & state == 'Rt'] %>% setnames(c('variable','Week.start'),c('stat','date'))
tda=dcast(tda, date ~ stat, value.var = 'value')

tda=res.train[loc == loc.t & state == 'Rtx'] %>% setnames(c('variable','Week.start'),c('stat','date'))
tda=dcast(tda, date ~ stat, value.var = 'value')

tda=res.train[loc == loc.t & state == 'Susceptibility'] %>% setnames(c('variable','Week.start'),c('stat','date'))
tda=dcast(tda, date ~ stat, value.var = 'value')

