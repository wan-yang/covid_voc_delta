library(FluMoDL)
library(rnoaa)
library(stringr)
library(tidyverse)
library(data.table)
library(lubridate)
library(MMWRweek)
library(tgp)

dir_data =  '~/Documents/WORK/covid19/proj.uk.sa.br/data/'
dir_code = '~/Documents/WORK/covid19/proj.uk.sa.br/scripts/'


#source("compiled_script_haokun")
# source("functions.R")
options(noaakey = "EMZodJTNROMlnsBPqBWvVRyjkhhQZLLt") # get key from https://www.ncdc.noaa.gov/cdo-web/token
update_climate_data=T
NOAA_allStations(force_retrieve = FALSE) %>% View()
NOAA_countryStations("IN", from = "2020-01-01", to = "2021-11-14")  %>% View()

res.wea = NULL
for(yr in 2020:2021){
  print(yr)
  stations=NOAA_countryStations("IN", from = as.Date(paste0(yr,'/01/01')), to = as.Date(paste0(yr,'/12/31')))
  
  for(i in 1:nrow(stations)){
    print(i)
    sta.t = stations[i,]
    usaf.t = sta.t$usaf
    wban.t = sta.t$wban
    tmp = try(isd(usaf=usaf.t, wban=wban.t, year=yr) %>% 
      dplyr::select(date,time,temperature,temperature_dewpoint,air_pressure,usaf_station,wban_station))
    if(class(tmp)=='try-error')
      next
    
    tmp %>% mutate(temperature=as.numeric(temperature),
               temperature_dewpoint=as.numeric(temperature_dewpoint),
               air_pressure=as.numeric(air_pressure),
               date=as.Date(date,format="%Y%m%d")) %>% 
      filter(temperature != 9999 & temperature_dewpoint != 9999 &
               air_pressure!= 99999) %>%
      group_by(date) %>% 
      summarise(mean_daily_temp=mean(temperature)/10,
                mean_daily_td=mean(temperature_dewpoint)/10,
                mean_daily_ap=mean(air_pressure)/10) %>% 
      mutate(exp=6.112*exp((17.67*mean_daily_td)/(mean_daily_td + 243.5)),
             specific_humidity=(0.622 * exp)/(mean_daily_ap - (0.378 * exp)),
             usaf=usaf.t, wban=wban.t) -> wea.daily
    res.wea = rbind(res.wea, wea.daily)
  }
} # yr

# average across all stations
res.wea %>% group_by(date) %>% 
  summarise(mean_daily_temp=mean(mean_daily_temp),
          mean_daily_td=mean(mean_daily_td),
          mean_daily_ap=mean(mean_daily_ap), 
          specific_humidity=mean(specific_humidity)) -> wea.daily

weeks = wea.daily %>% .$date %>% MMWRweek() %>% 
  setnames(c("MMWRyear", "MMWRweek", "MMWRday"),c('year','week','day'))

wea.daily = cbind(weeks, wea.daily)
wea.wkly = wea.daily %>% group_by(year, week) %>% 
  summarise(mean_daily_temp=mean(mean_daily_temp),
            mean_daily_td=mean(mean_daily_td),
            mean_daily_ap=mean(mean_daily_ap), 
            specific_humidity=mean(specific_humidity)) %>% 
  ungroup() %>% data.table()

wea.wk = wea.daily %>% group_by(week) %>% # combine both years
  summarise(mean_daily_temp=mean(mean_daily_temp),
            mean_daily_td=mean(mean_daily_td),
            mean_daily_ap=mean(mean_daily_ap), 
            specific_humidity=mean(specific_humidity)) %>% 
  ungroup() %>% data.table()

wea.wkly %>% setnames(c("mean_daily_temp","specific_humidity"),c('temp','spec.hum'))

wea.wk %>% setnames(c("mean_daily_temp","specific_humidity"),c('temp','spec.hum'))

save(res.wea, file=paste0(dir_data, 'wea_india_Jan2020_Nov2021.RData'))
write.csv(wea.daily, paste0(dir_data, 'wea.daily_india_Jan2020_Nov2021.csv'), row.names = F)
write.csv(wea.wkly, paste0(dir_data, 'wea.wkly_india_Jan2020_Nov2021.csv'), row.names = F)
write.csv(wea.wk, paste0(dir_data, 'wea.wk_india_Jan2020_Nov2021.csv'), row.names = F)

temp = dcast(wea.wkly, week ~ year, value.var = 'mean_daily_temp')


sh = dcast(wea.wkly, week ~ year, value.var = 'specific_humidity')

wea.hist = read.csv(paste0(dir_data,'wea.by.week_IN.csv')) %>% data.table()

source(paste0(dir_code,'get_relR0.R'))

matplot(temp[,2:3,with=F], type = 'l', lty = 1:2)
lines(wea.hist$temp, col='blue')

matplot(sh[,2:3,with=F], type = 'l', lty = 1:2)
lines(wea.hist$spec.hum, col='blue')


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
sn.now = fn_getRelR0.loc(da.t = wea.wk, ref.wk, Rwea_parm.bounds=Rwea_parm.bounds, smooth = T)

sn.hist = fn_getRelR0.loc(da.t = wea.hist, ref.wk, Rwea_parm.bounds=Rwea_parm.bounds, smooth = T)

sn.cp = cbind(sn.now[,1], sn.hist[,1])
matplot(sn.cp, type = 'l', lty = 1:2)

load(paste0(dir_data, 'wea_india_Jan2020_Nov2021.RData'))
unique(res.wea[, c('usaf', "wban"), with = F]) %>% nrow
               
               
               
               