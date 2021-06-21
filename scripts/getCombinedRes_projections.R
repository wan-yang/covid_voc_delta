# combine projections from multiple runs 
state.names = c('S1', 'E1','I1',
                'death1', 'newIobs1','newItot1','beta',
                'Tei','Tir','Trs','Td.mean','Td.sd',
                'p.mob','alpha','ifr')
  
files = list.files(dir_res, full.names = T)
files = files[grepl('.RData',files)]
files = files[grepl('_proj', files)]

# compare performance across runs?


res.proj <-  lapply(files, function(x) {
  
  try(load(x))
  
  id <- gsub(dir_res,'',x) %>% 
    strsplit('_') %>% unlist
  
  sce.t = id[3]
  
  
  d <- proj %>%
    melt(id.vars = c('measure','Week.start'), variable.factor = F)
  
  
  d$loc = id[1] %>% strsplit('\\//') %>% unlist %>% tail(1)
  d$run <- tail(id,1) %>% strsplit('\\.') %>% unlist %>% head(1) %>% gsub(pattern = 'r', replacement =  '') %>% as.integer()
  d$scenario = sce.t
  
  d
}) %>%
  rbindlist() %>%
  (function(d) d[, j = list(value = round(mean(value), 6)), by = list(loc, scenario, measure, variable, Week.start)]) # %>%


