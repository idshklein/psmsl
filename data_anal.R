library(tidyverse)
library(sf)
library(mapview)
library(lubridate)
library(plotly)
library(leafpop)
library(broom)
main_colnames <- c("station_ID", "latitude", "longitude", "station_name", "coastline_code", "station_code", "quality_flag")
station_colnames <- c("date","height","missing","isMtl_dataflag")
filelist <- read_delim("filelist.txt",delim  = ";",col_names = main_colnames)
df <- filelist %>% 
  mutate_all(str_trim) %>%
  mutate(path = paste0("./data/",station_ID ,".rlrdata"),
         data = map(path,~read_delim(.,delim  = ";",col_names = station_colnames) %>% 
                      mutate_all(str_trim) %>% 
                      mutate(date = as.numeric(date),
                             year = floor(date),
                             month = (round((date - year)*24) + 1)/2,
                             date = ymd(paste(year,month,1,sep = "-")),
                             yearfrac = as.numeric(date)/365,
                             height = as.numeric(height),
                             height = ifelse(height == -99999, NA_integer_,height),
                             missing = as.integer(missing),
                             isMtl = as.logical(as.integer(str_sub(isMtl_dataflag,2,2))),
                             dataflag = as.logical(as.integer(str_sub(isMtl_dataflag,3,3))),
                             missing_days_isMtl_dataflag = paste(missing,isMtl,dataflag,sep="_")
                      ) %>% 
                      select(date,height,missing_days_isMtl_dataflag,yearfrac)),
         model = map(data, ~tidy(lm(height~yearfrac, data = .))),
         slope = map(model,~.[2,2]),
         p.value = map(model,~.[2,4]),
         n_years = map(data, function(x){
           x %>% 
             summarise(diff = max(date) - min(date)) %>% 
             pull(diff) %>% 
             as.numeric()/365
         }
         ),
         pl = map(data,~ggplot(.,aes(x= date,y = height, label = missing_days_isMtl_dataflag)) + 
                    geom_point() +
                    geom_smooth(method = "lm"))) %>% 
  unnest(c(slope,p.value,n_years)) %>% 
  st_as_sf(coords = c("longitude","latitude"),crs=4326)


m1 <- mapview(df %>% filter(n_years > 50), popup = popupGraph(df[df$n_years > 50,]$pl, type = "png"),zcol = "estimate")
mapshot(m1,"index.html")

df$n_years %>% hist()




filelist %>% 
  slice(1) %>% 
  mutate_all(str_trim) %>%
  mutate(path = paste0("./data/",station_ID ,".rlrdata"),
         data = map(path,~read_delim(.,delim  = ";",col_names = station_colnames) %>% 
                      mutate_all(str_trim) %>% 
                      mutate(date = as.numeric(date),
                             year = floor(date),
                             month = (round((date - year)*24) + 1)/2,
                             date = ymd(paste(year,month,1,sep = "-")),
                             yearfrac = as.numeric(date)/365,
                             height = as.numeric(height),
                             height = ifelse(height == -99999, NA_integer_,height),
                             missing = as.integer(missing),
                             isMtl = as.logical(as.integer(str_sub(isMtl_dataflag,2,2))),
                             dataflag = as.logical(as.integer(str_sub(isMtl_dataflag,3,3))),
                             missing_days_isMtl_dataflag = paste(missing,isMtl,dataflag,sep="_")
                      ) %>% 
                      select(date,height,missing_days_isMtl_dataflag,yearfrac)),
         model = map(data, ~tidy(lm(height~yearfrac, data = .))),
         slope = map(model,~.[2,2]),
         p.value = map(model,~.[2,4]),
         n_years = map(data, function(x){
           x %>% 
             summarise(diff = max(date) - min(date)) %>% 
             pull(diff) %>% 
             as.numeric()/365
         }
         ),
         pl = map(data,~ggplot(.,aes(x= date,y = height, label = missing_days_isMtl_dataflag)) + 
                    geom_point() +
                    geom_smooth(method = "lm"))) %>% 
  unnest(c(slope,p.value,n_years)) %>% 
  st_as_sf(coords = c("longitude","latitude"),crs=4326) %>% 
  select(n_years)
