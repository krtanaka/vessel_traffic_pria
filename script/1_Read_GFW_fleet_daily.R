library(readr)
library(maps)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)
library(doParallel)

rm(list = ls())

# cores = detectCores()/2
# registerDoParallel(cores = cores)
registerDoParallel(cores = 10)

# first download GFW files from Google Drive folder and unzip them on your G drive
# https://drive.google.com/drive/folders/13jvlzDvJqhFxCTrrv-osajmosKL6fAmv?usp=sharing

fv = read_csv("G:/GFW/fishing-vessels-v2.csv")

daily_list = list.files("G:/GFW/fleet-daily_2012_2020/", pattern = ".csv")

load("data/SURVEY_MASTER.RData")
sm = df %>% subset(island %in% c("Howland", "Baker"))
sm = df %>% subset(island %in% c("Guam"))

# sm$lon = ifelse(sm$lon < 0, sm$lon + 360, sm$lon)

# df = vector("list")

df = foreach(i = 1:length(daily_list), .combine = rbind, .packages = c('dplyr', 'readr')) %dopar% {
  
  # i = 1000
  
  df_i = read_csv(paste0("G:/GFW/fleet-daily_2012_2020/", daily_list[i]))
  
  # df_i$cell_ll_lon = ifelse(df_i$cell_ll_lon < 0, df_i$cell_ll_lon + 360, df_i$cell_ll_lon)
  
  df_i = df_i %>% subset(cell_ll_lon >= range(pretty(sm$lon))[1]-0.5 &
                           cell_ll_lon <= range(pretty(sm$lon))[2]+0.5 & 
                           cell_ll_lat >= range(pretty(sm$lat))[1]-0.5 &
                           cell_ll_lat <= range(pretty(sm$lat))[2]+0.5 )
  
  # df[[i]] = df_i
  df_i
  
}

beepr::beep(2)

ggplot() + 
  geom_point(data = df, aes(cell_ll_lon, cell_ll_lat, fill = fishing_hours, color = fishing_hours, size = fishing_hours), shape = 21) + 
  scale_color_viridis_c() + 
  scale_fill_viridis_c() + 
  geom_point(data = sm, aes(lon, lat)) + 
  facet_wrap(~geartype) + 
  annotation_map(map_data("world"))

# df = rbindlist(df)

save(df, file = "data/gfw/fleet_daily_2012-2020.rdata")
