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
# sm$lon = ifelse(sm$lon < 0, sm$lon + 360, sm$lon)

# df = vector("list")

df = foreach(i = 1:length(daily_list), .combine = rbind, .packages = c('dplyr', 'readr')) %dopar% {
  
  # i = 1000
  
  df_i = read_csv(paste0("G:/GFW/fleet-daily_2012_2020/", daily_list[i]))
  
  # df_i$cell_ll_lon = ifelse(df_i$cell_ll_lon < 0, df_i$cell_ll_lon + 360, df_i$cell_ll_lon)
  
  df_i = df_i %>% subset(cell_ll_lon > -177.5 & cell_ll_lon < -175.5 & cell_ll_lat > -0.5 & cell_ll_lat < 1.5 )
  
  # df[[i]] = df_i
  df_i
  
}

beepr::beep(2)

plot(df$cell_ll_lon, df$cell_ll_lat)
points(sm$lon, sm$lat, col = 2)

# df = rbindlist(df)

save(df, file = "data/gfw/fleet_daily_2012-2020.rdata")
