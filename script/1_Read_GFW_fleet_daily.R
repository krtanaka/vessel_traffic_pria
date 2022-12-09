library(readr)
library(maps)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)

rm(list = ls())

# first download GFW files from Google Drive folder and unzip them on your G drive
# https://drive.google.com/drive/folders/13jvlzDvJqhFxCTrrv-osajmosKL6fAmv?usp=sharing

fv = read_csv("G:/GFW/fishing-vessels-v2.csv")

daily_list = list.files("G:/GFW/fleet-daily_2012_2020/", pattern = ".csv")

load("data/SURVEY_MASTER.RData")
sm = df %>% subset(island %in% c("Howland", "Baker"))
sm$lon = ifelse(sm$lon < 0, sm$lon + 360, sm$lon)

df = vector("list")

for (i in 1:length(daily_list)) {
  
  # i = 1
  
  df_i = read_csv(paste0("G:/GFW/fleet-daily_2012_2020/", daily_list[i]))
  
  df_i$cell_ll_lon = ifelse(df_i$cell_ll_lon < 0, df_i$cell_ll_lon + 360, df_i$cell_ll_lon)
  
  df_i = df_i %>% subset(cell_ll_lon > 183 & cell_ll_lon < 184 & cell_ll_lat > -1 & cell_ll_lat < 1 )
  
  # plot(df_i$cell_ll_lon, df_i$cell_ll_lat)
  # points(sm$lon, sm$lat, col = 2)

  df[[i]] = df_i
  print(i)
  
}

df = rbindlist(df)

save(df, file = "data/gfw/fleet_daily_2012-2020.rdata")
