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

fv = read_csv("G:/GFW/fishing-vessels-v2.csv")

daily_list = list.files("G:/GFW/mmsi-daily_2012_2020/", pattern = ".csv")

load("data/SURVEY_MASTER.RData")
sm = df %>% subset(island %in% c("Howland", "Baker"))
# sm$lon = ifelse(sm$lon < 0, sm$lon + 360, sm$lon)

# df = vector("list")

df = foreach(i = 1:length(daily_list), .combine = rbind, .packages = c('dplyr', 'readr')) %dopar% {
  
  # i = 3000
  
  df_i = read_csv(paste0("G:/GFW/mmsi-daily_2012_2020/", daily_list[i]))
  
  # df_i$cell_ll_lon = ifelse(df_i$cell_ll_lon < 0, df_i$cell_ll_lon + 360, df_i$cell_ll_lon)
  
  df_i = df_i %>% subset(cell_ll_lon > -177.5 & cell_ll_lon < -175.5 & cell_ll_lat > -0.5 & cell_ll_lat < 1.5 )
  
  # plot(df_i$cell_ll_lon, df_i$cell_ll_lat)
  # plot(sm$lon, sm$lat, col = 2)
  
  # df[[i]] = df_i
  df_i
  
}

beepr::beep(2)

plot(df$cell_ll_lon, df$cell_ll_lat)
points(sm$lon, sm$lat, col = 2)

# df = rbindlist(df)

df = merge(df, fv)

annual_fh = vector("list")

for (i in 2012:2020) {
  
  # i = 2020
  
  df_i = df %>% select(names(df)[3:24], paste0("fishing_hours_", i))
  colnames(df_i)[23] = "annual_fishing_hours"
  df_i$year = i
  
  annual_fh[[i]] = df_i
  print(i)

}

annual_fh = rbindlist(annual_fh)

df = annual_fh

save(df, file = "data/gfw/mmsi-daily_2012_2020.rdata")
