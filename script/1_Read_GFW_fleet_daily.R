library(readr)
library(maps)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)

rm(list = ls())

# first download GFW files from Google Drive folder
# https://drive.google.com/drive/folders/13jvlzDvJqhFxCTrrv-osajmosKL6fAmv?usp=sharing

fv = read_csv("G:/GFW/fishing-vessels-v2.csv")

daily_list = list.files("G:/GFW/fleet-daily-csvs-100-v2-2020/", pattern = ".csv")

# https://marineregions.org/gazetteer.php?p=details&id=26866
mpa = raster("data/MarineRegions-worldheritagemarineprogramme.tif")
mpa = rasterToPoints(mpa) %>% as.data.frame()
mpa$x = ifelse(mpa$x < 0, mpa$x + 360, mpa$x)
colnames(mpa)[3] = "z"

mpa = mpa %>% subset(z != 255L)
# mpa = mpa %>% subset(z == 153L)

mpa$z = "mpa"

ggplot() + 
  geom_raster(data = mpa, aes(x, y, fill = factor(z))) + 
  scale_fill_viridis_d()

df = vector("list")

for (i in 1:length(daily_list)) {
  
  # i = 1
  
  df_i = read_csv(paste0("G:/GFW/fleet-daily-csvs-100-v2-2020/", daily_list[i]))
  
  df_i$cell_ll_lon = ifelse(df_i$cell_ll_lon < 0, df_i$cell_ll_lon + 360, df_i$cell_ll_lon)
  
  df_i = df_i %>% subset(cell_ll_lon > 183 & cell_ll_lon < 192 & cell_ll_lat > -8 & cell_ll_lat < 1 )
  
  df[[i]] = df_i
  print(i)
}

df = rbindlist(df)

df %>% 
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date)) %>% 
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>% 
  summarise(fishing_hours = mean(fishing_hours, na.rm = T),
            hours = mean(hours, na.rm = T)) %>% 
  subset(fishing_hours > 0) %>%
  ggplot() + 
  geom_point(aes(cell_ll_lon, cell_ll_lat, color = fishing_hours, fill = fishing_hours, size = fishing_hours), shape = 21, alpha = 0.5) + 
  scale_fill_viridis_c("") + 
  scale_color_viridis_c("") + 
  geom_raster(data = mpa, aes(x, y), fill = "blue", alpha = 0.3) + 
  facet_wrap(~geartype)
