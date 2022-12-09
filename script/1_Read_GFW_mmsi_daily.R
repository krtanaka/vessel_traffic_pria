library(readr)
library(maps)
library(raster)
library(dplyr)
library(data.table)
library(ggplot2)

rm(list = ls())

fv = read_csv("G:/GFW/fishing-vessels-v2.csv")

daily_list = list.files("G:/GFW/mmsi-daily_2012_2020/", pattern = ".csv")

# https://marineregions.org/gazetteer.php?p=details&id=26866
mpa = raster("data/mpa/MarineRegions-worldheritagemarineprogramme.tif")
mpa = rasterToPoints(mpa) %>% as.data.frame()
mpa$x = ifelse(mpa$x < 0, mpa$x + 360, mpa$x)
colnames(mpa)[3] = "z"

mpa = mpa %>% subset(z != 255L)
# mpa = mpa %>% subset(z == 153L)

mpa$z = "mpa"

ggplot() + 
  geom_raster(data = mpa, aes(x, y, fill = factor(z))) + 
  scale_fill_viridis_d()

load("data/SURVEY_MASTER.RData"); sm = df %>% subset(island %in% c("Howland", "Baker")); sm$lon = ifelse(sm$lon < 0, sm$lon + 360, sm$lon)

df = vector("list")

for (i in 1:length(daily_list)) {
  
  # i = 3000
  
  df_i = read_csv(paste0("G:/GFW/mmsi-daily_2012_2020/", daily_list[i]))
  
  df_i$cell_ll_lon = ifelse(df_i$cell_ll_lon < 0, df_i$cell_ll_lon + 360, df_i$cell_ll_lon)
  
  df_i = df_i %>% subset(cell_ll_lon > 183 & cell_ll_lon < 184 & cell_ll_lat > -1 & cell_ll_lat < 1 )
  
  # plot(df_i$cell_ll_lon, df_i$cell_ll_lat)
  # plot(sm$lon, sm$lat, col = 2)
  
  df[[i]] = df_i
  print(i)
}

df = rbindlist(df)

save(df, file = "data/gfw/mmsi-daily_2012_2020.rdata")

df %>% 
  group_by(cell_ll_lon, cell_ll_lat) %>% 
  summarise(fishing_hours = mean(fishing_hours),
            hours = mean(hours)) %>% 
  # subset(fishing_hours > 0) %>%
  ggplot() + 
  geom_point(aes(cell_ll_lon, cell_ll_lat, color = fishing_hours, fill = fishing_hours, size = fishing_hours), shape = 21) + 
  scale_fill_viridis_c("") + 
  scale_color_viridis_c("") + 
  geom_raster(data = mpa, aes(x, y), fill = "blue", alpha = 0.3)

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


annual_fh %>% 
  # subset(year %in% c(2012, 2020)) %>%
  na.omit() %>%
  subset(annual_fishing_hours > 0) %>% 
  ggplot(aes(cell_ll_lon, cell_ll_lat)) + 
  geom_point(aes(fill = sqrt(annual_fishing_hours),
                 color = sqrt(annual_fishing_hours)), shape = 21, alpha = 0.5) +
  scale_fill_viridis_c("") +
  scale_color_viridis_c("") + 
  geom_raster(data = mpa, aes(x, y), fill = "red", alpha = 0.1) +
  facet_wrap(~year) + 
  coord_fixed()
