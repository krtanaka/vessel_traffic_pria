# I haven't had time to look through your Git repo for this. I'm super swamped right now leading up to the holidays. Would it be possible for you to make a couple maps -- one for Howland (with the previous HARP site: 00.82567 N 176.6448 W) and one for Baker -- that indicate the bathymetry and vessel traffic indicators? I'm guessing that would actually be a few maps given the difference in scales (how zoomed in on each island) among variables. Whatever is quickest for you is fine -- even if each variable has its own separate map.
# 
# Perhaps something like this: a separate map of each island with bathymetry at the finest resolution (Howland map has the HARP site shown on it), and one map of the waters surrounding both islands showing vessel movement (or a map per vessel indicator with all maps at the same scale so they can be overlaid).

library(raster)
library(dplyr)
library(ggplot2)

rm(list = ls())

load("data/gfw/fleet_daily_2012-2020.rdata")
df = df %>% 
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date)) %>% 
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>% 
  summarise(fishing_hours = mean(fishing_hours, na.rm = T),
            hours = mean(hours, na.rm = T)) %>% 
  subset(fishing_hours > 0) 

# https://marineregions.org/gazetteer.php?p=details&id=26866
mpa = raster("data/mpa/MarineRegions-worldheritagemarineprogramme.tif")
mpa = rasterToPoints(mpa) %>% as.data.frame()
mpa$x = ifelse(mpa$x < 0, mpa$x + 360, mpa$x)
colnames(mpa)[3] = "z"
mpa = mpa %>% subset(z != 255L)
# mpa = mpa %>% subset(z == 153L)
mpa$z = "mpa"

load("data/bathymetry/bathymetry_0-1000m_how.RData")
bathy = rasterToPoints(bathy) %>% as.data.frame()

harp = data_frame(x = -176.6448,
                  y = 0.82567,
                  date = "4/20/2017-1/28/2018") 

ggplot() + 
  geom_raster(data = bathy, aes(x, y, fill = elevation)) + 
  scale_fill_viridis_c("m") + 
  geom_point(data = harp, aes(x, y)) + 
  geom_label_repel(data = harp, aes(x, y, label = "HARP"), fill = alpha(c("red"), 0.8)) 

%>%
  ggplot() + 
  geom_point(aes(cell_ll_lon, cell_ll_lat, color = fishing_hours, fill = fishing_hours, size = fishing_hours), shape = 21, alpha = 0.5) + 
  scale_fill_viridis_c("") + 
  scale_color_viridis_c("") + 
  # geom_raster(data = mpa, aes(x, y), fill = "blue", alpha = 0.3) + 
  facet_wrap(~geartype)
