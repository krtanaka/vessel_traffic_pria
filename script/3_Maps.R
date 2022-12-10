# one for Howland with the previous HARP site: 0.82567 N 176.6448 W
# one for Baker - that indicate the bathymetry and vessel traffic indicators?
# a separate map of each island with bathymetry at the finest resolution (Howland map has the HARP site shown on it),

library(raster)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(ggspatial)
library(colorRamps)

rm(list = ls())

# Howland bathymetry 40m resolution
png("outputs/map_how.png", height = 7, width = 7, res = 500, units = "in")

load("data/bathymetry/bathymetry_0-1000m_how.RData")
bathy = rasterToPoints(bathy) %>% as.data.frame()

harp = data_frame(x = -176.6448,
                  y = 0.82567,
                  date = "4/20/2017-1/28/2018") 

print(ggplot() + 
        geom_raster(data = bathy, aes(x, y, fill = elevation)) + 
        scale_fill_gradientn(colors = matlab.like(100), "Depth (m)") + 
        geom_point(data = harp, aes(x, y, fill = 2), shape = 22, size = 5) + 
        geom_label_repel(data = harp,
                         aes(x, y, label = "HARP site \n\ 4/20/2017 - 1/28/2018"),
                         fontface = 'bold',
                         label.size = 0.1, 
                         alpha = 0.9, 
                         label.padding = 0.5, 
                         box.padding = 2, 
                         point.padding = 0.5,
                         seed = 1993) + 
        annotation_scale(location = "br", width_hint = 0.2) +
        coord_sf(crs = 4326) + 
        scale_x_continuous(sec.axis = dup_axis(), "") +
        scale_y_continuous(sec.axis = dup_axis(), "") +
        theme_minimal() +
        theme(legend.position = c(0.1, 0.15)))

dev.off()


# Baker bathymetry 40m resolution
png("outputs/map_bak.png", height = 7, width = 7, res = 500, units = "in")

load("data/bathymetry/bathymetry_0-1000m_bak.RData")
bathy = rasterToPoints(bathy) %>% as.data.frame()

(ggplot() + 
    geom_raster(data = bathy, aes(x, y, fill = elevation)) + 
    scale_fill_gradientn(colors = matlab.like(100), "Depth (m)") + 
    annotation_scale(location = "br", width_hint = 0.2) +
    coord_sf(crs = 4326) + 
    scale_x_continuous(sec.axis = dup_axis(), "") +
    scale_y_continuous(sec.axis = dup_axis(), "") +
    theme_minimal() +
    theme(legend.position = c(0.9, 0.9)))

dev.off()


# map of the waters surrounding both islands showing vessel movement 
# or a map per vessel indicator with all maps at the same scale so they can be overlaid).

# Phoenix island MPA (https://marineregions.org/gazetteer.php?p=details&id=26866)
mpa = raster("data/mpa/MarineRegions-worldheritagemarineprogramme.tif")
mpa = rasterToPoints(mpa) %>% as.data.frame()
mpa$x = ifelse(mpa$x < 0, mpa$x + 360, mpa$x)
colnames(mpa)[3] = "z"
mpa = mpa %>% subset(z != 255L)
# mpa = mpa %>% subset(z == 153L)
mpa$z = "mpa"

# Island shapefile
load('data/gis_island_boundaries/ncrmp_islands_shp.RData')
crs(ISL_bounds) = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"
ISL_bounds = ISL_bounds[which(ISL_bounds$ISLAND %in% c("BAKER", "HOWLAND")),]

# create island labels
load("data/SURVEY_MASTER.RData")
label = df %>% subset(island %in% c("Howland", "Baker")) %>% group_by(island) %>% summarise(lon = mean(lon), lat = mean(lat))

# GFW daily fleet data
load("data/gfw/fleet_daily_2012-2020.rdata")

df = df %>% 
  mutate(year = lubridate::year(date), 
         month = lubridate::month(date), 
         day = lubridate::day(date)) %>% 
  group_by(cell_ll_lon, cell_ll_lat, geartype) %>% 
  summarise(fishing_hours = median(fishing_hours, na.rm = T),
            hours = median(hours, na.rm = T)) 

# map by geartype
geartypes = unique(df$geartype)

b = marmap::getNOAA.bathy(lon1 = -177.5,
                          lon2 = -175.5,
                          lat1 = -0.5,
                          lat2 = 1.5,
                          resolution = 1)

b = marmap::fortify.bathy(b)

for (g in 1:length(geartypes)) {
  
  # g = 3
  
  df_i = df %>% 
    # subset(hours > 0) %>%
    subset(geartype == geartypes[g])
  
  png(paste0("outputs/gfw_map_", geartypes[g], ".png"), height = 7, width = 7, res = 500, units = "in")
  
  print(
    ggplot() + 
      
      geom_point(data = df_i, aes(cell_ll_lon, cell_ll_lat, color = hours, fill = hours, size = hours), shape = 21, alpha = 0.5) + 
      scale_color_gradientn(colours = matlab.like(10), guide = "legend") +
      scale_fill_gradientn(colours = matlab.like(10), guide = "legend") +
      
      guides(fill = guide_legend(override.aes = list(linetype = 0)),
             color = guide_legend(override.aes = list(linetype = 0))) + 
      
      new_scale_color() +
      new_scale_fill() +
      
      geom_contour(data = b,
                   aes(x = x, y = y, z = z, colour = stat(level)),
                   breaks = seq(-10000, 0, by = 200),
                   size = c(0.2),
                   alpha = 0.5,
                   show.legend = T) +
      
      scale_color_viridis_c("depth_m") + 
      
      geom_path(data = ISL_bounds, aes(long, lat, group = group), inherit.aes = F, size = 0.01, color = "darkgrey") + # coastline
      geom_polygon(data = ISL_bounds, aes(long, lat, group = group), fill = "darkgrey", color = NA, alpha = 0.9) + # land shapefile
      
      geom_label_repel(data = label,
                       aes(x = lon, y = lat, label = island),
                       fontface = 'bold',
                       label.size = 0.1, 
                       alpha = 0.9, 
                       label.padding = 0.5, 
                       box.padding = 0.5, 
                       point.padding = 0.1,
                       seed = 1993) + 
      ggtitle("GFW daily fleet: 2012-2020", subtitle = paste0("Gear Type: ", geartypes[g])) + 
      # geom_raster(data = mpa, aes(x, y), fill = "blue", alpha = 0.3) +
      coord_sf(crs = 4326) +
      scale_x_continuous("", expand = c(0,0)) +
      scale_y_continuous("", expand = c(0,0)) +
      theme_minimal())
  
  dev.off()
  
}
