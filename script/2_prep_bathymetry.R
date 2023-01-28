#########################################################
### Prep island bathymetry data using pibhm GIS files ###
#########################################################

rm(list = ls())

library(dplyr)
library(ggplot2)
library(raster)
library(tidyr)
library(marmap)
library(lattice)
library(readr)

island = c("how", "bak")

for (i in 1:length(island)) {
  
  if (island[i] == "bak") bathy = raster("data/bathymetry/pibhmc_bathy_40m_baker_7fdc_4612_8e42.nc")
  if (island[i] == "how") bathy = raster("data/bathymetry/pibhmc_bathy_40m_howland_d150_f94a_6f03.nc")
  
  if(min(values(bathy), na.rm = T) <= 0) {
    
    bathy[bathy <= -1500] <- NA
    bathy[bathy >= 0] <- NA  
    
  }
  
  bathy = readAll(bathy)
  plot(bathy)
  
  if (island[i] == "bak") save(bathy, file = 'data/bathymetry/bathymetry_0-1000m_bak.RData')
  if (island[i] == "haw") save(bathy, file = 'data/bathymetry/bathymetry_0-1000m_how.RData')
  
  bathy = rasterToPoints(bathy) %>% 
    as.data.frame() %>% 
    subset(elevation > -500) %>% 
    mutate(elevation = ifelse(elevation > -250, "0-249.99 m", "250-500 m"))
  
  if (island[i] == "bak") write_csv(bathy, file = 'data/bathymetry/bathymetry_0-500_bak.csv')
  if (island[i] == "haw") write_csv(bathy, file = 'data/bathymetry/bathymetry_0-500_how.csv')
  
}

# jarvis 

bathy = raster("data/bathymetry/jarvis_20m.asc")

bathy = rasterToPoints(bathy) %>% as.data.frame()

utmcoor <- SpatialPoints(cbind(bathy$x, bathy$y), proj4string = CRS("+proj=utm +units=m +zone=4 +south"))
longlatcoor <- spTransform(utmcoor,CRS("+proj=longlat"))
bathy$x <- coordinates(longlatcoor)[,1]
bathy$y <- coordinates(longlatcoor)[,2]

bathy = bathy %>% 
  mutate(elevation = jarvis_20m) %>% 
  subset(elevation > -500) %>% 
  mutate(elevation = ifelse(elevation > -250, "0-249.99 m", "250-500 m"))

write_csv(bathy, file = 'data/bathymetry/bathymetry_0-500_jar.csv')
