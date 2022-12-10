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

island = c("haw", "bak")

for (i in 1:length(island)) {
  
  if (island[i] == "bak") bathy = raster("data/bathymetry/pibhmc_bathy_40m_baker_7fdc_4612_8e42.nc")
  if (island[i] == "haw") bathy = raster("data/bathymetry/pibhmc_bathy_40m_howland_d150_f94a_6f03.nc")
  
  if(min(values(bathy), na.rm = T) <= 0) {
    
    bathy[bathy <= -1500] <- NA
    bathy[bathy >= 0] <- NA  
    
  }
  
  bathy = readAll(bathy)
  plot(bathy)
  
  if (island[i] == "bak") save(bathy, file = 'data/bathymetry/bathymetry_0-1000m_bak.RData')
  if (island[i] == "haw") save(bathy, file = 'data/bathymetry/bathymetry_0-1000m_how.RData')
  
}
