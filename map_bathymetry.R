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

bathy = raster("data/bathymetry/pibhmc_bathy_40m_baker_7fdc_4612_8e42.nc")
bathy = raster("data/bathymetry/pibhmc_bathy_40m_howland_d150_f94a_6f03.nc")

if(min(values(bathy), na.rm = T) <= 0) {
  
  bathy[bathy <= -1000] <- NA
  bathy[bathy >= 0] <- NA  
  
}

bathy = readAll(bathy)
plot(bathy)

save(bathy, file = 'data/bathymetry/bathymetry_0-1000m_bak.RData')
save(bathy, file = 'data/bathymetry/bathymetry_0-1000m_how.RData')

levelplot(bathy)

wireframe(unclass(as.bathy(bathy)), 
          shade = T,
          aspect = c(dim(bathy)[1]/dim(bathy)[2], 0.02),
          par.box = c(col = "transparent"),
          scales = list(arrows = FALSE, col = "transparent"), # col="black" is required
          par.settings = list(axis.line = list(col = 'transparent')),
          light.source = c(10,0,10),
          zlab = "",
          xlab = "",
          ylab = "",
          perspective = T,
          screen = list(z = 30, x = -60, y = 10),
          zoom = 3)
