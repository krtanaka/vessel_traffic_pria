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



levelplot(bathy)

library(rayshader)
unlink(bathy)

#And convert it to a matrix:
elmat = raster_to_matrix(bathy)

elmat %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(elmat), color = "desert") %>%
  add_shadow(ray_shade(elmat, zscale = 3), 0.5) %>%
  add_shadow(ambient_shade(elmat), 0) %>%
  plot_3d(elmat, zscale = 10, fov = 0, theta = 135, zoom = 0.75, phi = 45, windowsize = c(1000, 800))
Sys.sleep(0.2)
render_snapshot()

render_camera(fov = 0, theta = 60, zoom = 0.75, phi = 45)
render_scalebar(limits=c(0, 5, 10),label_unit = "km",position = "W", y=50,
                scale_length = c(0.33,1))
render_compass(position = "E")
render_snapshot(clear=TRUE)

montereybay %>% 
  sphere_shade(zscale = 10, texture = "imhof1") %>% 
  add_shadow(montshadow, 0.5) %>%
  add_shadow(montamb,0) %>%
  plot_3d(montereybay, zscale = 50, fov = 0, theta = -100, phi = 30, windowsize = c(1000, 800), zoom = 0.6,
          water = TRUE, waterdepth = 0, waterlinecolor = "white", waterlinealpha = 0.5,
          wateralpha = 0.5, watercolor = "lightblue")
render_label(montereybay, x = 350, y = 160, z = 1000, zscale = 50,
             text = "Moss Landing", textsize = 2, linewidth = 5)
render_label(montereybay, x = 220, y = 70, z = 7000, zscale = 50,
             text = "Santa Cruz", textcolor = "darkred", linecolor = "darkred",
             textsize = 2, linewidth = 5)
render_label(montereybay, x = 300, y = 270, z = 4000, zscale = 50,
             text = "Monterey", dashed = TRUE, textsize = 2, linewidth = 5)
render_label(montereybay, x = 50, y = 270, z = 1000, zscale = 50,  textcolor = "white", linecolor = "white",
             text = "Monterey Canyon", relativez = FALSE, textsize = 2, linewidth = 5) 
Sys.sleep(0.2)
render_snapshot(clear=TRUE)
