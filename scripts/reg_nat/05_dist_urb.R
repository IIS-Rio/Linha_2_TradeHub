library(terra)
library(tidyverse)
library(sf)
library(geobr)

library(foreach)
library(doParallel)

# uso/cobertura

# https://code.earthengine.google.com/?scriptPath=users%2Ffranciscoalbertas%2FIIS%3Anat_veg


urb <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/area_urbana_Brasil_2020.tif")
veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/NatVegCover_100m.tif")

urb_res <- resample(urb,veg)

plot(urb_res)

br <- read_country()%>%
  st_transform(crs(urb))

# recortar mascara brasil
urbc <- crop(urb_res,br)
urbm <- mask(urbc,br)
# isso tira o exterior como NA, pq o NA eh onde calcula a distancia 
urbm2 <- urbm  
urbm2[is.na(urbm2)] <- 3
# plot(vegm2)  
urbm2[urbm2==0] <-NA 
plot(urbm2)

# criar buffers ao redor dos biomas e calcular a distancia

biomas <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")

# esse buffer precisa ser maior! 5km eh mto pouco, tem q ser 50km

biomas_buffer <- st_buffer(biomas,dist=50000)

biomas_buffer$code_biome <- as.character(biomas_buffer$code_biome)

for(b in biomas_buffer$code_biome){
  
  r <- filter(biomas_buffer,code_biome==b)
  urbr <- crop(urbm2,r)
  urbm <- crop(urbr,r)
  dist <- terra::distance(urbm)
  # converter pra int
  #integerRaster <- as.integer(round(values(dist)))
  nm <- r$name_biome
  writeRaster(dist,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/urb_distance_",nm,".tif"),overwrite=T)
  
}



