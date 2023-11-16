library(terra)
library(tidyverse)
library(sf)
library(geobr)
library(foreach)
#library(doParallel)
library(future.apply)

# uco/cobertura

# raster 100m com veg nativa pra 2020, gerado no GEE:

# https://code.earthengine.google.com/?scriptPath=users%2Ffranciscoalbertas%2FIIS%3Anat_veg


veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/NatVegCover_100m.tif")

br <- read_country()%>%
  st_transform(crs(veg))

# recortar mascara brasil
vegc <- crop(veg,br)
vegm <- mask(vegc,br)
# isso tira o exterior como NA, pq o NA eh onde calcula a distancia 
vegm2 <- vegm  
vegm2[is.na(vegm2)] <- 3
# plot(vegm2)  
vegm2[vegm2==0] <-NA 



# criar buffers ao redor dos biomas e calcular a distancia

biomas <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")

# esse buffer precisa ser maior! 5km eh mto pouco, tem q ser 50km

biomas_buffer <- st_buffer(biomas,dist=50000)
biomas_buffer$code_biome <- as.character(biomas_buffer$code_biome)
for(b in biomas_buffer$code_biome){

  r <- filter(biomas_buffer,code_biome==b)
  vegr <- crop(vegm2,r)
  vegm <- crop(vegr,r)
  dist <- terra::distance(vegm)
  # converter pra int
  #integerRaster <- as.integer(round(values(dist)))
  nm <- r$name_biome
  writeRaster(dist,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_distance_",nm,".tif"),overwrite=T)

  }



r <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_distance_Amazônia.tif")
