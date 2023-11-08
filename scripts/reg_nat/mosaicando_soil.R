# mosaico dados de densidade carbono

profundidades <- c("0-5cm","5-15cm","15-30cm")

# listando dados por profundiades(sao diferentes, baixei de formas distintas)

prof_0_5 <- lapply(X = list.files("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc",pattern = profundidades[1],full.names = T),raster)

prof_5_15 <- lapply(X = list.files("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc",pattern = profundidades[2],full.names = T),raster)

prof_15_30 <- lapply(X = list.files("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc",pattern = profundidades[3],full.names = T),raster)

# funcao mosaico
combine_rasters <- function(raster_list) {
  # Check if the raster list is empty
  if (length(raster_list) == 0) {
    stop("No rasters to combine.")
  }
  # Combine rasters using mosaic
  combined_raster <- do.call(mosaic, c(raster_list, fun = mean))
  return(combined_raster)
}

combined_0_5 <- combine_rasters(prof_0_5)
combined_5_15 <- combine_rasters(prof_5_15)
combined_15_30 <- combine_rasters(prof_15_30)

# mascara br

Br <- read_country()%>%
  st_transform(crs = "+proj=igh +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs ")

combined_0_5c <- crop(combined_0_5,Br)
combined_0_5m <- mask(combined_0_5c,Br)

mascara <- combined_0_5m/combined_0_5m

# multiplicar por raster ao invés de fazer mascara
combined_5_15m <- combined_5_15*mascara
combined_15_30m <- combined_15_30*mascara


plot(combined_0_5m)
plot(combined_5_15m)
plot(combined_15_30m)

plot(st_geometry(Br),add=T)

writeRaster(combined_0_5m, filename ="/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/mosaico/soc_0_5cm_BR.tif" , format = "GTiff", options = "COMPRESS=DEFLATE")

writeRaster(combined_5_15m, filename ="/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/mosaico/soc_5_15cm_BR.tif" , format = "GTiff", options = "COMPRESS=DEFLATE")

writeRaster(combined_15_30m, filename ="/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/mosaico/soc_15_30cm_BR.tif" , format = "GTiff", options = "COMPRESS=DEFLATE")
