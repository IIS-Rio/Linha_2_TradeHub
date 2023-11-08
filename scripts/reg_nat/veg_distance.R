library(terra)

# uco/cobertura

# raster 100m com veg nativa pra 2020

veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/NatVegCover_100m.tif")

veg[veg==0] <- NA

#plot(veg)

# Calculate the Euclidean distance raster

euclidean_distance <- terra::distance(veg)



writeRaster(euclidean_distance,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_distance.tif",format = "GTiff", options = "COMPRESS=DEFLATE")
