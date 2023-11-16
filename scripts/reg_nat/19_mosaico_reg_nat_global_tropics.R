library(geobr)
library(tidyverse)
library(sf)
library(terra)


p <- "/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho"

# listando rasters...sao mtos
rs <- lapply(list.files(p,full.names = T,recursive = F)[2:16],rast)

#mask <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes_raster.tif")

mask <- read_country()%>%
  st_transform(crs(rs[[1]]))

#mask <- mask/mask

#plot(mask)


f <- function(x)crop(x,mask)
f2 <- function(x)mask(x,mask)


#testec <- lapply(rs,f)
#testem <- crop(testec,mask)

# Example usage
#testec <- lapply(rs, crop_with_check, mask = mask)

# Create an empty list to store the cropped rasters
# testec <- list()
# 
# # Loop through each raster and attempt to crop
# for (raster in rs) {
#   tryCatch({
#     # Attempt to crop the raster
#     cropped_raster <- crop(raster, mask)
#     
#     # Append the result to the list if successful
#     testec <- c(testec, list(cropped_raster))
#   }, error = function(e) {
#     # Handle the error (e.g., print a message or take other actions)
#     cat("Error:", conditionMessage(e), "\n")
#   })
# }
# 
# # Remove NULL elements from the list (if any)
# testec <- testec[!sapply(testec, is.null)]
# 
# # mascara
# 
# testem <- list()
# 
# # Loop through each raster and attempt to crop
# for (raster in testec) {
#   tryCatch({
#     # Attempt to crop the raster
#     msked_raster <- mask(raster, mask)
#     
#     # Append the result to the list if successful
#     testem <- c(testem, list(msked_raster))
#   }, error = function(e) {
#     # Handle the error (e.g., print a message or take other actions)
#     cat("Error:", conditionMessage(e), "\n")
#   })
# }

# Remove NULL elements from the list (if any)
# testem <- testem[!sapply(testem, is.null)]




# for(i in seq_along(testem)){
#   writeRaster(testem[[i]],paste0("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico/nat_reg","__",i,".tiff"), gdal=c("COMPRESS=DEFLATE"))
# }


testem <- lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico",full.names = T),rast)

rbase <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/NatVegCover_100m.tif")


# Set up a 2x7 plotting layout
#par(mfrow = c(2, 7))

lista_rasters <- list()

for (i in seq_along(testem)) {
  
  r <- testem[[i]]
  r_rs <- terra::resample(x = r,rbase)
  lista_rasters[[i]] <- r_rs
  dir.create("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico/ressampled_100m/")
  writeRaster(r_rs,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico/ressampled_100m/nat_reg","_100m_",i,".tiff"), gdal=c("COMPRESS=DEFLATE"))
  #plot(testem[[i]])
  #plot(mask,add=T)
}

#plot(testem[[1]])
# funcao mosaico (demora mto!)
combine_rasters <- function(raster_list) {
  # Check if the raster list is empty
  if (length(raster_list) == 0) {
    stop("No rasters to combine.")
  }
  # Combine rasters using mosaic
  combined_raster <- do.call(mosaic, c(raster_list, fun = max))
  return(combined_raster)
}
# 
# mosaico dos rasters...
mos <- combine_rasters(lista_rasters)
# 
# 
# merged_raster <- merge(testem[[1]],testem[[2]])
# 
# 
# 
# 
writeRaster(mos,"/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico/ressampled_100m/nat_reg_mos.tiff", gdal=c("COMPRESS=DEFLATE"))
# 
# 
