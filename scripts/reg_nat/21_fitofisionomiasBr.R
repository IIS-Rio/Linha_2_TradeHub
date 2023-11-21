# p <- "/dados/bd_iis/fitofisionomias/"

# metodo antigo
# 
# lista <- list.files(p,pattern ="albers_1000m",recursive = T,full.names = T )
# 
# # subset
# 
# lista_sub <- grep(x=lista,pattern = paste(c("aux.xml","nd"),collapse = "|"),value = T,invert = T)
# 
# fito <- lapply(lista_sub,rast)
# 
# fitona <- list()
# 
# for(i in seq_along(fito)){
#   r <- fito[[i]]
#   r[r==0] <- NA
#   fitona[[i]] <- r
# }
# 
# # mosaicando.
# 
# 
# 
# # Define the function to calculate mode
# f <- function(x)min(x,na.rm = T)
# 
# # Function to combine rasters
# combine_rasters <- function(raster_list) {
#   # Check if the raster list is empty
#   if (length(raster_list) == 0) {
#     stop("No rasters to combine.")
#   }
#   # Combine rasters using mosaic
#   combined_raster <- do.call(terra::mosaic, c(raster_list, fun =min))
#   return(combined_raster)
# }
# 
# 
# fito_mos <- combine_rasters(fitona)

# usando fito agregado

fito <- rast("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/fitofisionomias/fitofisionomias_12cl.tif")

plot(fito)

# adequando pj

rbase <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/crop_density_mollweide_1km.tif") 

fito_mospj <- terra::project(fito,rbase,method="mode")

names(fito_mospj) <- "fitofisionomias_Br"
varnames(fito_mospj) <- "12classes"

unique(fito_mospj)

writeRaster(fito_mospj,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/fitofisionomias.tiff", gdal=c("COMPRESS=DEFLATE"),overwrite=T)
