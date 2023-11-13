# funcao mosaico
combine_rasters <- function(raster_list) {
  # Check if the raster list is empty
  if (length(raster_list) == 0) {
    stop("No rasters to combine.")
  }
  # Combine rasters using mosaic
  combined_raster <- do.call(mosaic, c(raster_list, fun = min))
  return(combined_raster)
}

# dist_urb

urb_var <- lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/",pattern = "urb_distance",full.names = T),rast)

urban_mos <- combine_rasters(urb_var)

Br <- read_country()%>% st_transform(crs=crs(urb_var[[1]]))
urban_mosc <- crop(urban_mos,Br)
urban_mosm <- mask(urban_mosc,Br)
plot(urban_mosm)

# tranformando em km

urban_mosm <- urban_mosm/1000

writeRaster(urban_mosm,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/urb_distance_Br_km.tif", gdal=c("COMPRESS=DEFLATE"))


# dist veg (faltam alguns biomas!!)


veg_var <- lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/",pattern = "veg_distance",full.names = T),rast)

veg_mos <- combine_rasters(veg_var)

Br <- read_country()%>% st_transform(crs=crs(veg_var[[1]]))
veg_mosc <- crop(veg_mos,Br)
veg_mosm <- mask(veg_mosc,Br)

veg_mosm <- veg_mosm/1000


writeRaster(veg_mosm,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_distance_Br_faltaMAPANT_km.tif", gdal=c("COMPRESS=DEFLATE"))
