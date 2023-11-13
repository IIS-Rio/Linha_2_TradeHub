# veg density ~ local forest cover

# usar dados mapbiomas do projeto antigo

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020"

crop <- rast(list.files(p,pattern = paste(c("cropland"),collapse = "|"),full.names = T))

# ajustando celula

# rbase <- brick("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/multiband-2025-low.tif")


# rbase2 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_rasterized/BASE/BASE_CrpLnd2020.tif")

pj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#crs <- CRS("+init=epsg:31982")
#projection(rbase) <- CRS(crs)

# r base pra rasterizar, vale pra todos



r_base <- rast(terra::ext(crop), resolution = c(1000, 1000),crs=pj,vals=0)

crop_final <- resample(crop,r_base)

writeRaster(crop_final,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/crop_density_mollweide_1km.tif",gdal=c("COMPRESS=DEFLATE"))




