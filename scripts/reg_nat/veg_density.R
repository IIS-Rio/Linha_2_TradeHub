# veg density ~ local forest cover

# usar dados mapbiomas do projeto antigo

p <- "/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020"

veg <- lapply(list.files(p,pattern = paste(c("forest","grassland","wetland","otn"),collapse = "|"),full.names = T),raster)

vegsum <- Reduce("+",veg)

# ajustando celula

# rbase <- brick("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/multiband-2025-low.tif")


# rbase2 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_rasterized/BASE/BASE_CrpLnd2020.tif")

pj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
#crs <- CRS("+init=epsg:31982")
#projection(rbase) <- CRS(crs)

# r base pra rasterizar, vale pra todos

r_base <- raster(extent(vegsum), resolution = c(1000, 1000),crs=pj,vals=0)

veg_final <- resample(vegsum,r_base)

writeRaster(veg_final,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif",format = "GTiff", options = "COMPRESS=DEFLATE")




