library(fasterize)

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")%>%
  st_transform(crs(r_base))


r_base <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/crop_density_mollweide_1km.tif")


biomes_r <- fasterize(sf = biomes,raster = r_base,field="code_biome")

writeRaster(biomes_r,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes_raster.tif")

