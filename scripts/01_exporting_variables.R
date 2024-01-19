library(terra)

# acho q pode ser interessante usar ecorregioes nao so pra calcular vulnerabilidade, mas tb pra calcular o it e o bd.

ecorregioes <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/ec/ecoregions_comCerrado.tif")

baserast <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/agriculture.tif")

ecorregioespj <- project(ecorregioes,baserast,"near")

writeRaster(ecorregioespj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/ecoregions_wwf_plusCerrado.tif",gdal=("COMPRESS=DEFLATE"),overwrite=T)

impl_cost <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/custos_restauracao/custos_restauracao_2023_reais_ha.tiff")

impl_costpj <- project(impl_cost,baserast)
plot(impl_costpj)

writeRaster(impl_costpj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_implementation_cost_reais_ha.tif",gdal=("COMPRESS=DEFLATE"),overwrite=T)


# hfi --------------------------------------------------------------------------

hfi <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2050_ssp2.tif")
hfi2020 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2020.tif")

hfipj <- project(hfi,baserast)
hfi2020pj <- project(hfi2020,baserast)

writeRaster(hfipj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/hfi_br_2050_ssp2.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)


writeRaster(hfi2020pj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/hfi_br_2020.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)




