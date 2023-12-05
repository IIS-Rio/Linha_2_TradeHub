library(terra)
# library(sf)
# library(dplyr)


source("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/ec/ecosystem_vulnerability.R")

# areas naturais 2020!
eco_ras <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/ecoregions_wwf_plusCerrado.tif")

nat_ras = rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif")
# nat_ras_pj <- project(nat_ras,eco_ras)
# writeRaster(nat_ras_pj,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)


# areas antropicas 2020
ant_ras = rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/antropic_2020_rsp.tif")
# cria ant ras com projecao correta
# ant_ras = rast("/dados/projetos_andamento/TRADEhub/Linha_2/bd/data/antropic_2020_rsp.tif")
# ant_ras_pj <- project(ant_ras,eco_ras)
# writeRaster(ant_ras_pj,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/antropic_2020_rsp.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)
ec_layer = ecosystem_vulnerability(ecosystem_layer = eco_ras, natural_lc = nat_ras, anthropic_lu = ant_ras)


writeRaster(ec_layer,"/dados/projetos_andamento/TRADEhub/Linha_2/ec/ec_br_2020.tif")
