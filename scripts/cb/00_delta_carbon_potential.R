# pacotes ----------------------------------------------------------------------

library(terra)


#-------------------------------------------------------------------------------

p1 <- "/dados/projetos_andamento/CB-BR/results/Results_Andre_Junqueira_Nov2023/atualizacao"

delta_rest <- rast(file.path(p1,"delta_agbgC_restor_tonha_v6.1_BR.tif"))
plot(delta_rest)

rbase <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_implementation_cost_reais_ha.tif")

delta_restpj <- project(delta_rest,rbase,"bilinear")

writeRaster(delta_restpj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/delta_agbgC_restor_tonha_v6.1_BR.tif",gdal=c("COMPRESS=DEFLATE"))


# estoque C

AGBatual <- rast("/dados/projetos_andamento/CB-BR/results/Results_Andre_Junqueira_Nov2023/results/AGC_current_updated_v6.1_BR.tif")

BGatual <- rast("/dados/projetos_andamento/CB-BR/results/Results_Andre_Junqueira_Nov2023/results/COMP_FINAL_BG_tonha_2CJ_v6.1_BR.tif")

estoqueatual <- AGBatual+BGatual

estoqueatualpj <- project(estoqueatual,rbase,"bilinear")

plot(estoqueatualpj)

writeRaster(estoqueatualpj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/agbgC_current_tonha_v6.1_BR.tif",gdal=c("COMPRESS=DEFLATE"))

