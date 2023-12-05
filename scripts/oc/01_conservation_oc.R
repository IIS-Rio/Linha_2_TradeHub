# pacotes ----------------------------------------------------------------------

library(terra)

#-------------------------------------------------------------------------------

veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif")


npv_2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_oc_reais_ha_2050.tif")


# calculando oc conservacao:

vegmask <- veg
# usar um limiar: 0.9
vegmask[vegmask==1] <- NA
vegmask[vegmask<1] <- 1

# esse eh o co restauracao
npv_2050_mkd <- npv_2050*vegmask

# o de conservacao pode ser esse completo com os NAS!fazendo focal.

source("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/oc/na_filling_function.R")

# valoe minimo igual ao valor do raster de entrada

cons_oc <- gap.fill(r = npv_2050_mkd,min_threshold = 168 )


writeRaster(cons_oc,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/conservation_oc_reais_ha_2050.tif",gdal=c("COMPRESS=DEFLATE"))

# considerar UCs e TIs no co tanto pra restauracao como conservacao! Acho que vale igualar essas areas a zero! nao tem custo de oportunidade!


