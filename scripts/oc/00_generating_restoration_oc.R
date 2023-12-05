# gerar oc a partir de valor da terra, usando taxa de 6% -- sugerido pelo Cadu Young

# pacotes ----------------------------------------------------------------------

library(terra)

#-------------------------------------------------------------------------------

# usar raster antigo, depois substituir

vtn <- rast("/dados/projetos_andamento/custo_oportunidade/rasters_VTN/predicted_VTN_BR_mosaico_v2.tif")

# mascara de veg nat. custo oportunidade restauracao.

veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif")


plot(vtn)
plot(veg)
plot(veg,add=T)

# ajustar extent

vtnpj <- terra::project( vtn, veg,method = 'bilinear')

# convertendo em renda usando uma taxa de 6%

renda <- vtnpj*0.06


# funcao NPV

#npv= sum(renda/(1+r)^t)

npv <- function(renda,r,t){
  sum(renda/((1+r)^t))
}

npv_2050 <- npv(renda = renda,r=0.06,t=1:30)

# salvando oc pra restauracao

writeRaster(npv_2050,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_oc_reais_ha_2050.tif",gdal=c("COMPRESS=DEFLATE"))


