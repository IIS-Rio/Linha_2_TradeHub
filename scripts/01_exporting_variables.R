library(terra)

# acho q pode ser interessante usar ecorregioes nao so pra calcular vulnerabilidade, mas tb pra calcular o it e o bd.

ecorregioes <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/ec/ecoregions_comCerrado.tif")


writeRaster(ecorregioes,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/ecoregions_wwf_plusCerrado.tif",gdal=("COMPRESS=DEFLATE"))

impl_cost <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/custos_restauracao/custos_restauracao_2023_reais_ha.tiff")

writeRaster(impl_cost,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_implementation_cost_reais_ha.tif",gdal=("COMPRESS=DEFLATE"))

# current lus ------------------------------------------------------------------

# baseado no Mapbiomas

lst_lus <- rast(list.files("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/rawdata/land-use",pattern = "1km.tif",full.names = T)) # 4=ingored

# 6 pixeis dao valor =2. tira o ignored resolve isso.

rsum <-sum(lst_lus)

zero_count <- freq(rsum,0)# 185 pixels = 0

# add to ignored

lst_lus[[4]][rsum==0] <- 1

rsum2 <-sum(lst_lus)

rsum2df <- as.data.frame(rsum2)
rsum2dfbelow0 <- rsum2df%>%
  mutate(sum=round(sum,4))%>%
  filter(sum<1)


rtocomplete <- 1-rsum2

rsum3 <- rsum2+rtocomplete # ok

ign <- lst_lus[[4]]+ rtocomplete

lst_lus[[4]] <- ign

rsum4 <-sum(lst_lus) # ok

# reclassificar nas classes adequadas

agri <- lst_lus[[1]]+lst_lus[[7]]
natveg <- lst_lus[[5]]+lst_lus[[2]]+lst_lus[[8]]+lst_lus[[3]]
pastureland <- lst_lus[[6]]
other <- lst_lus[[4]]

# ajustar pj

rbase <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2025/base/downscaled_base_2025_agriculture.tif")

agripj <- project(agri,rbase)
natvegpj <- project(natveg,rbase)
pasturelandgpj <- project(pastureland,rbase)
otherpj <- project(other,rbase)


final_check <- round(agripj+natvegpj+pasturelandgpj+otherpj,4)#ok arredondando

writeRaster(agripj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/agriculture.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)
writeRaster(natvegpj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/natveg.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)
writeRaster(pasturelandgpj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/pastureland.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)
writeRaster(otherpj,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/other.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)

# hfi --------------------------------------------------------------------------

hfi <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2050_ssp2.tif")
hfi2020 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2020.tif")

crs(hfi)==crs(otherpj)# ok

writeRaster(hfi,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/hfi_br_2050_ssp2.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)

writeRaster(hfi2020,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/hfi_br_2020.tif",gdal=c("COMPRESS-DEFLATE"),overwrite=T)




