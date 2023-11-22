# fonte dos dados:https://data.mendeley.com/datasets/ybjt9gkzr2/1

# SANO, EDSON (2018), “Data for: Cerrado ecoregions: A spatial framework to assess and prioritize Brazilian savanna environmental diversity for conservation”, Mendeley Data, V1, doi: 10.17632/ybjt9gkzr2.1

library(sf)
library(terra)
library(tidyverse)
library(fasterize)

eco_Cerr <-st_read("/dados/projetos_andamento/TRADEhub/Linha_2/ec/ECORREGIOES_CERRADO_V7.shp") 

length(unique(eco_Cerr$NOME)) # 19 subregioes

dicionario_Cerrado <- data.frame(Nome=unique(eco_Cerr$NOME),ID=unique(eco_Cerr$SEQUENCIA))

# subsetar so a parte do cerrado pra somar depois com o raster com mais detalhes!!

# raster base

base_r <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/ec/ecosystems_2017_reproject.tif")

# dicionario base r

dicionario_global <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/ec/dicionario_ecorregioes_ID.csv")

ID_Cerrado <- filter(dicionario_global,ECO_NAME=="Cerrado")

plot(base_r==ID_Cerrado$ID)

# reprojeta shape
eco_Cerr <- st_transform(eco_Cerr,crs = crs(base_r))%>%
  # transforma coluna de interesse em fator
  mutate(NOME=as.factor(NOME))

# rasterizar ecorregioes cerrado

eco_Cerr_r <- fasterize(sf = eco_Cerr,raster = base_r,field="SEQUENCIA")

par(mfrow=c(1,2))

plot(eco_Cerr_r)
plot(base_r==ID_Cerrado$ID)
plot(base_r)

# # transformar no base, onde tem valor no raster de cerrado em NA

base_r2 <- base_r
 
#base_r2[!is.na(eco_Cerr_r)&base_r2==ID_Cerrado$ID] <- NA
 
# tudo oq era cerrado vira NA. nesse caso, tem q preenchar os nas com a classe de cerrado mais proxima.
base_r2[base_r2==ID_Cerrado$ID] <- NA


plot(base_r2)
plot(base_r)

# transforam em NA, oq for cerrado nas ecorregioes (aqui tem q ser na verdade na onde nao eh No dado anterior)

# base_r3 <- base_r
# 
# base_r3[!is.na(eco_Cerr_r)&base_r3==ID_Cerrado$ID] <- NA
# 
# plot(base_r3)
#plot(base_r2)


# somando os raster substituindo NAs pelos valores das regioes detalhadas do cerrado.

# ficou com uns NAs pq cerrado nao bate exatamente igual pros 2!


#result_raster <- terra::cover(base_r3,eco_Cerr_r,values=NA)
result_raster2 <- terra::cover(base_r2,eco_Cerr_r,values=NA)
result_raster2 <- rast(result_raster2)

#plot(result_raster)
plot(result_raster2)
plot(base_r)
plot(eco_Cerr_r)

# focal pra eliminar NAs (tem q ser janela super grande)

result_raster2_fc <- terra::focal(result_raster2,w=63,fun="modal",na.policy="only",na.rm=T)

plot(result_raster2_fc)
plot(is.na(result_raster2_fc))

plot(result_raster2)

# multiplicar por mascara br pra restabelecer nas corretos

mascara <- rast(base_r/base_r)

result_raster2_fcmsk <- result_raster2_fc*mascara
plot(result_raster2_fc)
plot(result_raster2_fcmsk)
plot(base_r)

# melhor usar result_raster_2

# ficam com alguns pixeis meio fragmentados

# ajustando pixel

r_1km <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes_raster.tif")

result_raster2_fcmskpj <- terra::resample(result_raster2_fcmsk,r_1km,method="mode")

# filtrar ecorregioes mto pequenas

writeRaster(result_raster2_fcmskpj,"/dados/projetos_andamento/TRADEhub/Linha_2/ec/ecoregions_comCerrado.tif",overwrite=T)

# salvando dicionario cerrado. A combinacao com o global fornece o id de todos os pixeis.

write.csv(dicionario_Cerrado,"/dados/projetos_andamento/TRADEhub/Linha_2/ec/dicionario_cerrado_ID.csv",row.names = F)

# escalando pra plotar
nx <- minmax(result_raster2_fcmskpj)  
rn <- (result_raster2_fcmskpj - nx[1,]) / (nx[2,] - nx[1,])

plot(rn)
unique(rn)
