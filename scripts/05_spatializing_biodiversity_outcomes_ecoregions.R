# OBS---------------------------------------------------------------------------
# criar uma coluna pra cada ratio no shapefile de bd. depois eh so corrigir os valores

# logica de todas as metricas. O denominador eh o cenario de referencia logo valores <1 indicam que o denominador eh maior que o cenario q esta sendo comparado. Porem, tem valores negativos e positivos: qndo eh positivo mas o valor absoluto do denominador eh maior, isso indica q houve melhora. se for menor eh piora.

# qndo da negativo e o valor do denominador eh menor = piora
# qndo da positivo e o valor do denominador eh maior = melhora

# criar uma tabela indicando se houve melhora ou piora

#------------------------------------------------------------------------------

# caminho

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/result_tables"

# listando tabelas

lst_tbls <- list.files(p,full.names = T)

# abrindo scenarios

base2020 <-read.csv(lst_tbls[grep("base_2020",lst_tbls)])
base2050 <-read.csv(lst_tbls[grep("base_2050",lst_tbls)])
fcnz <- read.csv(lst_tbls[grep("fcnz.csv",lst_tbls)])
fcnzplus <- read.csv(lst_tbls[grep("fcnzplus",lst_tbls)])

# filtrando df pra ficar apenas com it, bd e ec 

base_2020_m <- filter(base2020,variable=="metrics")
length(unique(base_2020_m$ecoregion))
base_2050_m <- filter(base2050,variable=="metrics")
length(unique(base_2050_m$ecoregion))
fcnz_m <- filter(fcnz,variable=="metrics")
length(unique(fcnz_m$ecoregion))
fcnzplus_m <- filter(fcnzplus,variable=="metrics")
length(unique(fcnzplus_m$ecoregion))

# rever 12 e 2:  nao formaram resultado! ver pq! a 2 nem criou a pasta. precisa gerar pra essas 2! 

# calculando valores relativos a 2020

base_relative <- base_2050_m%>%
  rename(valuebase2050=value)%>%
  left_join(base_2020_m[,c(1,3,8)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2020=value_2020)%>%
  #limpa na
  filter(!is.na(value_base_2020))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio20base=(valuebase2050/value_base_2020))


summary(base_relative)
length(unique(base_relative$ecoregion)) 
head(base_relative)

# pra excluir
#excluir <- c(13,257,266,68,208,677,326) 
# vou ter q recalcular metricas pra regiao 102(it parece gerar problemas)

fcnz_relative <- fcnz_m%>%
  rename(valuefcnz2050=value)%>%
  left_join(base_2020_m[,c(1,3,8)],by=c("ecoregion","name"))%>%
  rename(value_base_2020=value_2020)%>%
  #limpa na
  filter(!is.na(value_base_2020))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio_2020_fcnz=(valuefcnz2050/value_base_2020))%>%
  #rename(valuefcnz=value)%>%
  # adicionando valor 2050
  left_join(base_2050_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  rename(value_base_2050=value)%>%
  # calculating ratio
  mutate(ratio2050_fcnz=(valuefcnz2050/value_base_2050))#%>%
  #rename(valuebase2050=value)


length(unique(fcnz_relative$ecoregion)) # 60

fcnzplus_relative <- fcnzplus_m%>%
  rename(valuefcnzplus2050=value)%>%
  left_join(base_2020_m[,c(1,3,8)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2020=value_2020)%>%
  #limpa na
  filter(!is.na(value_base_2020))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio_2020_fcnzplus=(valuefcnzplus2050/value_base_2020))%>%
  #rename(valuefcnzplus=value)%>%
  # adicionando valor 2050
  left_join(base_2050_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  rename(valuebase2050=value)%>%
  # calculating ratio
  mutate(ratio2050_fcnzplus=(valuefcnzplus2050/valuebase2050))
  

summary(fcnzplus_relative)
length(unique(fcnzplus_relative$ecoregion)) # 60!

# abrindo raster ecoregioes

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif") 

ecoregion_vec <- as.polygons(ecoregion)

# espacializando razoes entre 2020 e 2050 --------------------------------------


ecoregion_vec2 <- st_as_sf(ecoregion_vec)

# baseline

ecoregion_vec2 <- st_as_sf(left_join(base_relative,ecoregion_vec2,by = join_by(ecoregion==focal_modal) ))

summary(ecoregion_vec2)

# fcnz (enquanto nao tiver completo, isso aqui vai continuar reduzindo n linhas!)

ecoregion_vec2 <- st_as_sf(left_join(fcnz_relative,ecoregion_vec2,by = join_by(ecoregion,name,year,variable,value_base_2020) ))%>%
  # elimina colunas com nome scenario
  select(-starts_with("scenario"))

summary(ecoregion_vec2)

# fcnz plus


ecoregion_vec2 <- st_as_sf(left_join(ecoregion_vec2,fcnzplus_relative,by = join_by(ecoregion,name,variable,year,value_base_2020,valuebase2050) ))

# limpando df

summary(ecoregion_vec2)

# var2keep <- c("value_base_2020","valuebase50","ecoregion","name","ratio_2020_base","ratio_2020_fcnz","ratio_2020_fcnzplus")

var2keep <- c("valuefcnz2050","value_base_2020","ratio_2020_fcnz","value_base_2050","ratio2050_fcnz","ratio20base","valuefcnzplus2050","ratio_2020_fcnzplus","ratio2050_fcnzplus","ecoregion","name")

#,"valuefcnzplus","valuefcnz"

ecoregion_vec2save <- ecoregion_vec2[,which(names(ecoregion_vec2)%in% var2keep)]

summary(ecoregion_vec2save)#ok


# incluindo nome ecorregiao! --------------------------------------------------

dicionario <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/ec/dicionario_Cerrado_wwf.csv")

ecoregion_vec2save <- left_join(ecoregion_vec2save,dicionario,by=join_by(ecoregion==ID))

# ajuste nomes

names(ecoregion_vec2save)[c(2,12)] <- c("metric_nm","ecor_nm")


# ajustando nomes pra salavar


# names(ecoregion_vec2save) <- c("ecoID","mtc_nm","vlfcnz","vlbse20","rt20fcnz","vlbse50" ,"rt50fcnz","rt20bse","vlfcnzplus","rt20fcnzplus","rt50fcnzplus","eco_nm","geometry" ) 


# vl base 20, vlbs50, rt20bse
# rtfcnz20,rt50fcnz,vlfcnz50
# vlfcnzpls50,rt20fcnzpls, rt50fcnzpls


names(ecoregion_vec2save) <- c("ecoID","mtc_nm","vlfcnz50","vlbse20","rt20fcnz","vlbse50","rt50fcnz","rt20bse","vlfcnzpls50","rt20fcnzpls","rt50fcnzpls","ecor_nm","geometry")   


st_write(ecoregion_vec2save,"/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv04.shp",append=FALSE)


