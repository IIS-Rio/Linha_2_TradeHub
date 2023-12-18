# criar uma coluna pra cada ratio no shapefile de bd. depois eh so corrigir os valores

# analise exploratoria resultado scenarios

# caminho

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/result_tables"

# listando tabelas

lst_tbls <- list.files(p,full.names = T)

# abrindo scenarios

base2020 <-read.csv(lst_tbls[grep("base_2020",lst_tbls)])
base2050 <-read.csv(lst_tbls[grep("base_2050",lst_tbls)])
fcnz <- read.csv(lst_tbls[grep("fcnz",lst_tbls)])
#fcnzplus <- read.csv(lst_tbls[grep("fcnzplus",lst_tbls)])

# filtrando df pra ficar apenas com it, bd e ec 

base_2020_m <- filter(base2020,variable=="metrics")
length(unique(base_2020_m$ecoregion))# 58, faltou ecor 12 e 2, gerar!
base_2050_m <- filter(base2050,variable=="metrics")
length(unique(base_2050_m$ecoregion))
fcnz_m <- filter(fcnz,variable=="metrics")
length(unique(fcnz_m$ecoregion))

# por algum motivo nao tem 2 ecorregioes no no base 2050 e no base 2020, q tem no fcnz_,
unique(fcnz_m$ecoregion)[!unique(fcnz_m$ecoregion) %in% unique(base_2020_m$ecoregion)]


# 12,13,2,68 parece nao ter no base 2020 e base 2050, mas ta erraod, pq tem tabelas
# rever!!!nao tem mesmo no baseline 2020! descobrir pq!
# 13 e 68 tao na lista pra excluir. 12 e 2 nao, mas efetivamente nao formaram resultado! ver pq! a 2 nem criou a pasta. precisa gerar pra essas 2! por enquanto, fazer as analises so com as 2.

# precisa checar se ta fazendo sentido os resultados pra 2020

# filtrando ecorregioes pras 60 corretas
base_2020_m <- base_2020_m[unique(base_2020_m$ecoregion)%in%unique(fcnz_m$ecoregion),]
base_2050_m <- base_2050_m[unique(base_2050_m$ecoregion)%in%unique(fcnz_m$ecoregion),]

base_relative <- left_join(base_2050_m,base_2020_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2020=value.y,value=value.x)%>%
  #limpa na
  filter(!is.na(value_base_2020))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio_2020_base=(value/value_base_2020))


fcnz_relative <- left_join(fcnz_m,base_2020_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2020=value.y,value=value.x)%>%
  #limpa na
  filter(!is.na(value_base_2020))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio_2020_fcnz=(value/value_base_2020))

# tem diferencas ainda no tamanho das tabelas, isso precisa sumir depois q tiver rodado todas!


# abrindo raster ecoregioes

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif") 

ecoregion_vec <- as.polygons(ecoregion)

# espacializando razoes entre 2020 e 2050 --------------------------------------

# associar valores a cada ecorregiao one to many

ecoregion_vec2 <- st_as_sf(ecoregion_vec)
# fcnz
ecoregion_vec2 <- st_as_sf(left_join(fcnz_relative,ecoregion_vec2,by = join_by(ecoregion==focal_modal) ))
# baseline
ecoregion_vec2 <- st_as_sf(left_join(base_relative,ecoregion_vec2,by = join_by(ecoregion,name) ))

# limpando df

var2keep <- c("ecoregion","name","ratio_2020_base","ratio_2020_fcnz")

ecoregion_vec2save <- ecoregion_vec2[,which(names(ecoregion_vec2)%in% var2keep)]

# valores tao estranhos, mto parecidos, rever baseline!! adicionar ratio 2050 tb

fcnz_relative2050 <- left_join(fcnz_m,base_2050_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2050=value.y,value=value.x)%>%
  #limpa na
  filter(!is.na(value_base_2050))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio_2050_fcnz=(value/value_base_2050))

# posicao colunas ecorregion, name e value base 2050: 1,3,8


ecoregion_vec2save <- st_as_sf(left_join(fcnz_relative2050[,c(1,3,8)],ecoregion_vec2save,by = join_by(ecoregion,name) ))

# incluindo nome ecorregiao!

dicionario <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/ec/dicionario_Cerrado_wwf.csv")

ecoregion_vec2save2 <- left_join(ecoregion_vec2save,dicionario,by=join_by(ecoregion==ID))

# ajuste nomes

names(ecoregion_vec2save2)[c(2,6)] <- c("metric_nm","ecor_nm")


st_write(ecoregion_vec2save2,"/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metrics.shp")
