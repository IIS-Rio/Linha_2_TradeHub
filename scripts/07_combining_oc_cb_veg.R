df_comb <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_rescaled.csv")


cb <-lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/cb",full.names = T),fread) 

cb2 <- left_join(cb[[1]],cb[[2]])%>%
  # caculando balanco liquido
  mutate(net_CO2=CO2_emited+CO2_sequestered,
         mtc_nm="net_cb.val",
         scen=gsub("_2050","",x=scen),
         ratio="compared2_2020",
         status=if_else(net_CO2>=0,"improved","worsened"))%>%
  dplyr::rename(ecoID=ecoregion_ID,
         scenario=scen,
         value=net_CO2)%>%
  left_join(unique(df_comb[,c(1,3)]))%>%
  filter(!is.na(ecor_nm))


names(df_comb)
names(cb2)[c(1,3,5,6,7,8,9)]

df_comb2 <- rbind(df_comb,cb2[,c(1,3,5,6,7,8,9)])

# continuar; calcular balanco carbono em relacao à 2050;


cb_base <- cb2%>%
  filter(scenario=="baseline")%>%
  dplyr::rename(value_base=value)

cb_otherscens <- cb2%>%
  filter(scenario!="baseline")%>%
  dplyr::rename(value_scens=value) %>%
  left_join(cb_base[,c(3,5)])%>%
  # calculando diferenca com 2050 (pensar se eh subtracao ou divisao)
  mutate(value=value_scens-value_base, 
         ratio="compared2_2050", 
         status=if_else(value_scens>value_base,"improved","worsened"))


# a comparacao com 2050 baseline ta estranha!

summary(cb_base)

# "ecoID"    "mtc_nm"   "ecor_nm"  "status"   "value"    "ratio"    "scenario"

df_comb3 <- rbind(df_comb2,cb_otherscens[,c(1,3,6,7,8,9,11)])

# falta net veg
# calcular area restaurada, area desmatada e net

eco_nms <- unique(cb_base[,c(3,9)])


veg_balance <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/lu_results/net_veg_balance.csv")%>%
  mutate(ratio="compared2_2020",
         scenario=gsub(x = scenario,pattern="_2050",replacement=""),
         status=if_else(net_veg_ha<0,"worsened","improved"))%>%
  left_join(eco_nms)

# comparacao com 2020 ja ta feita, falta com 2050

veg_balance_l <- pivot_longer(veg_balance,cols = c(2,4,5))%>%
  dplyr::rename(mtc_nm=name)



df_comb4 <-rbind(df_comb3,veg_balance_l)


# comparacao com 2050

veg_base <- veg_balance_l%>%
  # so net
  filter(mtc_nm=="net_veg_ha")%>%
  filter(scenario=="baseline")%>%
  dplyr::rename(value_base=value)

veg_otherscens <- veg_balance_l%>%
  filter(scenario!="baseline")%>%
  filter(mtc_nm=="net_veg_ha")%>%
  dplyr::rename(value_scens=value)%>%
  left_join(veg_base[,c(1,6,7)])%>%
  # calculando diferenca com 2050 (pensar se eh subtracao ou divisao)
  mutate(value=value_scens-value_base,
         ratio="compared2_2050",
         status=if_else(value>=0,"improved","worsened"))

# so fiz pra 2050 o net value

df_comb5 <-rbind(df_comb4,veg_otherscens[,c(1:5,6,9)])

# adicionar nova coluna ID  0-60

df_comb5 <- df_comb5 %>%
  mutate(ecoIDseq = dense_rank(ecoID))

# combinando com biomas!!
eco <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shp")%>%
  group_by(ecoID)%>%
  summarise()%>%
  st_make_valid()

biomes <- read_biomes()%>%st_transform(st_crs(eco))

eco_bio <- st_intersection(eco,biomes)

# calculando area

eco_bio$area <- as.numeric(st_area(eco_bio))/10^4


eco_bio_df <- st_drop_geometry(eco_bio)


eco_bio_df <- eco_bio_df %>%
  group_by(ecoID) %>%
  mutate(proportion = area / sum(area))%>%
  filter(!is.na(code_biome),
  # filtrando valores mto baixos
         proportion>0.05) %>%
  group_by(ecoID) %>%
  summarize(Biomes = paste(name_biome, collapse = ", "))
# nao da pra separar em biomas pq as eco pegam mais de 1.

#adicionar

df_comb6 <- left_join(df_comb5,eco_bio_df)

write.csv(df_comb6,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_carbon_lu_results.csv",row.names = F)
