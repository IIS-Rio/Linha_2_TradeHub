# pacotes ----------------------------------------------------------------------
#library(scales)
#library(ggpubr)
#library(ggmap)
#library(RColorBrewer)
#library(sf)
#library(geobr)
#library(ggthemes)
library(tidyr)
library(data.table)
#-------------------------------------------------------------------------------

# resultados biodiv

bio_tbl <- do.call(rbind,lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables",full.names = T),fread))%>%
  rename(ecoregion_ID=ecoregion)

# carbon

cb_tbl <-lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/cb",full.names = T),fread)

cb_em <- cb_tbl[[1]]%>%
  pivot_longer(2)%>%
  mutate(variable="carbon balance",
         year=2050)%>%
  rename(scenario=scen)

cb_seq <- cb_tbl[[2]]%>%
  pivot_longer(2)%>%
  mutate(variable="carbon balance",
         year=2050)%>%
  rename(scenario=scen)

# calculando net carbon

net_cb <- cb_em %>%
  rename(emited=value)%>%
  left_join(cb_seq[,c(1,2,4)],by=c("scenario","ecoregion_ID"))%>%
  rename(seq=value)%>%
  mutate(value=emited-seq,
         name="net_CO2")


final_tbl <- rbind(bio_tbl,cb_seq,cb_em,net_cb[,c(1:3,5,6,8)])
summary(bio_tbl$value)

# normalizando valores metricas (mantendo todos e eliminando valores)

bio_tbl_sc <- bio_tbl %>%
  # Remove extreme values for ratio 2050/base 2050
  mutate(value_skewed = if_else(value < -11.189, -11, if_else(value > 1, 1, value)),
         value_sc = if_else(value_skewed == -11, -1,
                            (value_skewed - min(value_skewed)) / (max(value_skewed) - min(value_skewed)) * 2 - 1)
  )

hist(bio_tbl_sc$value)
hist(bio_tbl_sc$value_sc)
hist(bio_tbl_sc$value_skewed)

# fcnz <- fcnz%>%
#   # adiciona coluna falando se piorou ou melhorou
#   mutate(status20=if_else(vlfcnz50>vlbse20,"improved","worsened"),
#          status50=if_else(vlfcnz50>vlbse50,"improved","worsened"),
#          # elimina valores extremos pra plotar: ratio 2050/2020
#          #         rt20fcnz = if_else(rt20fcnz < -2, -2, if_else(rt20fcnz > 2, 2, rt20fcnz)),
#          # elimina valores extremos ratio 2050/base 2050
#          #         rt50fcnz = if_else(rt50fcnz < -2, -2, if_else(rt50fcnz > 2, 2, rt50fcnz))
#   )%>%
#   mutate(
#     # reescala valores pra plotar entre -1 e 1 pra razaao 2020 (substitui min e max do proprio cenario pelos maximos e minimos totais!)
#     ratio_sc_20 = case_when(
#       status20 == "improved" ~ (rt20fcnz - minmax2020[1] ) / (minmax2020[2] - minmax2020[1]) , #min(rt20fcnz), max(rt20fcnz) min(rt20fcnz)
#       status20 == "worsened" ~ (rt20fcnz - minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
#       TRUE ~ NA_real_),
#     # reescala valores pra plotar entre -1 e 1 pra razaao 2050
#     ratio_sc_50 = case_when(
#       status50 == "improved" ~ (rt50fcnz - minmax2050[1]) / (minmax2050[2] -minmax2050[1]) ,
#       status50 == "worsened" ~ (rt50fcnz - minmax2050[1]) / (minmax2050[2] - minmax2050[1])* -1,
#       TRUE ~ NA_real_))


# shape com resultados razao ja calculada.

# ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv04.shp")

# names(ecoregions) <- c("ecoID","mtc_nm","vlfcnz50","vlbse20","rt20fcnz","vlbse50","rt50fcnz","rt20bse","vlfcnzpls50","rt20fcnzpls","rt50fcnzpls","ecor_nm","geometry") 

# base <- ecoregions%>%select_at(c(1,2,4,6,8,12))
# fcnz <- ecoregions%>%select_at(c(1,2,3,4,5,6,7,12))
# fcnplus <- ecoregions%>%select_at(c(1,2,4,6,9:12))
# 

# fcnz ------------------------------------------------------------------------


# OBS. Pras cores serem comparaveis, precisam estar todos na mesma escala. dividir pelos maiores e menores valores gerais
# 
# ecoregions_long_2020 <- pivot_longer(st_drop_geometry(ecoregions),cols = c(5,8,10))
# ecoregions_long_2050 <- pivot_longer(st_drop_geometry(ecoregions),cols = c(7,11))
# 
# minmax2020 <- range(ecoregions_long_2020$value)
# minmax2050 <- range(ecoregions_long_2050$value)
# 
# 
# fcnz <- fcnz%>%
#   # adiciona coluna falando se piorou ou melhorou
#   mutate(status20=if_else(vlfcnz50>vlbse20,"improved","worsened"),
#          status50=if_else(vlfcnz50>vlbse50,"improved","worsened"),
#          # elimina valores extremos pra plotar: ratio 2050/2020
#          #         rt20fcnz = if_else(rt20fcnz < -2, -2, if_else(rt20fcnz > 2, 2, rt20fcnz)),
#          # elimina valores extremos ratio 2050/base 2050
#          #         rt50fcnz = if_else(rt50fcnz < -2, -2, if_else(rt50fcnz > 2, 2, rt50fcnz))
#   )%>%
#   mutate(
#     # reescala valores pra plotar entre -1 e 1 pra razaao 2020 (substitui min e max do proprio cenario pelos maximos e minimos totais!)
#     ratio_sc_20 = case_when(
#       status20 == "improved" ~ (rt20fcnz - minmax2020[1] ) / (minmax2020[2] - minmax2020[1]) , #min(rt20fcnz), max(rt20fcnz) min(rt20fcnz)
#       status20 == "worsened" ~ (rt20fcnz - minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
#       TRUE ~ NA_real_),
#     # reescala valores pra plotar entre -1 e 1 pra razaao 2050
#     ratio_sc_50 = case_when(
#       status50 == "improved" ~ (rt50fcnz - minmax2050[1]) / (minmax2050[2] -minmax2050[1]) ,
#       status50 == "worsened" ~ (rt50fcnz - minmax2050[1]) / (minmax2050[2] - minmax2050[1])* -1,
#       TRUE ~ NA_real_))
# 
# # eliminando parte espacial
# 
# fcnz <- st_drop_geometry(fcnz)
# 
# # fcnzplus --------------------------------------------------------------------
# 
# 
# fcnplus <- fcnplus%>%
#   filter_all(all_vars(!is.na(.)))%>%
#   mutate(status20=if_else(vlfcnzpls50>vlbse20,"improved","worsened"),
#          status50=if_else(vlfcnzpls50>vlbse50,"improved","worsened"),
#          # elimina valores extremos pra plotar!!
#          #         rt20fcnzpls = if_else(rt20fcnzpls < -2, -2, if_else(rt20fcnzpls > 2, 2, rt20fcnzpls)),
#          #         rt50fcnzpls = if_else(rt50fcnzpls < -2, -2, if_else(rt50fcnzpls > 2, 2, rt50fcnzpls)
#          #         )
#   )%>%
#   mutate(ratio_sc_20 = case_when(
#     status20 == "improved" ~ (rt20fcnzpls - minmax2020[1]) / (minmax2020[2] - minmax2020[1]) ,
#     status20 == "worsened" ~ (rt20fcnzpls -minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
#     TRUE ~ NA_real_),
#     ratio_sc_50 = case_when(
#       status50 == "improved" ~ (rt50fcnzpls - minmax2050[1]) / (minmax2050[2] - minmax2050[1]) ,
#       status50 == "worsened" ~ (rt50fcnzpls - minmax2050[1]) / (minmax2050[2] - minmax2050[1])* -1,
#       TRUE ~ NA_real_))
# 
# 
# fcnplus <- st_drop_geometry(fcnplus)
# 
# # base ------------------------------------------------------------------------
# 
# 
# base <- base%>%
#   filter_all(all_vars(!is.na(.)))%>%
#   mutate(status=if_else(vlbse50>vlbse20,"improved","worsened"),
#          # elimina valores extremos pra plotar!!
#          rt20fcnzpls = if_else(rt20bse < -2, -2, if_else(rt20bse > 2, 2, rt20bse)))%>%
#   mutate(ratio_sc = case_when(
#     status == "improved" ~ (rt20bse - minmax2020[1]) / (minmax2020[2] - minmax2020[1]) ,
#     status == "worsened" ~ (rt20bse - minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
#     TRUE ~ NA_real_))
# 
# base <- st_drop_geometry(base)
# 
# # convertendo em long
# 
# base_long_20 <- pivot_longer(base,cols =c(9) )%>% 
#   dplyr::select(c(1,2,6,7,10))%>%
#   mutate(ratio="compared2_2020",
#          scenario="baseline"
#   )
# 
# 
# fcnz_long_20 <- pivot_longer(fcnz,cols =c(11) )%>% 
#   dplyr::select(c(1,2,8,9,13))%>%
#   mutate(ratio="compared2_2020",
#          scenario = "fcnz"
#   )%>%
#   rename(status=status20)
# 
# 
# fcnz_long_50 <- pivot_longer(fcnz,cols =c(12) )%>% 
#   dplyr::select(c(1,2,8,10,13))%>%
#   mutate(ratio="compared2_2050",
#          scenario="fcnz"
#   )%>%
#   rename(status=status50)
# 
# 
# fcnzplus_long_20 <- pivot_longer(fcnplus,cols =c(11) )%>% 
#   dplyr::select(c(1,2,8,9,13))%>%
#   mutate(ratio="compared2_2020",
#          scenario="fcnzplus"
#   )%>%
#   rename(status=status20)
# 
# 
# fcnzplus_long_50 <- pivot_longer(fcnplus,cols =c(12) )%>% 
#   dplyr::select(c(1,2,8,10,13))%>%
#   mutate(ratio="compared2_2050",
#          scenario="fcnzplus"
#   )%>%
#   rename(status=status50)
# 
# 
# # combinando em um df so
# 
# df_comb <- rbind(base_long_20,fcnz_long_20,fcnz_long_50,fcnzplus_long_20,fcnzplus_long_50)


# salvando resultados

# write.csv(df_comb,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_rescaled.csv",row.names = F)

write.csv(final_tbl,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_raw.csv",row.names = F)

write.csv(bio_tbl_sc,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_results_sc.csv",row.names = F)
