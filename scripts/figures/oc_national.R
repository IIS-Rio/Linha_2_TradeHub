df_oc <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/oc_reais_ecoregions.csv")

df_oc2<-
  df_oc%>%
  group_by(scen) %>% # interacting info about scenario and co2 conditions
  summarise(sum_value=sum(oc_restoration))
