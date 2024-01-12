library(ggpubr)


df <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_carbon_lu_results.csv")

# plotar net_veg ~ metrica (usando 2050 como base)


df_2050 <- filter(df,ratio=="compared2_2050")
df_2020 <- filter(df,ratio=="compared2_2020")

bio <- filter(df_2050,mtc_nm%in%c("it.val","bd.val","ec.val"))%>%
  dplyr::rename(bio=value)

netveg <- filter(df_2050,mtc_nm=="net_veg_ha")%>%
  dplyr::rename(veg=value)

# precisa incorporar area

eco <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shp")%>%
  group_by(ecoID)%>%
  summarise()%>%
  st_make_valid()

eco$area <- as.numeric(st_area(eco))/10^4


eco_df <- st_drop_geometry(eco)

# juntando

df2plot <- left_join(bio,netveg[,c(1,5,7)])%>%
  left_join(eco_df)%>%
  mutate(prop_veg =veg/area)


df2plot%>%
  ggplot()+
  geom_point(aes(x=area,y=bio,color=status))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Add
  facet_grid(scenario~mtc_nm,scales="free")+
  xlab("ecoregion area")+
  scale_y_log10()+
  theme_bw()#+
  ylim(0.15,0.16)

# sugestao bruna dividir metrica pelo net veg. seria bio ~ metrica/net

df2plot%>%
  mutate(bio_area=(bio/area)*10^6)%>%
  ggplot()+
  geom_point(aes(x=bio_area,y=bio,color=status))+
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")+
  geom_vline(xintercept = 0, linetype = "dotted", color = "black")+
  facet_grid(scenario~mtc_nm,scales="free")+
  xlab("biodiversity change/area (10^6ha)")+
  #scale_y_log10()+
  theme_bw()#+
  #ylim(0.15,0.16)


# net vegetation nao parece importar mto. a condicao inicial deve ser mais importante. tem uma resposta negativa da metrica com a quantidade restaurada. Isso deve ser um proxy pra areas mto degradadas. Acho q ta mto achatado. tirar achatamento.

# pensar em alguma analise q faça sentido

# parece q oq mais importa é o tamanho da ecorregiao. Ecorregioes grandes sao menos sensiveis a mudancas, qqer q sejam. faz sentido ja q todas elas sao metricas relacionadas à area. como atenuar esse efeito?

hist(bio$bio)
summary(bio$bio)
