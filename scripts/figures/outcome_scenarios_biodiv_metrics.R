# pacotes ----------------------------------------------------------------------
library(scales)
library(ggpubr)
library(ggmap)
library(RColorBrewer)
library(sf)
library(geobr)
library(ggthemes)
#-------------------------------------------------------------------------------

# shape com resultados razao ja calculada.

ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv02.shp")

names(ecoregions) <- c("ecoID","mtc_nm","rt20fcnz","vlbse50","rt20bse","vlbse20","rt20fcnzplus","vlfcnz50","rt50fcnz","vlfcnzplus50","rt50fcnzplus","eco_nm","geometry" ) 

# plotando comparado a base 2020

base <- ecoregions%>%select_at(c(1,2,4,5,6,12))
fcnz <- ecoregions%>%select_at(c(1,2,3,6,8,9,12))
fcnplus <- ecoregions%>%select_at(c(1,2,6,7,10,12))

# fcnz -------------------------------------------------------------------------

fcnz <- fcnz%>%
  mutate(status=if_else(vlfcnz50>vlbse20,"improved","worsened"),
         # elimina valores extremos pra plotar!!
         rt50fcnz = if_else(rt20fcnz < -2, -2, if_else(rt50fcnz > 2, 2, rt50fcnz)))%>%
  mutate(ratio_sc = case_when(
    status == "improved" ~ (rt20fcnz - min(rt20fcnz)) / (max(rt20fcnz) - min(rt20fcnz)) ,
    status == "worsened" ~ (rt20fcnz - min(rt20fcnz)) / (max(rt20fcnz) - min(rt20fcnz))* -1,
    TRUE ~ NA_real_))

# fcnzplus -------------------------------------------------------------------------


fcnplus <- fcnplus%>%
  filter_all(all_vars(!is.na(.)))%>%
  mutate(status=if_else(vlfcnzplus50>vlbse20,"improved","worsened"),
         # elimina valores extremos pra plotar!!
         rt20fcnzplus = if_else(rt20fcnzplus < -2, -2, if_else(rt20fcnzplus > 2, 2, rt20fcnzplus)))%>%
  mutate(ratio_sc = case_when(
    status == "improved" ~ (rt20fcnzplus - min(rt20fcnzplus)) / (max(rt20fcnzplus) - min(rt20fcnzplus)) ,
    status == "worsened" ~ (rt20fcnzplus - min(rt20fcnzplus)) / (max(rt20fcnzplus) - min(rt20fcnzplus))* -1,
    TRUE ~ NA_real_))

# base -------------------------------------------------------------------------


base <- base%>%
  filter_all(all_vars(!is.na(.)))%>%
  mutate(status=if_else(vlbse50>vlbse20,"improved","worsened"),
         # elimina valores extremos pra plotar!!
         rt20fcnzplus = if_else(rt20bse < -2, -2, if_else(rt20bse > 2, 2, rt20bse)))%>%
  mutate(ratio_sc = case_when(
    status == "improved" ~ (rt20bse - min(rt20bse)) / (max(rt20bse) - min(rt20bse)) ,
    status == "worsened" ~ (rt20bse - min(rt20bse)) / (max(rt20bse) - min(rt20bse))* -1,
    TRUE ~ NA_real_))

# plotando mapas ---------------------------------------------------------------

# brazil countours

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(st_crs(ecoregions))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")


plot_base <- base%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc), color = "transparent") + # era col_code
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "base 2050/base 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")


plot_fcnz <- fcnz%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc), color = "transparent") + # era col_code
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnz 2050/base 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")

plot_fcnzplus <- fcnplus%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc), color = "transparent") + # era col_code
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnzplus 2050/base 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")


ggsave(plot = plot_fcnz,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_fcnz_base_2050.png",height = 18,width = 32,units = "cm",dpi=100)

ggsave(plot = plot_fcnzplus,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_fcnzplus_base_2050.png",height = 18,width = 32,units = "cm",dpi=100)

# combinando

combined <- ggarrange(plot_base,plot_fcnz,plot_fcnzplus,nrow = 3,common.legend = T)


ggsave(plot = combined,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_allscen_base_2050.png",height = 25,width = 32,units = "cm",dpi=200)

# precisa ter de fato comparacao considerando 2050, pra evidenciar danos com relacao ao baseline 2050!

# falta preencher ecorregioes com NAs e melhorar a estetica da figura!