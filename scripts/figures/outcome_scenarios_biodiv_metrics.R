# plota resultados relativos a 2020.

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

ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv04.shp")

names(ecoregions) <- c("ecoID","mtc_nm","vlfcnz50","vlbse20","rt20fcnz","vlbse50","rt50fcnz","rt20bse","vlfcnzpls50","rt20fcnzpls","rt50fcnzpls","ecor_nm","geometry") 



base <- ecoregions%>%select_at(c(1,2,4,6,8,12))
fcnz <- ecoregions%>%select_at(c(1,2,3,4,5,6,7,12))
fcnplus <- ecoregions%>%select_at(c(1,2,4,6,9:12))

# fcnz ------------------------------------------------------------------------


# OBS. Pras cores serem comparaveis, precisam estar todos na mesma escala. dividir pelos maiores e menores valores gerais

ecoregions_long_2020 <- pivot_longer(st_drop_geometry(ecoregions),cols = c(5,8,10))
ecoregions_long_2050 <- pivot_longer(st_drop_geometry(ecoregions),cols = c(7,11))

minmax2020 <- range(ecoregions_long_2020$value)
minmax2050 <- range(ecoregions_long_2050$value)

# mas aqui se os valores sao subtituidos por -2 e 2. os valores minmax deveriam ser esses.

# ficou pior fazendo isso. pensar melhor como fazer todos na mesma escala de cores!!
# pensar em separar o minmax 2020 e o minmax 2050!!

fcnz <- fcnz%>%
  # adiciona coluna falando se piorou ou melhorou
  mutate(status20=if_else(vlfcnz50>vlbse20,"improved","worsened"),
         status50=if_else(vlfcnz50>vlbse50,"improved","worsened"),
    # elimina valores extremos pra plotar: ratio 2050/2020
    rt20fcnz = if_else(rt20fcnz < -2, -2, if_else(rt20fcnz > 2, 2, rt20fcnz)),
    # elimina valores extremos ratio 2050/base 2050
    rt50fcnz = if_else(rt50fcnz < -2, -2, if_else(rt50fcnz > 2, 2, rt50fcnz)))%>%
  mutate(
    # reescala valores pra plotar entre -1 e 1 pra razaao 2020 (substitui min e max do proprio cenario pelos maximos e minimos totais!)
    ratio_sc_20 = case_when(
    status20 == "improved" ~ (rt20fcnz - minmax2020[1] ) / (minmax2020[2] - minmax2020[1]) , #min(rt20fcnz), max(rt20fcnz) min(rt20fcnz)
    status20 == "worsened" ~ (rt20fcnz - minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
    TRUE ~ NA_real_),
    # reescala valores pra plotar entre -1 e 1 pra razaao 2050
    ratio_sc_50 = case_when(
      status50 == "improved" ~ (rt50fcnz - minmax2050[1]) / (minmax2050[2] -minmax2050[1]) ,
      status50 == "worsened" ~ (rt50fcnz - minmax2050[1]) / (minmax2050[2] - minmax2050[1])* -1,
      TRUE ~ NA_real_))


summary(fcnz$ratio_sc_50)

table(fcnz$status20)# so 1 improved
table(fcnz$status50) # 1uase todos melhoram!

# fcnzplus --------------------------------------------------------------------

table(fcnplus$status20)# so 4 improved. 
table(fcnplus$status50)# todas!

fcnplus <- fcnplus%>%
  filter_all(all_vars(!is.na(.)))%>%
  mutate(status20=if_else(vlfcnzpls50>vlbse20,"improved","worsened"),
         status50=if_else(vlfcnzpls50>vlbse50,"improved","worsened"),
         # elimina valores extremos pra plotar!!
         rt20fcnzpls = if_else(rt20fcnzpls < -2, -2, if_else(rt20fcnzpls > 2, 2, rt20fcnzpls)),
         rt50fcnzpls = if_else(rt50fcnzpls < -2, -2, if_else(rt50fcnzpls > 2, 2, rt50fcnzpls)
         ))%>%
  mutate(ratio_sc_20 = case_when(
    status20 == "improved" ~ (rt20fcnzpls - minmax2020[1]) / (minmax2020[2] - minmax2020[1]) ,
    status20 == "worsened" ~ (rt20fcnzpls -minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
    TRUE ~ NA_real_),
    ratio_sc_50 = case_when(
      status50 == "improved" ~ (rt50fcnzpls - minmax2050[1]) / (minmax2050[2] - minmax2050[1]) ,
      status50 == "worsened" ~ (rt50fcnzpls - minmax2050[1]) / (minmax2050[2] - minmax2050[1])* -1,
      TRUE ~ NA_real_))

# base ------------------------------------------------------------------------

# so tem a comparacao com 2020

table(base$status) # todos pioram (ok)

base <- base%>%
  filter_all(all_vars(!is.na(.)))%>%
  mutate(status=if_else(vlbse50>vlbse20,"improved","worsened"),
         # elimina valores extremos pra plotar!!
         rt20fcnzpls = if_else(rt20bse < -2, -2, if_else(rt20bse > 2, 2, rt20bse)))%>%
  mutate(ratio_sc = case_when(
    status == "improved" ~ (rt20bse - minmax2020[1]) / (minmax2020[2] - minmax2020[1]) ,
    status == "worsened" ~ (rt20bse - minmax2020[1]) / (minmax2020[2] - minmax2020[1])* -1,
    TRUE ~ NA_real_))


# plotando mapas --------------------------------------------------------------

# brazil countours

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(st_crs(ecoregions))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")



# mapas baseline ---------------------------------------------------------------

plot_base <- base%>% 
  ggplot() +
  # valor das metricas
  geom_sf( aes(fill = ratio_sc), color = "transparent") + # era col_code
  #limite ecorregioes
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.1)+
  # biomas
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "base 2050/base 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")+
  theme(legend.position = "none")

# fcnz -------------------------------------------------------------------------

plot_fcnz2020 <- fcnz%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc_20), color = "transparent") + # era col_code
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.05)+
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnz 2050/base 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")+
  theme(legend.position = "none")


plot_fcnz2050 <- fcnz%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc_50), color = "transparent") + # era col_code
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.05)+
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnz 2050/base 2050", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")+
  theme(legend.position = "right")

# fcnzplus ----------------------------------------------------------------------

plot_fcnzplus_2020 <- fcnplus%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc_20), color = "transparent") + # era col_code
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.05)+
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnzplus 2050/base 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")+
  theme(legend.position = "right")


plot_fcnzplus_2050 <- fcnplus%>% #ecoregions2
  #mutate(log_ratio_sc = log(ratio_sc + 1)) %>%
  ggplot() +
  #geom_sf(data = Br, color = "black", fill = NA) +
  #geom_sf(aes(fill = log_ratio_sc), color = "transparent") +
  geom_sf( aes(fill = ratio_sc_50), color = "transparent") + # era col_code
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.05)+
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnzplus 2050/base 2050", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtc_nm")+
  theme(legend.position = "none")

# resultados dos ratios esta estranho, rever!!

# salvando resultados ---------------------------------------------------------

# isso com os resultados na mesma escala!!

ggsave(plot = plot_fcnz2020,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_fcnz_base_2020.png",height = 18,width = 32,units = "cm",dpi=100)

ggsave(plot = plot_fcnz2050,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_fcnz_base_2050.png",height = 18,width = 32,units = "cm",dpi=100)


ggsave(plot = plot_fcnzplus_2020,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_fcnzplus_base_2020.png",height = 18,width = 32,units = "cm",dpi=100)

ggsave(plot = plot_fcnzplus_2050,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_fcnzplus_base_2050.png",height = 18,width = 32,units = "cm",dpi=100)

ggsave(plot = plot_base,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_base_2050_2020.png",height = 18,width = 32,units = "cm",dpi=100)


# combinando

# # extraindo legenda
# 
# legenda <- get_legend(plot_fcnz2020,position = c("top"))
# plot(legenda)
# 
# 
# legenda_size <- 5
# 
# combined <- ggarrange(plot_base,plot_fcnz2020,plot_fcnzplus_2020,nrow = 3,legend = "none")
# 
# combined <- ggarrange(plot_base,plot_fcnz2020,plot_fcnzplus_2020,nrow = 3,align = "v")

library(cowplot)

combined3 <- plot_base/plot_fcnz2020/plot_fcnzplus_2020
combined4 <- plot_fcnz2050/plot_fcnzplus_2050
# combined2 <- ggarrange(legenda,combined,heights = c(1,5),widths = c(1,5),nrow = 2,ncol=1,hjust = c(0.5, 0.5),align = "h")


ggsave(plot = combined3,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_allscen_base_2020.png",height = 20,width = 32,units = "cm",dpi=200)

ggsave(plot = combined4,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_allscen_base_2050.png",height = 20,width = 32,units = "cm",dpi=200)
