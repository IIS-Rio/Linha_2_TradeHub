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


# juntando em um dataframe unico -----------------------------------------------

# pra isso precisa colocar em formato long

base_l <- base %>%
  dplyr::rename(ratio_sc_20=ratio_sc)%>%
  pivot_longer(cols=c(rt20bse,vlbse20,ratio_sc_20))%>%
  select(c(-3,-6))%>%
  dplyr::rename(status20=status)%>%
  mutate(status50=NA,
         scenario="baseline")

fcnz_l <- fcnz %>%pivot_longer(cols=c(vlfcnz50,rt20fcnz,rt50fcnz,ratio_sc_20,ratio_sc_50))%>%
  select(c(-3,-4))%>%
  mutate(scenario="fcnz")

fcnzplus_l <- fcnplus %>%pivot_longer(cols=c(vlfcnzpls50,rt20fcnzpls,rt50fcnzpls,ratio_sc_20,ratio_sc_50))%>%
  select(c(-3,-4))%>%
  mutate(scenario="fcnzplus")

# juntando

df_l <- rbind(base_l,fcnz_l,fcnzplus_l)


# plotando mapas --------------------------------------------------------------

# brazil countours

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(st_crs(ecoregions))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")

values2plot <- unique(df_l$name)


# 2020 -------------------------------------------------------------------------

compared2_2020_map <- df_l%>%
  # definindo comparacao
  filter(name==values2plot[grep("sc_20",values2plot)])%>%
  ggplot() +
  # valor das metricas
  geom_sf( aes(fill = value), color = "transparent") + # era col_code
  #limite ecorregioes
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.1)+
  # biomas
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "Relative to baseline 2020", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap(mtc_nm~scenario)+
  theme(legend.position = "top",
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title =  element_text(size = 6))


# barplot

df_comb_bars <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_rescaled.csv")

library(readr)
bio_carbon_lu_results <- read_csv("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_carbon_lu_results.csv")

# Barplot with rotated axis and facets

results_2020bar <- df_comb_bars%>%
  filter(
         #mtc_nm=="bd.val",
         ratio=="compared2_2020")%>%
  left_join(bio_carbon_lu_results)%>%
  ggplot(aes(x = as.factor(ecoIDseq), y = value, fill = value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Add vertical line at zero
  labs(title = "",
       x = "",
       y = "",
       fill = "Condition") +
  theme_minimal_grid() +
  theme(
    #axis.line.y = element_line(color = "black"),
    #axis.line.x = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(mtc_nm~scenario,  switch = "x")+
  #coord_flip() +
  #scale_fill_manual(values = c("blue","red"))
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 10),
        axis.ticks.x = element_line(size = 0.2, color = "black"),
        strip.text = element_text(size = 8)  # Adjust facet title size
        )


combined <- ggarrange(compared2_2020_map,results_2020bar,widths = c(0.5,1))

ggsave("/dados/pessoal/francisco/Linha_2_TradeHub/figures/ppt/biodiv_comparedto2020.png",plot = combined,scale = 1,width = 32,height = 18,units = "cm", bg = "white")

# 2050 -------------------------------------------------------------------------

compared2_2050_map <- df_l%>%
  # definindo comparacao
  filter(name==values2plot[grep("sc_50",values2plot)])%>%
  ggplot() +
  # valor das metricas
  geom_sf( aes(fill = value), color = "transparent") + # era col_code
  #limite ecorregioes
  geom_sf(data = ecoregions, color = "white",fill=NA, lwd = 0.1)+
  # biomas
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "Relative to baseline 2050", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap(mtc_nm~scenario)+
  theme(legend.position = "top",
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title =  element_text(size = 6))


# barplot

library(readr)

bio_carbon_lu_results <- read_csv("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_carbon_lu_results.csv")

# Barplot with rotated axis and facets

results_2050bar <- df_comb_bars%>%
  filter(
    #mtc_nm=="bd.val",
    ratio=="compared2_2050")%>%
  left_join(bio_carbon_lu_results[,c(1,2,3,6,7,8)])%>%
  ggplot(aes(x = as.factor(ecoIDseq), y = value, fill = value)) +
  geom_bar(stat = "identity", position = position_dodge(width = 1)) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black") +  # Add vertical line at zero
  labs(title = "",
       x = "",
       y = "",
       fill = "Condition") +
  theme_minimal_grid() +
  theme(
    #axis.line.y = element_line(color = "black"),
    #axis.line.x = element_line(color = "black"),
    panel.border = element_rect(color = "black", fill = NA)
  ) +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(mtc_nm~scenario,  switch = "x")+
  #coord_flip() +
  #scale_fill_manual(values = c("blue","red"))
  scale_fill_gradient2(low = "red", mid = "gray", high = "blue", midpoint = 0)+
  theme(legend.position = "none",
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 5),
        axis.text.y = element_text(size = 10),
        axis.ticks.x = element_line(size = 0.2, color = "black"),
        strip.text = element_text(size = 8)  # Adjust facet title size
  )


combined2050 <- ggarrange(compared2_2050_map,results_2050bar,widths = c(0.5,1))

ggsave("/dados/pessoal/francisco/Linha_2_TradeHub/figures/ppt/biodiv_comparedto2050.png",plot = combined2050,scale = 1,width = 32,height = 18,units = "cm", bg = "white")
