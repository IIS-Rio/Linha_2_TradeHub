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

ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metrics.shp")


# plotando resultados 2050

# bd ---------------------------------------------------------------------------

# fcnz


# checar qndo bd eh menor no cenario 2050
# qndo isso acontece, o ratio, como % eh uma diferenca entre o ratio e 1

# qndo bd eh maior no cenario 2050 e >1. pra calcular a % eh so usar mesmo o proprio ratio
# qndo eh menor q 1? eh 1 - o ratio.

# ratio > signfica que base>scen, logo, a reducao eh maior no base!
# pra mostrar onde eh pior no scen, teria q inverter entao!
# valores negativos ou positivos nao tem siginificado. so maior ou menor importa. # qndo base eh > q fcnz?
# qndo os valores do ratio sao negativos ate menor q 1 positivo!

# Specify the number of shades of blue you want

# fcnz: ratio 2050


num_shades_blue <- length(unique(ecoregions$r_2050_[ecoregions$r_2050_<1]))
num_shades_red <- length(unique(ecoregions$r_2050_[ecoregions$r_2050_>1]))
# Generate a range of blue color codes
# a qntidade de cores muda a legenda, e ferra a transicao.

blue_palette <- colorRampPalette(c("lightblue", "darkblue"))(num_shades_blue)
red_palette <- colorRampPalette(c("orange", "darkred"))(num_shades_red)

blue_palette_legend <- colorRampPalette(c("lightblue", "darkblue"))(15)
red_palette_legend <- colorRampPalette(c("orange", "darkred"))(5)

color_df_blues <- data.frame(unique_vals=unique(ecoregions$r_2050_[ecoregions$r_2050_<1]))%>%
    arrange(unique_vals)%>%
    mutate(col_code=rev(blue_palette))

color_df_red <- data.frame(unique_vals=unique(ecoregions$r_2050_[ecoregions$r_2050_>1]))%>%
  arrange(unique_vals)%>%
  mutate(col_code=(red_palette))

#combine blues and reds

color_df_fcnz_ratio2050 <- rbind(color_df_blues,color_df_red)


# create id column to combine spatial with colors

color_df_fcnz_ratio2050$unique_vals <- as.numeric(color_df_fcnz_ratio2050$unique_vals)

color_df_fcnz_ratio2050$ID_fcnz <- round(color_df_fcnz_ratio2050$unique_vals,5)

ecoregions$ID_fcnz <- round(ecoregions$r_2050_,5)

ecoregions2 <- left_join(ecoregions,color_df_fcnz_ratio2050[,2:3])

# plotando maapa ---------------------------------------------------------------

# brazil countours

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(st_crs(ecoregions2))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")


plot_fcnz <- ecoregions2%>%
  ggplot() +
  geom_sf(data = Br, color = "black", fill = NA) +
  geom_sf( aes(fill = col_code), color = "transparent") +
  geom_sf(data = biomes, color = "black",fill=NA)+
  scale_fill_identity() +
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnz", fill = "Legend Title" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_wrap("mtrc_nm")

# tem q ser isso pra legenda, e o de cima pra plotar!

range(ecoregions2$r_2050_)# -27.45295;149.41552 - isso varia de acordo com o a metrica. mas no range teria q deixar igual!e como dar essa perspectiva num quadro mais geral? o graidente de cores nao permite isso. precisaria de um grafico complementar tipo barra....talvez stackeado em negativo e positivo, por cenario, com as barras dividas por ecorregiao.

legend_fcnz <- ggplot() +
  geom_sf(data = Br, color = "black", fill = NA) +
  geom_sf(data = ecoregions2, aes(fill =r_2050_), color = "transparent") +
  scale_fill_gradientn(colours = 
    c(blue_palette_legend, red_palette_legend),
    limits = range(ecoregions2$r_2050_),
    name = "Relative\nchange",
    breaks = c(-27,110,149),
    labels = c("-27", "1", "149")) +
    labs( title = "fcnz") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))

legend2plot <- get_legend(legend_fcnz,position = "top")

# # baseline
# 
# # o range eh mto maior, tem q mudar os valores relativos no eixo. ou normalizar pra ficar na mesma escala!!
# 
# bd_baseline <- filter(base_relative,name=="bd.val")
# 
# num_shades_blue2 <- length(unique(bd_baseline$ratio[bd_baseline$ratio<1]))
# num_shades_red2 <- length(unique(bd_baseline$ratio[bd_baseline$ratio>1]))
# # Generate a range of blue color codes
# # a qntidade de cores muda a legenda, e ferra a transicao.
# 
# blue_palette2 <- colorRampPalette(c("lightblue", "darkblue"))(num_shades_blue2)
# red_palette2 <- colorRampPalette(c("orange", "darkred"))(num_shades_red2)
# 
# color_df_blues2 <- data.frame(unique_vals=unique(bd_baseline$ratio[bd_baseline$ratio<1]))%>%
#   arrange(unique_vals)%>%
#   mutate(col_code=rev(blue_palette2))
# 
# color_df_red2 <- data.frame(unique_vals=unique(bd_baseline$ratio[bd_baseline$ratio>1]))%>%
#   arrange(unique_vals)%>%
#   mutate(col_code=(red_palette2))
# 
# #combine
# 
# color_df2 <- rbind(color_df_blues2,color_df_red2)
# 
# # adicionnando aos poligonos
# 
# color_df2$unique_vals <- as.numeric(color_df2$unique_vals)
# color_df2$ID <- round(color_df2$unique_vals,5)
# 
# ecoregion_vec3 <- st_as_sf(ecoregion_vec)
# 
# ecoregion_vec3 <- st_as_sf(left_join(bd_baseline,ecoregion_vec3,by = join_by(ecoregion==focal_modal),copy=TRUE ))
# 
# ecoregion_vec3$ID <- round(ecoregion_vec3$ratio,5)
# 
# ecoregion_vec3 <- left_join(ecoregion_vec3,color_df2)
# 
# # por enquanto isso gera um gradiente de cores so pra bd... nao gera pra todos!ou gera?
# 
# plot_baseline <- ggplot() +
#   geom_sf(data = Br, color = "black", fill = NA) +
#   geom_sf(data = ecoregion_vec3, aes(fill = col_code), color = "transparent") +
#   geom_sf(data = biomes, color = "black",fill=NA)+
#   scale_fill_identity() +
#   #scale_fill_manual(values = ecoregion_vec2$col_code ) +
#   #ggtitle("fcnz")+
#   labs( title = "baseline", fill = "Legend Title" ) +
#   #guides(fill = guide_legend(title = "Ratio Legend"))+
#   theme_map()+
#   theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))


# isso aqui ta ficando melhor
# arranged_plot <- ggarrange(plot_baseline,plot_fcnz,plot_fcnz,legend2plot,ncol=4,heights = c(1,1,1,0.5),widths = c(1,1,1,0.5))+
#   theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))

arranged_plot <- ggarrange(plot_fcnz,legend2plot,nrow =2,heights = c(2,0.5),widths = c(2,2,2,0.5))+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))
  


# Add the title
final_bd <- annotate_figure(arranged_plot , top = text_grob("Changes in biodiversity metrics compared to baseline 2050",color = "black", face = "bold", size = 12))

ggsave(plot = final_bd,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_biodiv_metrics_example_fig.png",height = 18,width = 32,units = "cm",dpi=100)

# falta preencher ecorregioes com NAs, corrigir o baseline 2020 e melhorar a estetica da figura!