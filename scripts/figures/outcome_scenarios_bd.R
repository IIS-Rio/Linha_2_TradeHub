# pacotes ----------------------------------------------------------------------
library(scales)
library(ggpubr)
library(ggmap)
library(RColorBrewer)
library(sf)
library(geobr)
library(ggthemes)
#-------------------------------------------------------------------------------

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

# filtrando apenas dados completos

#base <- base[base$ecoregion%in% fcnz$ecoregion,]

# calcular % em relacao a 2050 ou 2020? por enquanto, 2050


# comparando metricas ----------------------------------------------------------

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



# cenario fcnz em funcao do base 2050 (mudar depois pra 2020)

# como faltam 2 regioes no base_2020, vai dar NAs nelas (2,12)

fcnz_relative <- left_join(fcnz_m,base_2020_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2020=value.y,value=value.x)%>%
          #limpa na
          filter(!is.na(value_base_2020))%>%
          # foca so em biodiv%>%
          filter(!name %in% c("cb.val","oc.val"))%>%
          # calculating ratio
          mutate(ratio=(value/value_base_2020))

# ficou igaul 2020 e 2050 

base_relative <- left_join(base_2050_m,base_2020_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2020=value.y,value=value.x)%>%
  #limpa na
  filter(!is.na(value_base_2020))%>%
  # foca so em biodiv%>%
  filter(!name %in% c("cb.val","oc.val"))%>%
  # calculating ratio
  mutate(ratio=(value/value_base_2020))


# tem diferencas ainda no tamanho das tabelas, isso precisa sumir depois q tiver rodado todas!


# abrindo raster ecoregioes

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif") 

ecoregion_vec <- as.polygons(ecoregion)

# bd ---------------------------------------------------------------------------

# fcnz

bd_fcnz <- filter(fcnz_relative,name=="bd.val")

# checar qndo bd eh menor no cenario 2050
# qndo isso acontece, o ratio, como % eh uma diferenca entre o ratio e 1

# qndo bd eh maior no cenario 2050 e >1. pra calcular a % eh so usar mesmo o proprio ratio
# qndo eh menor q 1? eh 1 - o ratio.

check <- filter(bd2,value>value_base_2050)

# ajustando isso pra criar a coluna %

bd_fcnz <- bd_fcnz %>%
  mutate(rel_change=if_else(value<value_base_2020,ratio-1,if_else(value>value_base_2020&ratio>1,ratio,1-ratio)))

ecoregion_vec2 <- st_as_sf(ecoregion_vec)
ecoregion_vec2 <- st_as_sf(left_join(bd_fcnz,ecoregion_vec2,by = join_by(ecoregion==focal_modal) ))


# ratio > signfica que base>scen, logo, a reducao eh maior no base!
# pra mostrar onde eh pior no scen, teria q inverter entao!
# valores negativos ou positivos nao tem siginificado. so maior ou menor importa. 

# qndo base eh > q fcnz?
# qndo os valores do ratio sao negativos ate menor q 1 positivo!

# Define a color palette
# Define a custom color palette
#bd_rast <- rasterize(vect(ecoregion_vec),ecoregion,field="ratio")
# bd_rast2 <- rasterize(vect(ecoregion_vec),ecoregion,field="ratio")

# Specify the number of shades of blue you want

num_shades_blue <- length(unique(bd_fcnz$ratio[bd_fcnz$ratio<1]))
num_shades_red <- length(unique(bd_fcnz$ratio[bd_fcnz$ratio>1]))
# Generate a range of blue color codes
# a qntidade de cores muda a legenda, e ferra a transicao.

blue_palette <- colorRampPalette(c("lightblue", "darkblue"))(num_shades_blue)
red_palette <- colorRampPalette(c("orange", "darkred"))(num_shades_red)

blue_palette_legend <- colorRampPalette(c("lightblue", "darkblue"))(15)
red_palette_legend <- colorRampPalette(c("orange", "darkred"))(5)

color_df_blues <- data.frame(unique_vals=unique(bd_fcnz$ratio[bd_fcnz$ratio<1]))%>%
    arrange(unique_vals)%>%
    mutate(col_code=rev(blue_palette))

color_df_red <- data.frame(unique_vals=unique(bd_fcnz$ratio[bd_fcnz$ratio>1]))%>%
  arrange(unique_vals)%>%
  mutate(col_code=(red_palette))

#combine

color_df <- rbind(color_df_blues,color_df_red)

# tentar plotar como shape mesmo (mais rapido!)

# da pra fazer um painel por cenario, e com cada metrica!!

color_df$unique_vals <- as.numeric(color_df$unique_vals)
color_df$ID <- round(color_df$unique_vals,4)

ecoregion_vec2$ID <- round(ecoregion_vec2$ratio,4)

ecoregion_vec2 <- left_join(ecoregion_vec2,color_df)

# brazil countours

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(st_crs(ecoregion_vec2))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")


plot_fcnz <- ggplot() +
  geom_sf(data = Br, color = "black", fill = NA) +
  geom_sf(data = ecoregion_vec2, aes(fill = col_code), color = "transparent") +
  geom_sf(data = biomes, color = "black",fill=NA)+
  scale_fill_identity() +
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "fcnz", fill = "Legend Title" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))

# tem q ser isso pra legenda, e o de cima pra plotar!
legend_fcnz <- ggplot() +
  geom_sf(data = Br, color = "black", fill = NA) +
  geom_sf(data = ecoregion_vec2, aes(fill =ratio), color = "transparent") +
  scale_fill_gradientn(colours = 
    c(blue_palette_legend, red_palette_legend),
    limits = range(ecoregion_vec2$ratio),
    name = "Relative\nchange",
    breaks = c(-4.1, 4.5,7.4),
    labels = c("-4.1", "1", "7")) +
    labs( title = "fcnz") +
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))

legend2plot <- get_legend(legend_fcnz,position = "left")

# baseline

# o range eh mto maior, tem q mudar os valores relativos no eixo. ou normalizar pra ficar na mesma escala!!

bd_baseline <- filter(base_relative,name=="bd.val")

num_shades_blue2 <- length(unique(bd_baseline$ratio[bd_baseline$ratio<1]))
num_shades_red2 <- length(unique(bd_baseline$ratio[bd_baseline$ratio>1]))
# Generate a range of blue color codes
# a qntidade de cores muda a legenda, e ferra a transicao.

blue_palette2 <- colorRampPalette(c("lightblue", "darkblue"))(num_shades_blue2)
red_palette2 <- colorRampPalette(c("orange", "darkred"))(num_shades_red2)

color_df_blues2 <- data.frame(unique_vals=unique(bd_baseline$ratio[bd_baseline$ratio<1]))%>%
  arrange(unique_vals)%>%
  mutate(col_code=rev(blue_palette2))

color_df_red2 <- data.frame(unique_vals=unique(bd_baseline$ratio[bd_baseline$ratio>1]))%>%
  arrange(unique_vals)%>%
  mutate(col_code=(red_palette2))

#combine

color_df2 <- rbind(color_df_blues2,color_df_red2)

# adicionnando aos poligonos

color_df2$unique_vals <- as.numeric(color_df2$unique_vals)
color_df2$ID <- round(color_df2$unique_vals,5)

ecoregion_vec3 <- st_as_sf(ecoregion_vec)

ecoregion_vec3 <- st_as_sf(left_join(bd_baseline,ecoregion_vec3,by = join_by(ecoregion==focal_modal),copy=TRUE ))

ecoregion_vec3$ID <- round(ecoregion_vec3$ratio,5)

ecoregion_vec3 <- left_join(ecoregion_vec3,color_df2)

# baseline tb ta melhorando, nao pode ta certo!

plot_baseline <- ggplot() +
  geom_sf(data = Br, color = "black", fill = NA) +
  geom_sf(data = ecoregion_vec3, aes(fill = col_code), color = "transparent") +
  geom_sf(data = biomes, color = "black",fill=NA)+
  scale_fill_identity() +
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #ggtitle("fcnz")+
  labs( title = "baseline", fill = "Legend Title" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_map()+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))


# isso aqui ta ficando melhor
arranged_plot <- ggarrange(plot_baseline,plot_fcnz,plot_fcnz,legend2plot,ncol=4,heights = c(1,1,1,0.5),widths = c(1,1,1,0.5))+
  theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))

# Add the title
final_bd <- annotate_figure(arranged_plot , top = text_grob("Extinction Debt",color = "black", face = "bold", size = 12))

ggsave(plot = final_bd,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_bd_example_fig.png",height = 18,width = 32,units = "cm",dpi=100)

