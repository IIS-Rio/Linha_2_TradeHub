library(scales)
library(ggpubr)
library(ggmap)
# analise exploratoria resultado scenarios

# caminho

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/result_tables"

# listando tabelas

lst_tbls <- list.files(p,full.names = T)

# abrindo scenarios

base <-read.csv(lst_tbls[grep("base",lst_tbls)])
fcnz <- read.csv(lst_tbls[grep("fcnz",lst_tbls)])

# filtrando apenas dados completos

base <- base[base$ecoregion%in% fcnz$ecoregion,]

# calcular % em relacao a 2050 ou 2020? por enquanto, 2050


# comparando metricas ----------------------------------------------------------

base_m <- filter(base,variable=="metrics")
fcnz_m <- filter(fcnz,variable=="metrics")

# cenario fcnz em funcao do base 2050 (mudar depois pra 2020)

fcnz_relative <- left_join(fcnz_m,base_m[,c(1,3,4)],by=c("ecoregion","name"))%>%
  dplyr::rename(value_base_2050=value.y,value=value.x)%>%
          #limpa na
          filter(!is.na(value))%>%
          # foca so em biodiv%>%
          filter(!name %in% c("cb.val","oc.val"))%>%
          # calculating ratio
          mutate(ratio=(value/value_base_2050),
                 inv_ratio=1-ratio)# invertendo pra mostrar onde eh pior no cenario nao-base


# as metricas sao de reducao. qnto menor a reducao, pior, logo qnto menor o valor do ratio, pior!

# abrindo raster ecoregioes

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif") 

ecoregion_vec <- as.polygons(ecoregion)

bd <- filter(fcnz_relative,name=="bd.val")

ecoregion_vec <- st_as_sf(ecoregion_vec)
ecoregion_vec <- st_as_sf(left_join(bd,ecoregion_vec,by = join_by(ecoregion==focal_modal) ))


# ratio > signfica que base>scen, logo, a reducao eh maior no base!
# pra mostrar onde eh pior no scen, teria q inverter entao!
# valores negativos ou positivos nao tem siginificado. so maior ou menor importa. 

# qndo base eh > q fcnz?
# qndo os valores do ratio sao negativos ate menor q 1 positivo!

# Define a color palette
# Define a custom color palette
#bd_rast <- rasterize(vect(ecoregion_vec),ecoregion,field="ratio")
# bd_rast2 <- rasterize(vect(ecoregion_vec),ecoregion,field="ratio")

library(RColorBrewer)

# Specify the number of shades of blue you want

num_shades_blue <- length(unique(df$ratio[df$ratio<1]))
num_shades_red <- length(unique(df$ratio[df$ratio>1]))
# Generate a range of blue color codes
# a qntidade de cores muda a legenda, e ferra a transicao.

blue_palette <- colorRampPalette(c("lightblue", "darkblue"))(num_shades_blue)
red_palette <- colorRampPalette(c("orange", "darkred"))(num_shades_red)

blue_palette_legend <- colorRampPalette(c("lightblue", "darkblue"))(20)
red_palette_legend <- colorRampPalette(c("orange", "darkred"))(4)

color_df_blues <- data.frame(unique_vals=unique(df$ratio[df$ratio<1]))%>%
    arrange(unique_vals)%>%
    mutate(col_code=rev(blue_palette))

color_df_red <- data.frame(unique_vals=unique(df$ratio[df$ratio>1]))%>%
  arrange(unique_vals)%>%
  mutate(col_code=(red_palette))

#combine

color_df <- rbind(color_df_blues,color_df_red)

# tentar plotar como shape mesmo (mais rapido!)

# da pra fazer um painel por cenario, e com cada metrica!!

color_df$unique_vals <- as.numeric(color_df$unique_vals)
color_df$ID <- round(color_df$unique_vals,3)

ecoregion_vec$ID <- round(ecoregion_vec$ratio,3)

ecoregion_vec2 <- left_join(ecoregion_vec,color_df)

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
  theme_map()

# tem q ser isso pra legenda, e o de cima pra plotar!
legend_fcnz <- ggplot() +
  geom_sf(data = Br, color = "black", fill = NA) +
  geom_sf(data = ecoregion_vec2, aes(fill = ratio), color = "transparent") +
  scale_fill_gradientn(colours = 
    c(blue_palette_legend, red_palette_legend),
    limits = range(ecoregion_vec2$ratio),
    name = "Relative change",
    breaks = c(-2.5,0, 1)
  ) +
  labs(
    title = "fcnz",
    fill = "Legend Title"
  ) +
  theme_map()

legend2plot <- get_legend(legend_fcnz,position = "left")
plot(legend2plot)

# isso aqui ta ficando melhor
arranged_plot <- ggarrange(plot_fcnz,plot_fcnz,plot_fcnz,legend2plot,ncol=4,heights = c(1,1,1,0.5),widths = c(1,1,1,0.5))

# Add the title
final_bd <- annotate_figure(arranged_plot , top = text_grob("Extinction Debt",color = "black", face = "bold", size = 12))

ggsave(plot = plot,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/outcome_bd_fcnz_relative_baseline2050.png",height = 10,width = 10,units = "cm",dpi=100)
