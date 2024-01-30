library(ggplot2)
library(RColorBrewer)
library(ggpubr)
library(ggmap)
library(ggthemes)
# plotar mapa de areas restauradas

baseline2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_baseline_2050.tif")
 
fcnz <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_fcnz_2050.tif")

fcnzplus <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_fcnzplus_2050.tif")

# funcao transforma em area e depois em df

area <- 1000*1000/10^4
scenarios <- c("baseline","fcnz","fcnzplus")

f <- function(x,area,index){
  
  x=x*area
  df <- as.data.frame(x,xy=T)
  names(df)[3] <-"area_changed" 
  df$scen <- scenarios[index]
  return(df)
  c=c+1
}
  

restored_areas <- lapply(seq_along(list(baseline2050, fcnz, fcnzplus)), function(i) {f(list(baseline2050, fcnz, fcnzplus)[[i]], area, i)
})

restored_areas_df <- do.call(rbind,restored_areas)%>%filter(!is.na(area_changed))

# adicionar br

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(crs(fcnz))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")%>% st_transform(crs(fcnz))


plot(fcnz)
plot(st_geometry(biomes),add=T)
restored_areas_df <- restored_areas_df%>%filter(area_changed>0)

restored_plot <-  ggplot() +
  geom_tile(data = restored_areas_df,aes(x = x, y = y, fill = area_changed)) +
  #scale_fill_viridis_c() +  # You can change the color scale as needed
  scale_fill_gradientn(colors = brewer.pal(9, "Blues")) +
  geom_sf(data = biomes, color = "black",fill=NA)+
  #geom_sf(data = Br, color = "black", fill = NA) +
  facet_wrap(~ scen) +
  labs(fill = "Restored areas (ha)") +
  theme_map()

ggsave(plot = restored_plot,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/restored_areas.png",height = 18,width = 32,units = "cm",dpi=250)


# plotar mapa de areas desmatadas

baseline2050_desm <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_baseline_2050.tif")*-1

fcnz_desm <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_fcnz_2050.tif")*-1

fcnzplus_desm <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_fcnzplus_2050.tif")*-1

# transformar em area, fazer gradiente azul em oposicao ao desmatamento vermelho
# ajustar painel!

converted_areas <- lapply(seq_along(list(baseline2050_desm, fcnz_desm, fcnzplus_desm)), function(i) {f(list(baseline2050_desm, fcnz_desm, fcnzplus_desm)[[i]], area, i)
})

converted_areas_df <- do.call(rbind,converted_areas)%>%filter(!is.na(area_changed))

converted_areas_df <- converted_areas_df%>%filter(area_changed!=0)

converted_plot <-  ggplot() +
  geom_tile(data = converted_areas_df,aes(x = x, y = y, fill = area_changed)) +
  #scale_fill_viridis_c() +  # You can change the color scale as needed
  scale_fill_gradientn(colors = brewer.pal(9, "Reds")) +
  geom_sf(data = biomes, color = "black",fill=NA)+
  #geom_sf(data = Br, color = "black", fill = NA) +
  facet_wrap(~ scen) +
  labs(fill = "Converted area (ha)") +
  theme_map()

ggsave(plot = converted_plot,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/converted_areas.png",height = 18,width = 32,units = "cm",dpi=250)
