# pacotes ----------------------------------------------------------------------
library(scales)
library(ggpubr)
library(RColorBrewer)
library(sf)
library(geobr)
library(ggthemes)
library(tidyr)
library(data.table)
library(ggrepel)
#-------------------------------------------------------------------------------

# abrindo resultados

df_comb <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_rescaled.csv")


# Barplot with rotated axis and facets

results_2020 <- df_comb%>%
  filter(mtc_nm=="bd.val",
         ratio=="compared2_2020")%>%
  ggplot(aes(x = as.factor(ecoID), y = value, fill = status)) +
    geom_bar(stat = "identity") +
    labs(title = "Reduction of extinction debt",
         x = "Ecoregion ID",
         y = "Rescaled metric (-1;1)",
         fill = "Condition") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~scenario, scales = "free_y")+
    coord_flip() +
  scale_fill_manual(values = c("blue","red"))


results_2050 <- df_comb%>%
  filter(mtc_nm=="bd.val",
         ratio=="compared2_2050")%>%
  ggplot(aes(x = as.factor(ecoID), y = value, fill = status)) +
  geom_bar(stat = "identity") +
  labs(title = "Reduction of extinction debt",
       x = "Ecoregion ID",
       y = "Rescaled metric (-1;1)",
       fill = "Condition") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~scenario, scales = "free_y")+
  coord_flip() +
  scale_fill_manual(values = c("blue","red"))


combined <- ggarrange(results_2020,results_2050,common.legend = T)

ggsave(plot = combined,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bd_barplot.png",width = 32,height = 20,units = "cm")

# adicionando mapa!

ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv04.shp")

names(ecoregions) <- c("ecoID","mtc_nm","vlfcnz50","vlbse20","rt20fcnz","vlbse50","rt50fcnz","rt20bse","vlfcnzpls50","rt20fcnzpls","rt50fcnzpls","ecor_nm","geometry")

coords <-st_coordinates( st_centroid(ecoregions) )

ecoregions <- cbind(ecoregions,coords)

# labels

labels <- unique(ecoregions[,c(1,13,14)])

labels <- st_drop_geometry(labels)


library(Polychrome)

P60 = createPalette(60,  c("#ff0000", "#00ff00", "#0000ff"))
P60 <- sortByHue(P60)
P60 <- as.vector(t(matrix(P60, ncol=4)))
# Extract color codes from the palette
library(lessR)
colors <- getColors(P60, 60)



legend_labels <- unique(ecoregions[,c(1,12)])
legend_labels$tolegend=paste0(legend_labels$ecoID,":",legend_labels$ecor_nm)

ecoregions <- ecoregions%>% 
  mutate(tolegend=paste0(ecoID," :",ecor_nm))

plot_ecor <- ecoregions%>% 
  ggplot() +
  geom_sf( aes(fill = as.factor(ecoID)), color = "darkgray") + # era col_code
  geom_text_repel(data=labels,aes(x = X, y = Y, label = ecoID),
                  #hjust = 0, nudge_x = 1, nudge_y = 4,
                  size = 3, color = "black", fontface = "bold",bg.color = "white",bg.r = 0.25)+
  theme_map()+
  scale_fill_manual(values = P60)
  

ggsave(plot = plot_ecor,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ecoregions2.png",width = 30,height = 20,units = "cm")

#plot_ecor + scale_fill_viridis_d()

