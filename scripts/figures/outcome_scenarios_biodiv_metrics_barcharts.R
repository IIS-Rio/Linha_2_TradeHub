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

bio <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_results_sc.csv")


# Extract unique IDs
unique_ids <- unique(bio$ecoregion_ID)

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_ids <- seq_len(length(unique_ids))

# Create a mapping between old and new IDs
id_mapping <- data.frame(old_ID = unique_ids, new_ID = new_ids)

bio2 <- merge(bio, id_mapping, by.x = "ecoregion_ID", by.y = "old_ID", all.x = TRUE)

# Barplot with rotated axis and facets

# results_2020 <- df_comb%>%
#   filter(mtc_nm=="bd.val",
#          ratio=="compared2_2020")%>%
#   ggplot(aes(x = as.factor(ecoID), y = value, fill = status)) +
#     geom_bar(stat = "identity") +
#     labs(title = "Reduction of extinction debt",
#          x = "Ecoregion ID",
#          y = "Rescaled metric (-1;1)",
#          fill = "Condition") +
#     theme_minimal() +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     facet_wrap(~scenario, scales = "free_y")+
#     coord_flip() +
#   scale_fill_manual(values = c("blue","red"))

m <- c("bd.val","ec.val","it.val")
label <- c("Reduction of extinction debt","Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact")
plt_list <- list()
for(p in seq_along(m)){
  results_2050 <- bio2%>%
     filter(name==m[p])%>%
    # criando coluna com cores
    mutate(condition=if_else(value>=0,"improved","worsened"),
      #scaled_value = ifelse(value >= 0, 
      #                           rescale(value, to = c(0, 1)), 
      #                           rescale(value, to = c(-1, 0)))
      scaled_value=value/max(value)) %>%
    ggplot(aes(x = as.factor(new_ID), y = scaled_value
               , fill = condition
               )) +
    geom_bar(stat = "identity") +
    labs(title = label[p],
         x = "Ecoregion ID",
         y = "",
         fill = "Condition") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~scenario, scales = "free_y")+
    coord_flip() +
    scale_fill_manual(values = c("blue","red")) +
    labs(y = "")  # Remove y-axis label here

  plt_list[[p]] <-  results_2050 
}


combined <- ggarrange(plotlist = plt_list,common.legend = T,nrow = 3)

# ggsave(plot = combined,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bd_barplot.png",width = 32,height = 20,units = "cm")
# 
# # adicionando mapa!
# 
# ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv04.shp")
# 
# names(ecoregions) <- c("ecoID","mtc_nm","vlfcnz50","vlbse20","rt20fcnz","vlbse50","rt50fcnz","rt20bse","vlfcnzpls50","rt20fcnzpls","rt50fcnzpls","ecor_nm","geometry")
# 
# coords <-st_coordinates( st_centroid(ecoregions) )
# 
# ecoregions <- cbind(ecoregions,coords)
# 
# # labels
# 
# labels <- unique(ecoregions[,c(1,13,14)])
# 
# labels <- st_drop_geometry(labels)
# 
# 
# library(Polychrome)
# 
# P60 = createPalette(60,  c("#ff0000", "#00ff00", "#0000ff"))
# P60 <- sortByHue(P60)
# P60 <- as.vector(t(matrix(P60, ncol=4)))
# # Extract color codes from the palette
# library(lessR)
# colors <- getColors(P60, 60)
# 
# 
# 
# legend_labels <- unique(ecoregions[,c(1,12)])
# legend_labels$tolegend=paste0(legend_labels$ecoID,":",legend_labels$ecor_nm)
# 
# ecoregions <- ecoregions%>% 
#   mutate(tolegend=paste0(ecoID," :",ecor_nm))
# 
# plot_ecor <- ecoregions%>% 
#   ggplot() +
#   geom_sf( aes(fill = as.factor(ecoID)), color = "darkgray") + # era col_code
#   geom_text_repel(data=labels,aes(x = X, y = Y, label = ecoID),
#                   #hjust = 0, nudge_x = 1, nudge_y = 4,
#                   size = 3, color = "black", fontface = "bold",bg.color = "white",bg.r = 0.25)+
#   theme_map()+
#   scale_fill_manual(values = P60)
#   
# 
# ggsave(plot = plot_ecor,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ecoregions2.png",width = 30,height = 20,units = "cm")
# 
# #plot_ecor + scale_fill_viridis_d()
# 
