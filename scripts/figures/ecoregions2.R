library(ggthemes)
library(ggmap)
library(viridis)
library(sf)
library(geobr)
library(ggrepel)

ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shx")[,1]

names(ecoregions)[1] <- "ID"

legenda <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")


ecoregions <- unique(left_join(ecoregions,legenda))


unique_ids <- unique(ecoregions$ID)

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_ids <- seq_len(length(unique_ids))

# Create a mapping between old and new IDs
id_mapping <- data.frame(old_ID = unique_ids, new_ID = new_ids)

ecoregions2 <- merge(ecoregions, id_mapping, by.x = "ID", by.y = "old_ID", all.x = TRUE)

Br <- read_country()%>%st_transform(crs(eco_sub))
biomes <- read_biomes()%>% st_transform(crs(ecoregions_sub))

# subset ecoregions 

excluir <- c(13,257,266,68,208,677,326) # IDs para excluir

ecoregions_sub <- ecoregions

# da pra plotar com recorte de bioma. isso facilitaria a vizualizacao! (menos cores)
ecoregions_sub[ecoregions_sub%in%excluir] <- NA

color_palette <- terrain.colors(60)
plot(ecoregions_sub, col=color_palette, main="ecoregions", legend=F)
plot(st_geometry(biomes[1:6,]),add=T)
# Define the color palette
color_palette <- viridis(60)
# associar ecoregiao ao bioma em q ela tem maior area inserida!
# Plot the map

# Calculate the centroid of the multipolygons
centroid <- st_centroid(ecoregions2)

ecoregions2$label <- paste(ecoregions2$new_ID,ecoregions2$Nome)
# Define the color palette
color_palette <- terrain.colors(n = 60)
centroid <- cbind(centroid,st_coordinates(centroid))

ecomaps <- ggplot() +
  geom_sf(data = ecoregions2, aes(fill = label), color = "black") +
  #scale_fill_discrete(colours = color_palette) +
  scale_fill_manual(values = color_palette) +
  theme_map()+
  #theme(legend.position = "none")+
  geom_text_repel(data = centroid,aes(x = X, y = Y, label = new_ID),
                  hjust = 0, nudge_x = 1, nudge_y = 4,
                  size = 3, color = "black", fontface = "bold",bg.color = "white",bg.r = 0.25)+
  guides(fill = guide_legend(
    nrow =30 ,       # Number of rows
    byrow = TRUE,   # Arrange items by row
    title.position = "top"  # Position title at the top
  ))+
  theme(legend.text = element_text(size=10),
        legend.position = "right")


# draw text labels


ggsave(plot = ecomaps, "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ecoregions_with_names.png",width = 24,height = 15,scale = 1)
