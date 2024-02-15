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

ecoregions <- unique(st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shp")[,c(1,12)])

names(ecoregions)[1] <- "ecoregion_ID"

# abrindo resultados

bio <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_results.csv")


# Extract unique IDs
unique_ids <- unique(bio$ecoregion_ID)

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_ids <- seq_len(length(unique_ids))

# Create a mapping between old and new IDs
id_mapping <- data.frame(old_ID = unique_ids, new_ID = new_ids)

bio2 <- merge(bio, id_mapping, by.x = "ecoregion_ID", by.y = "old_ID", all.x = TRUE)

bio2 <- mutate(bio2,condition=if_else(value>0,"improved",if_else(value<0,"worsened","no changes")))


ecoregions2 <- left_join(ecoregions,bio2)

# plotando mapas --------------------------------------------------------------

# brazil countours

Br <- read_country(2019)%>%
  mutate(dissolve=1)%>%
  group_by(dissolve)%>%
  summarise()%>%
  st_transform(st_crs(ecoregions))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")

ecoregions3 <- st_drop_geometry(ecoregions2)
plot(ecoregions3$value~ecoregions3$value_skewed)
# mapas baseline ---------------------------------------------------------------

# Define a function to generate the color range based on the values
generate_color_range <- function(value_skewed) {
  ifelse(value_skewed < 0, paste0("#F8766D", 1 - value_skewed/11),
         ifelse(value_skewed > 0, paste0("#619CFF", value_skewed/2),
                "gray"))
}

# Create a new column "color" based on the color range
ecoregions2$color <- generate_color_range(ecoregions2$value_skewed)

mapa1 <- ecoregions2%>% 
  ggplot() +
  # valor das metricas
  geom_sf( aes(fill = condition), color = "transparent") + # era col_code
  #limite ecorregioes
  geom_sf(data = ecoregions2, color = "white",fill=NA, lwd = 0.1)+
  # biomas
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  # scale_fill_gradient2(low = "#F8766D", mid = "gray", high = "#619CFF", midpoint = 0,breaks=c(-11,-2,-1,-0.5,0,0.1,0.2,0.3,0.4,0.5,1,2))+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  scale_fill_manual(values = c("#619CFF", "gray", "#F8766D"))+
  #ggtitle("fcnz")+
  labs( title = "Biodiversity consequences in 2050", fill = "Relative change" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size=8,angle = 45, hjust = 1),
        axis.text.y = element_text(size=8),
        strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10),
        legend.position = "top",
        axis.title = element_text(size=8)) +
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_grid(scenario~name)#+
  #theme(legend.position = "none")

ggsave(plot = mapa1,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bio_2050.png",width = 18,height = 15,units = "cm")


# Calculate maximum values for each level of the name column
max_values <- st_drop_geometry(ecoregions2) %>%
  group_by(name,scenario) %>%
  summarize(max_value = max(value))

# Merge the maximum values with the original dataframe
ecoregions3 <- ecoregions2 %>%
  left_join(max_values, by = c("name")) %>%
  mutate(scaled_value = value / max_value,
         scaled_value=if_else(is.infinite(scaled_value),0,scaled_value),
         scaled_value=if_else(is.nan(scaled_value),0,scaled_value)) %>%
  select(-max_value)

ecoregions4 <- st_drop_geometry(ecoregions3)

ecoregions3%>% 
  ggplot() +
  # valor das metricas
  geom_sf( aes(fill = scaled_value), color = "transparent") + # era col_code
  #limite ecorregioes
  geom_sf(data = ecoregions2, color = "white",fill=NA, lwd = 0.1)+
  # biomas
  geom_sf(data = biomes, color = "black",fill=NA)+
  #scale_fill_identity() +
  # scale_fill_gradient2(low = "darkred", mid = "lightgray", high = "darkblue", midpoint = 0,breaks=c(-3,0,1))+
  scale_colour_gradient2(
    low = muted("red"),
    mid = "gray",
    high = muted("blue"),
    midpoint = 0
  )+
  #scale_fill_manual(values = ecoregion_vec2$col_code ) +
  #scale_fill_manual(values = c("#619CFF", "gray", "#F8766D"))+
  #ggtitle("fcnz")+
  labs( title = "Biodiversity consequences for 2050", fill = "Scaled values" ) +
  #guides(fill = guide_legend(title = "Ratio Legend"))+
  theme_minimal()+
  theme(axis.text.x = element_text(size=8,angle = 45, hjust = 1),
        axis.text.y = element_text(size=8),
        strip.text.y = element_text(size = 8),
        strip.text.x = element_text(size = 8),
        legend.text = element_text(size = 8),
        plot.title = element_text(size = 10),
        legend.position = "top",
        axis.title = element_text(size=8)) +
  #theme(plot.margin = margin(t = 0, r = 0, b = 0, l = 0,"cm"))+
  facet_grid(scenario~name)#+
#theme(legend.position = "none")
