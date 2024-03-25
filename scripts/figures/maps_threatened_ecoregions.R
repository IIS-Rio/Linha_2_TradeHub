library(geobr)
library(ggmap)
library(ggpubr)
library(ggrepel)
# mapas das ecorregioes mais ameaçadas no cenario base

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

# lolipop chart
# 1 pra cada metrica

m <- c("bd.val","ec.val","it.val")
label <- c("Reduction of extinction debt","Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact")
scenarios <- c("baseline","fcnz","fcnzplus")

# Calculate maximum values for each level of the name column
max_values <- bio2 %>%
  group_by(name) %>%
  summarize(max_value = max(value))

# Merge the maximum values with the original dataframe
bio2 <- bio2 %>%
  left_join(max_values, by = "name") %>%
  mutate(scaled_value = value / max_value) %>%
  dplyr::select(-max_value)

eco_table <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

names(eco_table)[2] <-"ecoregion_ID" 

bio3 <- left_join(bio2,eco_table)

bio3$condition <- factor(bio3$condition,levels = c("improved","worsened","no changes"))


bio2$condition <- factor(bio2$condition,levels = c("improved","worsened","no changes"))

# Calculate maximum values for each level of the name column
max_values <- bio3 %>%
  group_by(name,scenario) %>%
  summarize(max_value = max(abs(value)))

# Merge the maximum values with the original dataframe
bio4 <-bio3 %>%
  left_join(max_values, by = c("name","scenario")) %>%
  mutate(scaled_value = value / max_value,
         scaled_value=if_else(is.infinite(scaled_value),0,scaled_value),
         scaled_value=if_else(is.nan(scaled_value),0,scaled_value)) %>%
  dplyr::select(-max_value)

# Calculate the sum of scaled_values for each scenario and name
bio4[, sum_scaled := sum(scaled_value), by = .(scenario, name)]

# Calculate the ratio based on condition
bio4[, ratio := ifelse(condition == "improved", scaled_value / sum_scaled, scaled_value / abs(sum_scaled))]



# Calculate cumulative sum of ratio
# Calculate cumulative sum of ratio per scenario and name
bio4 <- bio4 %>%
  group_by(scenario, name) %>%
  arrange(ratio) %>%
  mutate(cumulative_ratio = cumsum(ratio))

# data frame com as variaveis q acumulam 0.5 da importancia.
#
trshld <- bio4%>%
  filter(scenario=="base_2050",
         cumulative_ratio>=-0.51)

# ecorregioes espacial

ec_sp <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shp")

ec_sp <- ec_sp[unique(ec_sp$ecoID),]

ec_sp <- unique(ec_sp[,1])

ec_sp2 <- ec_sp%>%filter(ecoID %in% trshld$ecoregion_ID)

ec_sp3 <- left_join(trshld,ec_sp2,by=c("ecoregion_ID"="ecoID"))

ec_sp3 <- st_as_sf(ec_sp3)


br <- read_country(2020)%>%
  mutate(summarise=1)%>%
  group_by(summarise)%>%
  summarise()%>%
  st_transform(crs = st_crs(ec_sp3))

biomes <- read_biomes(2019)%>%
  st_transform(crs = st_crs(ec_sp3))%>%
  filter(name_biome!="Sistema Costeiro")



# plotar por metricas


centroid <- st_centroid(ec_sp3)
centroid <- cbind(centroid,st_coordinates(centroid))
# bd

color_palette <- terrain.colors(n = 60)

metricas <- c("bd.val","ec.val","it.val")
labels <- c("Reduction of extinction debt","Reduction in ecosystem vulnerability","Reduction of ecosystem integrity impacts")




plt_lst <- list()

for(m in seq_along(metricas)){
  # definindo rotulos ecorregioes
  centroid_s <- filter(centroid,name==metricas[m])
  centroid_s$new_ID <- as.character(centroid_s$new_ID)
  p <- ec_sp3 %>%
    filter(name==metricas[m])%>%
    ggplot() +
    geom_sf(data = biomes,fill = "lightgray", color = "black")+
    scale_fill_gradientn(colors = rev(brewer.pal(9, "Reds"))) +
    geom_sf(aes(fill = scaled_value), color = "white")+  
    geom_sf(data = biomes,fill = "transparent", color = "black")+
    geom_text_repel(data = centroid_s,aes(x = X, y = Y, label = new_ID),
                    hjust = 0, nudge_x = 1, nudge_y = 4,
                    size = 3, color = "black", fontface = "bold",bg.color = "white",bg.r = 0.25)+
    theme(legend.text = element_text(size=10),
          legend.position = "right")+
    xlab("")+
    ylab("")+
    theme_minimal()+
    labs(fill = "Scaled value")+
    ggtitle(labels[m])
  plt_lst[[m]] <- p    
}

plt_lst[[1]]
plt_lst[[2]]
plt_lst[[3]]

panel <- ggarrange(plotlist = plt_lst,common.legend = T,nrow = 3)
panel2 <- ggarrange(plotlist = plt_lst,common.legend = T)


ggsave(plot = panel, "/dados/pessoal/francisco/Linha_2_TradeHub/figures/threathened__ecoregions1column.png",width = 10,height = 20,scale = 1)
ggsave(plot = panel2, "/dados/pessoal/francisco/Linha_2_TradeHub/figures/threathened__ecoregions2column.png",width = 18,height = 15,scale = 1)
