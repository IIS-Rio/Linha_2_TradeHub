
ref <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/agbgC_current_tonha_v6.1_BR.tif")

ec <- st_make_valid(unique(st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shp")[,c(1,12)])%>%st_transform(crs(ref)))

biomes <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")%>%st_transform(crs(ref))

biomes$area <- as.numeric(st_area(biomes))/10^4

# adicionar ids corretos!!

# Extract unique IDs
unique_ids <- unique(ec$ecoID)

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_ids <- seq_len(length(unique_ids))

# Create a mapping between old and new IDs
id_mapping <- data.frame(old_ID = unique_ids, new_ID = new_ids)

ec2 <- merge(ec, id_mapping, by.x = "ecoID", by.y = "old_ID", all.x = TRUE)

ec2$area_eco <- as.numeric(st_area(ec2))/10^4

int <- st_intersection(ec2,biomes)

int$area_ha <- as.numeric(st_area(int))/10^4

# adicionando area total

int_df <- st_drop_geometry(int)

int_df <- left_join(int_df,st_drop_geometry(biomes))

int_df <- left_join(int_df,st_drop_geometry(ec2[,c(1,5)]))


# % area (area relativa da ecoregiao que esta no bioma, nao area do bioma)

int_df$relative_area <- int_df$area_ha/int_df$area_eco


ecoregion_counts <- int_df %>%
  group_by(ecoID,eco_nm) %>%
  summarize(ecoregion_count = n())


ecoregions_multiple_rows <- ecoregion_counts %>%
  filter(ecoregion_count > 1) %>%
  pull(ecoID)

# filtrar areas menores q 0.05!

int_df2 <- int_df %>%
  filter(relative_area > 0.05 | ecoID %in% ecoregions_multiple_rows)

collapsed_df <- int_df2%>%
  group_by(ecoID,eco_nm) %>%
  summarize(biomes = paste(unique(name_biome), collapse = ", "))


collapsed_df <- left_join(collapsed_df,st_drop_geometry(ec2[,c(1,3)]))


write.csv(collapsed_df,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/intersect_biomes.csv",row.names = F)
