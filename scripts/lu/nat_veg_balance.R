#ecoregions

eco <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv03.shp")%>%
  group_by(ecoID)%>%
  summarise()

converted <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff",pattern = "conversion",full.names = T)

restored <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff",pattern = "restored",full.names = T)


scens <- c("baseline_2050","fcnz_2050","fcnzplus_2050")

# calculando area convertida ---------------------------------------------------

convertion_list <- list()

for(i in seq_along(scens)){
  r <- rast(converted[i])
  # transformar em area
  r_area <- r*(1000*1000/10000)
  zonal <- as.data.frame(terra::extract(x = r_area,y = eco,fun=sum,na.rm=T,ID=T,bind=T))%>%
    dplyr::rename(cvtd_ha=layer.1)%>%
    mutate(scenario=scens[i])
  convertion_list[[i]] <- zonal
}

convertion_df <- as.data.frame(do.call(rbind,convertion_list))

# calculando area restaurada ---------------------------------------------------

restoration_list <- list()

for(i in seq_along(scens)){
  r <- rast(restored[i])
  # transformar em area
  r_area <- r*(1000*1000/10000)
  zonal <- as.data.frame(terra::extract(x = r_area,y = eco,fun=sum,na.rm=T,ID=T,bind=T))%>%
    dplyr::rename(rstrd_ha=layer.1)%>%
    mutate(scenario=scens[i])
  restoration_list[[i]] <- zonal
}

restoration_df <- do.call(rbind,restoration_list)


# combinando os 2

lu_chng <- left_join(convertion_df,restoration_df)

# calculando net

lu_chng <- lu_chng%>%
  mutate(net_veg_ha=cvtd_ha+rstrd_ha)

# salvando

write.csv(lu_chng,"/dados/projetos_andamento/TRADEhub/Linha_2/lu_results/net_veg_balance.csv",row.names = F)

