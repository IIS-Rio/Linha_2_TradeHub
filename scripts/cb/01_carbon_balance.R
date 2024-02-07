# pacotes-----------------------------------------------------------------------

library(terra)

#-------------------------------------------------------------------------------
# balanco carbono

lst_rstrs <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff",full.names = T)

snrs_nms <- c("baseline_2050","fcnz_2050","fcnzplus_2050")

# sequestro carbono via restauracao --------------------------------------------

restored_areas <- grep("rest",lst_rstrs,value=T)

# ecorregioes

ecoregions <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")

# carbono restaracao

cb_rest <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/delta_agbgC_restor_tonha_v6.1_BR.tif")

unique_regions <- unique(values(ecoregions), na.rm = TRUE)
unique_regions <- unique_regions[!is.nan(unique_regions)]

lst_restored_areas <- list()
c=1
for (region_value in unique_regions) {
  # Create a mask for the current region
  region_mask <- ecoregions == region_value
  # Mask the large raster based on the current region
  #masked_raster <- mask(ls_rster[[1]], region_mask)
  for(i in seq_along(restored_areas)){
    r <- rast(restored_areas[[i]])
    # transformar em qntidade e computar em tabela
    masked_raster <- r*region_mask
    # ha restaurados
    masked_raster_area <-round( masked_raster*100)
    # pensar em como combinar info. restauracao com carbono!
    carbon_restored <- cb_rest*masked_raster_area
    total_carbon_restored <- sum(carbon_restored[],na.rm = T)
    # convert to CO2
    convertion_rate <- 44/12
    total_CO2_restored <- total_carbon_restored*convertion_rate
    df <- data.frame(scen=snrs_nms[i],CO2_sequestered=total_CO2_restored,ecoregion_ID=region_value)
    lst_restored_areas[[c]] <- df
    c=c+1
  }
}

restored_df <- do.call(rbind,lst_restored_areas)

write.csv(restored_df,"/dados/projetos_andamento/TRADEhub/Linha_2/cb/CO2_sequestered_per_ecoregion.csv",row.names = F)


# emissao de carbono -----------------------------------------------------------

# carbono restaracao

cb_emission <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/agbgC_current_tonha_v6.1_BR.tif")


converted_areas <- grep("conversion",lst_rstrs,value=T)

lst_converted_areas <- list()
c=1
for (region_value in unique_regions[1:4]) {
  # Create a mask for the current region
  region_mask <- ecoregions == region_value
  # Mask the large raster based on the current region
  #masked_raster <- mask(ls_rster[[1]], region_mask)
  for(i in seq_along(converted_areas)){
    r <- rast(converted_areas[[i]])
    # transformar em qntidade e computar em tabela
    masked_raster <- r*region_mask
    # ha desmatamdos
    masked_raster_area <- round(masked_raster*100)
    # pensar em como combinar info. restauracao com carbono!
    carbon_emited <- cb_emission*masked_raster_area
    total_carbon_emited <- sum(carbon_emited[],na.rm = T)
    # convert to CO2
    convertion_rate <- 44/12
    total_CO2_emited <- total_carbon_emited*convertion_rate
    df <- data.frame(scen=snrs_nms[i],CO2_emited=total_CO2_emited,ecoregion_ID=region_value)
    lst_converted_areas[[c]] <- df
    c=c+1
  }
}

emission_df <- do.call(rbind,lst_converted_areas)

write.csv(emission_df,"/dados/projetos_andamento/TRADEhub/Linha_2/cb/CO2_emited_per_ecoregion.csv",row.names = F)
