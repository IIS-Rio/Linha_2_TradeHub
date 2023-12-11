# pacotes ----------------------------------------------------------------------

library(terra)

#-------------------------------------------------------------------------------

# abrindo mudancas uso terra

lst_rstrs <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff",full.names = T)

snrs_nms <- c("baseline_2050","fcnz_2050","fcnzplus_2050")

# ------------------------------------------------------------------------------

# 
restored_areas <- grep("rest",lst_rstrs,value=T)
converted_areas <- grep("conve",lst_rstrs,value=T)
conserved_areas <- grep("conse",lst_rstrs,value=T)

# ecorregioes

ecoregions <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")

# custo oportunidade da restauracao (usando raster com conservacao tb, pra testar)

oc_con <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/conservation_oc_reais_ha_2050.tif")

ic_costs <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_implementation_cost_reais_ha.tif")

unique_regions <- unique(values(ecoregions), na.rm = TRUE)
unique_regions <- unique_regions[!is.nan(unique_regions)]

lst_restored_areas <- list()
lst_converted_areas <- list()
lst_conserved_areas <- list()
lst_ic_areas <- list()

c=1
for (region_value in unique_regions) {
  # Create a mask for the current region
  region_mask <- ecoregions == region_value
  # Mask the large raster based on the current region
  #masked_raster <- mask(ls_rster[[1]], region_mask)
  for(i in seq_along(restored_areas)){
    r <- rast(restored_areas[[i]])
    r_conv <- rast(converted_areas[[i]])
    # como o raster tem valor zero, precisa adicionar o 1.
    r_conserved <- rast(conserved_areas[[i]])+1
    # transformar em qntidade e computar em tabela
    masked_raster <- r*region_mask
    masked_raster_conv <- r_conv*region_mask
    masked_raster_cons <- r_conserved*region_mask
    # ha restaurados
    masked_raster_area <- masked_raster*100
    masked_raster_conv_area <- masked_raster_conv*100
    masked_raster_cons_area <- masked_raster_cons*100
    # pensar em como combinar info. restauracao com carbono!
    oc_restored <- oc_con*masked_raster_area
    ic_restored <- ic_costs*masked_raster_area
    oc_converted <- oc_con*masked_raster_conv_area
    oc_conserved <- oc_con*masked_raster_cons_area
    total_oc_restored <- sum(oc_restored[],na.rm = T)
    total_ic_restored <- sum(ic_restored[],na.rm = T)
    total_oc_converted <- sum(oc_converted[],na.rm = T)
    total_oc_conserved <- sum(oc_conserved[],na.rm = T)
    df <- data.frame(scen=snrs_nms[i],oc_restoration=total_oc_restored,ecoregion_ID=region_value)
    df2 <- data.frame(scen=snrs_nms[i],oc_conversion=total_oc_converted,ecoregion_ID=region_value)
    df3 <- data.frame(scen=snrs_nms[i],oc_convserved=total_oc_conserved,ecoregion_ID=region_value)
    df4 <- data.frame(scen=snrs_nms[i],ric_convserved=total_ic_restored,ecoregion_ID=region_value)
    
    lst_restored_areas[[c]] <- df
    lst_converted_areas[[c]] <- df2
    lst_conserved_areas[[c]] <- df3
    lst_ic_areas[[c]] <- df4
    c=c+1
  }
}

oc_restored_df <- do.call(rbind,lst_restored_areas)
oc_converted_df <- do.call(rbind,lst_converted_areas)
oc_conserved_df <- do.call(rbind,lst_conserved_areas)
ic_df <- do.call(rbind,lst_ic_areas)

write.csv(oc_restored_df,"/dados/projetos_andamento/TRADEhub/Linha_2/oc/oc_restored_ecoregion.csv",row.names = F)

write.csv(oc_converted_df,"/dados/projetos_andamento/TRADEhub/Linha_2/oc/oc_converted_ecoregion.csv",row.names = F)

write.csv(oc_conserved_df,"/dados/projetos_andamento/TRADEhub/Linha_2/oc/oc_conserved_ecoregion.csv",row.names = F)

write.csv(ic_df,"/dados/projetos_andamento/TRADEhub/Linha_2/oc/implementation_costs_ecoregion.csv",row.names = F)