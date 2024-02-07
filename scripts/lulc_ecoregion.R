library(terra)
library(tidyverse)

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")

convertion_ls <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/",pattern = "convers",full.names = T)

restored_ls <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/",pattern = "restored",full.names = T)

unique_ID <- unique(terra::values(ecoregion))[2:68]
                
scenarios <- c("baseline","fcnz","fcnzplus")

area_pixel <- (1000*1000)/10000

conv_list <- list()
c <- 1
for(id in seq_along(unique_ID)){
  for (s in scenarios){
    con <- round(rast(grep(pattern = s,x = convertion_ls,value = T)),2)
    res <- round(rast(grep(pattern = s,x = restored_ls,value = T)),2)
    # multiplicando
    eco_sub <- ecoregion 
    eco_sub[eco_sub!=unique_ID[id]] <- NA
    eco_sub[!is.na(eco_sub)] <- 1
    eco_area <- sum(eco_sub[],na.rm = T)*area_pixel
    con_area <- con*area_pixel
    res_area <- res*area_pixel
    conv_area <- eco_sub*con_area
    rest_area <- eco_sub*res_area
    area_total <- sum(conv_area[],na.rm=T)
    area_total_res <- sum(rest_area[],na.rm=T)
    df <- data.frame(area_conv_ha=area_total,area_rest_ha=area_total_res,scenario=s,ecoregion=unique_ID[id],eco_area)
    conv_list[[c]] <- df
    }
  c <- c+1
}

lulc_areas <- do.call(rbind,conv_list)

write.csv(lulc_areas,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/lulc_ecoregions.csv",row.names = F)