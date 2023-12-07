library(terra)

# a partir dos rasters fornecidos pelo arthur, separar em usos por cenario

# multiband rasters

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS"

scens <- list.dirs(p,full.names = F,recursive = F)

rstr_ls <- grep(pattern =paste(scens,collapse = "|") ,x = list.files(p,full.names = T,recursive = T),value = T)

years <- seq(2025,2050,5)

base_r <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/agbgC_current_tonha_v6.1_BR.tif")
crscopy <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS/pa_br_usoterra_mapbiomas8_30m_2020_reclassified_albers_100m.tif")
crs <- crs(crscopy)

#vegetação, agricultura, pastagem e outros
usos <- c("nat_veg","agriculture","pastureland","other")

for(scen in scens){
  # cenarios
  r_scen <- grep(scen,rstr_ls,value = T)
  for(y in years){
    # anos
    r_years <- grep(y,r_scen,value = T)
    r <- rast(r_years)
    for(lu in seq_along(usos)){
      rtosave <- r[[lu]]
      crs(rtosave) <- crs
      rtosavepj <- project(rtosave,base_r)
      # faltou adicionar a pasta land_use
      ytosave <- file.path("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata",y)
      dir.create(ytosave)
      scentosave <- file.path(ytosave,scen)
      dir.create(scentosave)
      writeRaster(rtosavepj,file.path(scentosave,paste0(usos[[lu]],".tif")), gdal=c("COMPRESS=DEFLATE"),overwrite=T)
    
    }
    
  }
  
}



  
  