# a partir dos rasters fornecidos pelo arthur, separar em usos por cenario

# multiband rasters

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS"

scens <- list.dirs(p,full.names = F,recursive = F)

rstr_ls <- grep(pattern =paste(scens,collapse = "|") ,x = list.files(p,full.names = T,recursive = T),value = T)

years <- seq(2025,2050,5)

#vegetação, agricultura, pastagem e outros
usos <- c("natural_vegetation","agriculture","pastureland","other")

for(scen in scens){
  # cenarios
  r_scen <- grep(scen,rstr_ls,value = T)
  for(y in years){
    # anos
    r_years <- grep(y,r_scen,value = T)
    r <- rast(r_years)
    for(lu in seq_along(usos)){
      rtosave <- r[[lu]]
      ytosave <- file.path("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata",y)
      dir.create(ytosave)
      scentosave <- file.path(ytosave,scen)
      dir.create(scentosave)
      writeRaster(rtosave,file.path(scentosave,paste0("downscaled_",scen,"_",y,"_",usos[[lu]],".tif")), gdal=c("COMPRESS=DEFLATE"),overwrite=T)
    
    }
    
  }
  
}



  
  