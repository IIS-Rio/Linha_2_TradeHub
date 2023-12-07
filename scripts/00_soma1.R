# pacotes ----------------------------------------------------------------------

library(terra)

#-------------------------------------------------------------------------------

# testando soma um

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS"

scens <- list.dirs(p,full.names = F,recursive = F)

rstr_ls <- grep(pattern =paste(scens,collapse = "|") ,x = list.files(p,full.names = T,recursive = T),value = T)

years <- seq(2025,2050,5)

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS"

scens <- list.dirs(p,full.names = F,recursive = F)

rstr_ls <- grep(pattern =paste(scens,collapse = "|") ,x = list.files(p,full.names = T,recursive = T),value = T)

years <- seq(2025,2050,5)

#vegetação, agricultura, pastagem e outros
usos <- c("natural_vegetation","agriculture","pastureland","other")

lis2test <- list()
c=1
for(scen in scens){
  # cenarios
  r_scen <- grep(scen,rstr_ls,value = T)
  for(y in years){
    # anos
    r_years <- grep(y,r_scen,value = T)
    r <- rast(r_years)
    rsum <-sum(r)
    rsum <- round(rsum,2)
    lis2test[[c]] <- rsum
    c=c+1
  }
 
 
}

#-completando soma 1 pros lu regionais -----------------------------------------


p <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use_regional"

dirs <- list.files(p)

for(dir in dirs){
  
  ls_rstr <- list.files(file.path(p,dir),full.names = T)
  soma <- sum(rast(ls_rstr))
  if(minmax(soma)[1]<1){
    r2complete <- 1-soma
    # abrir raster other
    other <- rast(file.path(p,dir,"other.tif"))
    newother <- other+r2complete
    writeRaster(newother,file.path(p,dir,"other.tif"),overwrite=T)
    
  }else{next}
}

# completando soma 1 pros cenarios downscaled ----------------------------------


p2 <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050"

dirs <- list.files(p2)

for(dir in dirs){
  
  ls_rstr <- list.files(file.path(p2,dir),full.names = T)
  soma <- sum(rast(ls_rstr))
  if(minmax(soma)[1]<1){
    r2complete <- 1-soma
    # abrir raster other
    other <- rast(file.path(p2,dir,"other.tif"))
    newother <- other+r2complete
    writeRaster(newother,file.path(p2,dir,"other.tif"),overwrite=T)
    
  }else{next}
}