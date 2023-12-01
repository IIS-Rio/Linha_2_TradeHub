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



