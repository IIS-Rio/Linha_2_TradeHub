# nao estou mais reprojetando
r2020 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS/base/multiband-2020-low.tif")

soma1 <- round(sum(r2020),2)
plot(soma1)
#vegetação, agricultura, pastagem e outros
usos <- c("nat_veg","agriculture","pastureland","other")

baser <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/fcnz/agriculture.tif")
crs <- crs(baser)

for(u in seq_along(usos)){
  lu <- r2020[[u]]
  lu <- round(lu,2)
  # adequando projecao
  #lupj <- project(lu,baser,"cubicspline")
  writeRaster(lu,file.path("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020",paste0(usos[u],".tif")),overwrite=T)
}
