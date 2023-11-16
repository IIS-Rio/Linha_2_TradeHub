
p <- "/dados/projetos_andamento/TRADEhub/Linha_2/soil_data"

soc <- stack(list.files(file.path(p,"soc","mosaico"),full.names = T))
soc_mean <- calc(soc,mean)

terra::writeRaster(soc_mean,file.path(p,"soc","mosaico","mean_soc_5_30cm.tif"))

ph <- stack(list.files(file.path(p,"phh2o","mosaico"),full.names = T))
ph_mean <- calc(ph,mean)
writeRaster(soc_mean,file.path(p,"phh2o","mosaico","mean_ph_5_30cm.tif"))
