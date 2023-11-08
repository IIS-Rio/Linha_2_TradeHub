# WorldClim dataset 93 which includes 19 bioclimatic variables derived from the monthly temperature and rainfall values. They are 30 arc-second resolution (approximately 1-km) rasters and include: (i) Annual Mean Temperature; (ii) Mean Diurnal Range; (iii) Isothermality; (iv) Temperature Seasonality; (v) Max Temperature of Warmest Month; (vi) Min Temperature of Coldest Month; (vii) Temperature Annual Range; (viii) Mean Temperature of Wettest Quarter; (ix) Mean Temperature of Driest Quarter; (x) Mea
# 
# Page 15/27 Temperature of Warmest Quarter; (xi) Mean Temperature of Coldest Quarter; (xii) Annual Precipitation; (xiii) Precipitation of Wettest Month; (xiv) Precipitation of Driest Month; (xv) Precipitation Seasonality (Coe cient of Variation); (xvi) Precipitation of Wettest Quarter; (xvii) Precipitation of Driest Quarter; (xviii) Precipitation of Warmest Quarter; and (xix) Precipitation of Coldest Quarter



p <- "/dados/bd_iis/WorldClim"

world_clim_var <- list.files(p,pattern=".tif",full.names = T)
world_clim_nms <- list.files(p,pattern=".tif",full.names = F)

# recortar pro Br. E ajustar tb a resolucao

br <- read_country()
pj='+proj=longlat +datum=WGS84 +no_defs '
br_pj <- st_transform(br,pj)

for(i in 1:length(world_clim_var)){
  
  #print(r)
  r <- raster(world_clim_var[[i]])
  rc <- crop(r,br_pj)
  rm <- mask(rc,br_pj)
  writeRaster(rm,file.path("/dados/projetos_andamento/TRADEhub/Linha_2/climate",paste0("BR_",world_clim_nms[[i]])))

  }
