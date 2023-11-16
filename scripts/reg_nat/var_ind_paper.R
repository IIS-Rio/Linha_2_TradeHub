# raster binario regeneracao natural - usar mapbiomas (histórico??)
# No paper "Global potential"  usaram variavel calculada pelo global forest watch:
# Pantropical tree plantation expansion (2000-2012)

# lado bom eh q ja ta pronto

# baixando

url <- "https://www.arcgis.com/sharing/rest/content/items/8196f3a8707840188f357d50cfad32d8/data"

destfile = "/dados/projetos_andamento/TRADEhub/Linha_2/pantropical_tree_plant_exp/pantropical_tree_plant_exp.gpkg"

library(curl)
library(fasterize)
curl_download(url, destfile, quiet = FALSE)


#-------------------------------------------------------------------------------

library(rworldmap)

# Get country boundaries
countries <- rworldmap::countriesCoarse




gpkg_file <- "/dados/projetos_andamento/TRADEhub/Linha_2/pantropical_tree_plant_exp/PredEnsRF1_Finp4v3_mergeArea_traindata_bal3passBbDisp.gpkg"

# camadas disponiveis
available_layers <- st_layers(gpkg_file)


# Choose the layer you want to work with (replace 'your_layer_name' with the actual layer name)

# not 1
#not 2
#not 3
# 4 - 15

bandas <- list()

final_raster <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/NatVegCover_100m.tif")

for(i in 4:15){
  
  # abrindo a banda
  selected_layer <- st_read(gpkg_file, layer =paste0( "PredEnsRF1_Finp4v3_mergeArea_traindata_bal3passBbDisp_",i))%>% # only reg
    filter(wt.Pred=="regrowth")%>%
    mutate(to_raster=1)
  
  # criando base raster com 100m
  
  base_raster <- raster(extent(selected_layer), crs = crs(selected_layer),vals=1,res=c(0.008983153,0.008983153))
  
  
  # rasterizando
  
  reg_r <- fasterize(sf = selected_layer,field="to_raster",base_raster)
  
  # transformando pra origem comum
  
  reg_reproj <- projectRaster(from = reg_r,to=final_raster,method = 'ngb')
  
  
  # guarando em uma lista
  
  bandas[[i]] <- reg_reproj 
  
}



# funcao mosaico
combine_rasters <- function(raster_list) {
  # Check if the raster list is empty
  if (length(raster_list) == 0) {
    stop("No rasters to combine.")
  }
  # Combine rasters using mosaic
  combined_raster <- do.call(mosaic, c(raster_list, fun = mean))
  return(combined_raster)
}


# r2resampled <- projectRaster(from = bandas[[4]],to=final_raster,method = 'ngb')
# 
# r2resampled2 <- projectRaster(from = bandas[[5]],to=final_raster,method = 'ngb')

nat_reg_mos <- combine_rasters(bandas[4:15])

# transformando NAs em 0

nat_reg_mos2 <- nat_reg_mos

nat_reg_mos2[is.na(nat_reg_mos2)] <- 0

# mascara

nat_reg_mos2c <- crop(nat_reg_mos2,Br)
nat_reg_mos2m <- mask(nat_reg_mos2c,Br)


writeRaster(nat_reg_mos2m,"/dados/projetos_andamento/TRADEhub/Linha_2/pantropical_tree_plant_exp/natural_reg_Br_Fagan.tif")

# transformar em dataframe, usar todos os valores 1 e sortear valores 0 ou usar mesmo n. valores 0 pra calibrar o modelo. 

n_reg <- cellStats(x = nat_reg_mos2m,na.rm=T,stat = sum)
