
#https://git.wur.nl/isric/soilgrids/soilgrids.notebooks/-/blob/master/markdown/wcs_from_R.md

#- libraries -------------------------------------------------------------------

library(XML)
library(rgdal)
library(gdalUtilities)
# library(raster)
# library(stars)
library(geobr)
library(sf)
library(dplyr)
#library(RColorBrewer)
#library(mapview)
#library(tmap)
#-------------------------------------------------------------------------------

# baixar solo 3 3 camadas

#Note the CRS of stder. As the ISRIC layers use the Homolosine projection, we need to reproject our layer using the sf library:

# pro brasil todo nao ta indo. tentar por bioma(cer=3,ma=4,pampa=5,pantanal=6)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
#br <- read_country()# Br todo
am <- read_amazon() # amazonia legal
am_igh <- st_transform(am, igh)
cer <- read_biomes()%>% # cerrado
  filter(code_biome==3)
cer_igh <- st_transform(cer, igh)
# pegar sul e nordeste
ma <- read_biomes()%>% # mata atlantica
  filter(code_biome==4)
ma_igh <- st_transform(ma, igh)
pam <- read_biomes()%>% # pampa
  filter(code_biome==5)
pam_igh <- st_transform(pam, igh)
#SP <- read_state(code_state = 35) # teste pra SP
# projecao dos dados de soc
#br_igh <- st_transform(br, igh)
#SP_igh <- st_transform(SP, igh)
am_igh <- st_transform(am, igh)

# inserindo em lista pra fazer loop

regioes <- list(am_igh,cer_igh,ma_igh,pam_igh)
regioes_nm <- c("legal_am","cerrado","ma","pampa")

# lista de profundidades (15-30 foi na unha!)

profundidades <- c("0-5cm","5-15cm")
c=1

for(reg in regioes){
  for(prof in profundidades){
    #bounding box of our area of interest:
    # br - am
    # variavel interesse
    voi = "soc" # soil organic carbon 
    depth = prof
    quantile = "mean"
    # Calculate the non-overlapping part of Brazil (didn't work so will get biomes and than mosaic)
    #br_without_am <- st_difference( br,am) nao funfou
    #bbox <- st_bbox(SP_igh)
    #bbox <- st_bbox(br_igh)
    #bbox <- st_bbox(am_igh)
    #bbox <- st_bbox(br_without_am)
    #bbox <- st_bbox(cer_igh)
    #bbox <- st_bbox(ma_igh)
    #bbox <- st_bbox(pam_igh)
    bbox <- st_bbox(reg)
    #
    # Create a rectangular polygon from the bounding box
    #bbox_polygon <- st_as_sfc(st_bbox(bbox))
    
    # Plot the bounding box
    # plot(bbox_polygon, col = "blue", border = "blue", lwd = 2, xlim = c(0, 1), ylim = c(0, 1),add=T)
    
    
    # Now, let’s use the bbox data to define the boundary box limits as used by the GDA libraryL. By the way, this is one of the trickiest parts of using GDAL.
    
    ## ul means upper left
    ## lr means lower right
    ulx = bbox$xmin
    uly = bbox$ymax
    lrx= bbox$xmax
    lry = bbox$ymin
    bb <- c(ulx, uly, lrx, lry)
    
    wcs_path = paste0("https://maps.isric.org/mapserv?map=/map/",voi,".map") # Path to the WCS. See maps.isric.org
    wcs_service = "SERVICE=WCS"
    wcs_version = "VERSION=2.0.1"
    
    #We paste all the components of the WCS request:
      
    wcs = paste(wcs_path,wcs_service,wcs_version,sep="&") # This works for gdal >= 2.3
    
    l1 <- newXMLNode("WCS_GDAL")
    l1.s <- newXMLNode("ServiceURL", wcs, parent=l1)
    l1.l <- newXMLNode("CoverageName", paste0("soc_",prof,"_mean"), parent=l1)
    
    # Save to local disk
    xml.out = "/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/sg.xml"
    
    saveXML(l1, file = xml.out)
    
    # Download raster as GeoTIFF (Warning: it can be large!)
    
    # file.out <- "/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/legal_am_soc_15-30cm_mean.tif"
    # 
    # file.out <- "/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/cerrado_soc_15-30cm_mean.tif"
    # 
    # file.out <- "/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/ma_soc_15-30cm_mean.tif"
    
    file.out <- paste0("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/",regioes_nm[c],"_soc_",prof,"_mean.tif")
    
    gdal_translate(xml.out, file.out,
                   tr=c(250,250), projwin=bb,
                   projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES")
    )
    
    
  }
  
  c=c+1
}
# plot(st_geometry(br_igh))
# plot(raster("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/cerrado_soc_15-30cm_mean.tif"),add=T)
# plot(raster("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/legal_am_soc_15-30cm_mean.tif"),add=T)
# plot(raster("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/ma_soc_15-30cm_mean.tif"),add=T)
# 
# plot(raster("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/pampa_soc_15-30cm_mean.tif"),add=T)


    
