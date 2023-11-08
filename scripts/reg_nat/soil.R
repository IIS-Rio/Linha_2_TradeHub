
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

# baixar solo 3 3 camadas SOC

#Note the CRS of stder. As the ISRIC layers use the Homolosine projection, we need to reproject our layer using the sf library:

# pro brasil todo nao ta indo. tentar por bioma(cer=3,ma=4,pampa=5,pantanal=6)
igh='+proj=igh +lat_0=0 +lon_0=0 +datum=WGS84 +units=m +no_defs'
#br <- read_country()# Br todo
#am <- read_amazon() # amazonia legal

biomes <- read_biomes() # cerrado
biomes_igh <- st_transform(biomes, igh)

regioes_nm <- c("am","caat","cer","ma","pampa","pantanal")

# lista de profundidades 

profundidades <- c("0-5cm","5-15cm","15-30cm")

# variavel de interesse
# soc = # soil organic carbon 
# phh2o = pH

voi = "phh2o"

for(i in 1:length(regioes_nm)){
  for(j in 1:length(profundidades)){
    
    reg <- biomes_igh[i,]
    #bounding box of our area of interest:
    # variavel interesse
    #voi = "soc" 
    depth = profundidades[j]
    quantile = "mean"
    
    bbox <- st_bbox(reg)
    
    # Create a rectangular polygon from the bounding box
    
    # Now, let’s use the bbox data to define the boundary box limits as used by the GDA libraryL. By the way, this is one of the trickiest parts of using GDAL.
    
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
    l1.l <- newXMLNode("CoverageName", paste0(voi,"_",depth,"_mean"), parent=l1)
    
    # Save to local disk
    xml.out = paste0("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/",voi,"/sg.xml")
    
    saveXML(l1, file = xml.out)
    
        file.out <- paste0("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/",voi,"/",regioes_nm[i],"_",voi,"_",depth,"_mean.tif")
    
    gdal_translate(xml.out, file.out,
                   tr=c(250,250), projwin=bb,
                   projwin_srs =igh, co=c("TILED=YES","COMPRESS=DEFLATE","PREDICTOR=2","BIGTIFF=YES")
    )
    
    
  }
}


