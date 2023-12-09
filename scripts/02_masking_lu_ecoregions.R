# clipar os lu do brasil por cada ecorregiao

# pacotes ----------------------------------------------------------------------
library(terra)
#-------------------------------------------------------------------------------

# caminho dos Lus

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020"

ls_rster <- rast(list.files(p,full.names = T))

rster <- list.files(p,full.names = F)
rster <- gsub(pattern = ".tif",replacement = "",x = rster)

ecoregions <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/ecoregions_wwf_plusCerrado.tif")

unique_regions <- unique(values(ecoregions), na.rm = TRUE)
unique_regions <- unique_regions[!is.nan(unique_regions)]

for (region_value in unique_regions) {
  # Create a mask for the current region
  region_mask <- ecoregions == region_value
  
  # Mask the large raster based on the current region
  #masked_raster <- mask(ls_rster[[1]], region_mask)
  for(i in seq_along(rster)){
    masked_raster <- ls_rster[[i]]*region_mask
  # Export the masked raster to a file (replace 'output_path' with your desired file path)
    dirpath <- paste0("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use_regional/ecoregion_",region_value,"/")
    dir.create(dirpath)
    output_filename <- paste0(dirpath,rster[i], ".tif")
    writeRaster(masked_raster, filename = output_filename, overwrite = TRUE,gdal=c("COMPRESS-DEFLATE"))
  
    }
  
  }




