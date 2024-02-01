# pacotes ----------------------------------------------------------------------

library(vroom)
library(terra)
# ------------------------------------------------------------------------------

# bd Br brasil (checar aqui se ta certo o endereco!)

bdbr = vroom("/dados/pessoal/luga/dev/BD-BR/data/database_versions/BD_BR_taxon_v0.1.csv.gz")

# Raster base ------------------------------------------------------------------
base_ras = rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use_regional/ecoregion_1/agriculture.tif")

# Lendo rasters de espécies ----------------------------------------------------
sp_ids = unique(bdbr$spp_id)

t0 = Sys.time()
for (iter in seq_along(sp_ids)) {
  cat(iter, "/", length(sp_ids), '\r')
  
  sp_raster_path = paste0("/dados/projetos_andamento/BD-BR/enm/", sp_ids[iter],"/present/ensemble/", sp_ids[iter], "_ensemble_0.5_consensus.tif")
  if(!file.exists(sp_raster_path)) {next}
  sp_raster = rast(sp_raster_path)
  sp_raster_reproj = project(x = sp_raster, y = base_ras, method = 'near')
  sp_raster_crop = mask(x = crop(x = sp_raster_reproj, y = base_ras), mask = base_ras)
  
  writeRaster(x = sp_raster_crop,
              filename = paste0("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/species/", sp_ids[iter], ".tif"),
              gdal = "COMPRESS=DEFLATE",
              overwrite = T)
}

Sys.time() - t0
hahah