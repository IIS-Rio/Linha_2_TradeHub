library(terra)

# abrindo raster ecoregioes

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")

# abrindo metricas calculadas

eco_metrics <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv02.shp")

ecoregion_df <- freq(ecoregion)

# ID ecorregioes completo

ID_eco <- unique(ecoregion_df$value)

# ecorregioes pra excluir

excluir <- c(13,257,266,68,208,677,326)

ID_eco <- ID_eco[!ID_eco %in% excluir]

# identificando ecorregioes q precisam ser rodadas!

head(eco_metrics)
summary(eco_metrics)
eco_done <- unique(eco_metrics$ecoID)
eco_miss <- ID_eco[!ID_eco %in% eco_done] # 690 (falta fcnz),727 (falta fcnz)

plot(ecoregion==727)
