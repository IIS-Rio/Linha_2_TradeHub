# pacotes ----------------------------------------------------------------------

library(terra)

#-------------------------------------------------------------------------------

rbase <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/species/FAU_E_100.tif")

veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif")

npv_2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_oc_reais_ha_2050.tif")

# calculando oc conservacao:

vegmask <- veg
# usar um limiar: 0.9
vegmask[vegmask>=0.9] <- NA
vegmask[vegmask<0.9] <- 1

# esse eh o co restauracao
npv_2050_mkd <- npv_2050*vegmask
#npv_2050_mkd[is.na(veg)] <- -1


# o de conservacao pode ser esse completo com os NAS!fazendo focal.

source("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/oc/na_filling_function.R")

# valoe minimo igual ao valor do raster de entrada

# Create a small subset containing pixels with values and NAs
# Determine the extent of the small square (replace with your coordinates)
# xmin <- -4e+06-10^5
# xmax <- -4e+06
# ymin <- -2e+06
# ymax <- -2e+06+10^5

# testing how it works!
# Create a polygon from the square extent
# square_poly <- vect(ext(xmin, xmax, ymin, ymax),crs=crs(npv_2050_mkd))
# 
# # Mask the original raster using the square extent
# masked_raster <- mask(npv_2050_mkd, square_poly)
# masked_rasterc <- crop(masked_raster, square_poly)


# plot(masked_rasterc)
#min <- minmax(npv_2050_mkd)[1]

# plot(masked_rasterc)
cons_oc <- fill_nas(r = npv_2050_mkd)

# plot(masked_rasterc)

brmask <- veg
brmask[!is.na(brmask)] <- 1
cons_oc <- cons_oc*brmask

# plot(masked_rasterc)
# plot(cons_oc)

writeRaster(cons_oc,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/conservation_oc_reais_ha_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)




