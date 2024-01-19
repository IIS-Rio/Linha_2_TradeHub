
# calcular onde houve restauracao e conservacao, para balanco de carbono e custo de oportunidade

library(terra)

# read rasters -----------------------------------------------------------------

# current

natveg_c <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/nat_veg.tif")


# Baseline 2050

natveg_base2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/base/nat_veg.tif")

# fcnz 2050

nat_veg_fcnz2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/fcnz/nat_veg.tif")
# fcnzplus 2050

nat_veg_fcnzplus2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/fcplusnz/nat_veg.tif")

# Difference between current and Baseline 2050 ---------------------------------

# > 0 = current tem menos vegetacao: teve restauracao
# < 0 = current tem mais vegetacao: teve conversao

natveg_diff_baseline_c_2050 = natveg_base2050 - natveg_c

writeRaster(natveg_diff_baseline_c_2050,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/natveg_diff_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# conversion

conv_baseline <- natveg_diff_baseline_c_2050

conv_baseline[conv_baseline > 0] = 0

writeRaster(conv_baseline,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

restored_baseline <- natveg_diff_baseline_c_2050
restored_baseline[restored_baseline < 0] = 0

writeRaster(restored_baseline,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# Difference between current and fcnz 2050 ----------------------------------

natveg_diff_fcnz_c_2050 = nat_veg_fcnz2050 - natveg_c


conv_fcnz <- natveg_diff_fcnz_c_2050

conv_fcnz[conv_fcnz > 0] = 0

writeRaster(conv_fcnz,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_fcnz_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

restored_fcnz <- natveg_diff_fcnz_c_2050
restored_fcnz[restored_fcnz < 0] = 0

writeRaster(restored_fcnz,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_fcnz_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)



# Difference between current and fcnzplus 2050 ---------------------------------

natveg_diff_fcnzplus_c_2050 = nat_veg_fcnzplus2050 - natveg_c

conv_fcnzplus <- natveg_diff_fcnzplus_c_2050

conv_fcnzplus[conv_fcnzplus > 0] = 0

plot(conv_fcnzplus) # da praticamente 0, mas nao totalmente
plot(conv_fcnzplus!=0) #areas marginais

# nao da zero. pq?

# ver se original da diferente

fcnzplus_o <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS/fcplusnz/multiband-2050-low.tif")
crs(fcnzplus_o)<- "+proj=aea +lat_0=-12 +lon_0=-54 +lat_1=-2 +lat_2=-22 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

fcnzplus_o_veg <- fcnzplus_o[[1]]

baseline_o <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/ResultsISS/base/multiband-2020-low.tif")

baseline_o_veg <- baseline_o[[1]]


diff <- fcnzplus_o_veg-baseline_o_veg

conv_o <- diff

conv_o[conv_o>0] <- 0

plot(conv_o)

writeRaster(conv_fcnzplus,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_fcnzplus_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

restored_fcnzplus <- natveg_diff_fcnzplus_c_2050
restored_fcnzplus[restored_fcnzplus < 0] = 0

writeRaster(restored_fcnzplus,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_fcnzplus_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# natural areas that remained conserved ----------------------------------------


conserved_c_baseline <-  natveg_diff_baseline_c_2050

# so oq da zero

conserved_c_baseline[conserved_c_baseline!=0] <- NA

writeRaster(conserved_c_baseline,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/natvegconserved_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

conserved_fcnz <-  natveg_diff_fcnz_c_2050

# so oq da zero

conserved_fcnz[conserved_fcnz!=0] <- NA

writeRaster(conserved_fcnz,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/natvegconserved_current_fcnz_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)



conserved_fcnzplus <-  natveg_diff_fcnzplus_c_2050

# so oq da zero

conserved_fcnzplus[conserved_fcnzplus!=0] <- NA

writeRaster(conserved_fcnzplus,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/natvegconserved_current_fcnzplus_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

