
# calcular onde houve restauracao e conservacao, para balanco de carbono e custo de oportunidade

library(terra)

# read rasters ----

# current

natveg_c <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020/natveg.tif")


# Baseline 2050

natveg_base2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/base/natveg.tif")

# fcnz 2050

nat_veg_fcnz2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/fcnz/natveg.tif")
# fcnzplus 2050

nat_veg_fcnzplus2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050/fcplusnz/natveg.tif")


# Difference between current and Baseline 2050 ----

natveg_diff_baseline_c_2050 = natveg_base2050 - natveg_c


writeRaster(natveg_diff_baseline_c_2050,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/natveg_diff_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# conversion

conv_baseline <- natveg_diff_baseline_c_2050

conv_baseline[conv_baseline > 0] = 0

writeRaster(conv_baseline,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

restored_baseline <- natveg_diff_baseline_c_2050
restored_baseline[restored_baseline < 0] = 0

writeRaster(restored_baseline,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# continuar substituindo!

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

writeRaster(conv_fcnzplus,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_fcnzplus_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

restored_fcnzplus <- natveg_diff_fcnzplus_c_2050
restored_fcnzplus[restored_fcnzplus < 0] = 0

writeRaster(restored_fcnzplus,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/restored_current_fcnzplus_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)









