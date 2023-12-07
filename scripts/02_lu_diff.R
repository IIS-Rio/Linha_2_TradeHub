# desconsiderar!!

# Land Use algebra

library(raster)

# read rasters ----

#r_base <- rast("")


# current

for_c = rast("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/forest_1km.tif")
wet_c = rast("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/wetland_1km.tif")
otn_c = rast("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/otn_1km.tif")
grass_c= rast("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/land_uses_1km/Baseline_2020/grassland_1km.tif")

nat_c = for_c + wet_c + otn_c + grass_c

# Baseline 2050

nat_veg_base <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/2050/base/downscaled_base_2050_natural_vegetation.tif")

# fcnz 2050

nat_veg_fcnz <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/2050/fcnz/downscaled_fcnz_2050_natural_vegetation.tif")
# fcnzplus 2050

nat_veg_fcnzplus <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/2050/fcplusnz/downscaled_fcplusnz_2050_natural_vegetation.tif")


# Difference between current and Baseline 2050 ----

nat_c <-project(nat_c,nat_veg_base)

nat_diff_baseline_c_2050 = nat_veg_base - nat_c


writeRaster(nat_diff_baseline_c_2050,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/nat_diff_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# conversion

nat_diff_baseline_c_2050[nat_diff_baseline_c_2050 > 0] = 0

conv_baseline = nat_diff_baseline_c_2050

writeRaster(conv_baseline,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/lu_diff/conversion_current_baseline_2050.tif",gdal=c("COMPRESS=DEFLATE"),overwrite=T)

# continuar substituindo!
# Difference between current and Soy Moratorium 2050 ----

nat_sm = for_sm + wet_sm + otn_sm + grass_sm

nat_sm_c = nat_sm - nat_c

writeRaster(nat_sm_c,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/x_scenarios/x_sm_2050.tif")

# conversion

nat_sm_c[nat_sm_c > 0] = 0

conv_sm = nat_sm_c

writeRaster(conv_sm,"/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/x_scenarios/x_sm_2050_conversion.tif")


# exploring the drivers of land use change -----

#silviculture
silv_c = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2020/baseline_2020_silviculture_cerrado_1km.tif")
silv_bau = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2050/baseline_2050_silviculture_cerrado_1km.tif")
silv_sm = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/soy_moratorium_2050/soy_moratorium_2050_silviculture_cerrado_1km.tif")

silv_bau_conv = silv_bau - silv_c
silv_bau_conv[silv_bau_conv < 0] = 0
silv_sm_conv = silv_sm - silv_c
silv_sm_conv[silv_sm_conv < 0] = 0

#agriculture
crop_c = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2020/baseline_2020_cropland_cerrado_1km.tif")
past_c = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2020/baseline_2020_pasture_cerrado_1km.tif")
agri_c = crop_c + past_c
agri_bau = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2050/baseline_2050_agriculture_cerrado_1km.tif")
agri_sm = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/soy_moratorium_2050/soy_moratorium_2050_agriculture_cerrado_1km.tif")

agri_bau_conv = agri_bau - agri_c
agri_bau_conv[agri_bau_conv < 0] = 0
agri_sm_conv = agri_sm - agri_c
agri_sm_conv[agri_sm_conv < 0] = 0

#ignored
ign_c = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2020/baseline_2020_ignored_cerrado_1km.tif")
ign_bau = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/baseline_2050/baseline_2050_ignored_cerrado_1km.tif")
ign_sm = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/soy_moratorium_2050/soy_moratorium_2050_ignored_cerrado_1km.tif")

ign_bau_conv = ign_bau - ign_c
ign_bau_conv[ign_bau_conv < 0] = 0
ign_sm_conv = ign_sm - ign_c
ign_sm_conv[ign_sm_conv < 0] = 0

#pixels containing agriculture and silviculture expansion
map_test_silv = agri_bau_conv
map_test_silv[map_test_silv > 0] = 0

map_test_silv[(agri_bau_conv > 0) & (silv_bau_conv > 0)] = 1

#pixels containing agriculture and urban expansion
map_test_urb = agri_bau_conv
map_test_urb[map_test_urb > 0] = 0

map_test_urb[(agri_bau_conv > 0) & (ign_bau_conv > 0)] = 1

# conversion due to agriculture only ----

# BAU

conv_bau_agr = conv_bau + silv_bau_conv + ign_bau_conv
conv_bau_agr[conv_bau_agr > 0] = 0

writeRaster(conv_bau_agr, "/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/x_scenarios/x_bau_2050_conversion_agr.tif")

# Soy Moratorium

conv_sm_agr = conv_sm + silv_sm_conv + ign_sm_conv
conv_sm_agr[conv_sm_agr > 0] = 0

writeRaster(conv_sm_agr, "/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/x_scenarios/x_sm_2050_conversion_agr.tif")

# checking difference between scenarios

conv_2050_diff = conv_bau_agr - conv_sm_agr


# zonal -----

base_ras_cerr = raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/base_ras_cerrado.tif")

# BAU
# Aggregate values based on the categorical raster
agg_val_bau = zonal(conv_bau_agr, base_ras_cerr, fun = mean)

# Create the table
agg_val_bau_tbl = data.frame(agg_val_bau)

names(agg_val_bau_tbl)[names(agg_val_bau_tbl) == "zone"] <- "muni_code"
names(agg_val_bau_tbl)[names(agg_val_bau_tbl) == "value"] <- "x_value"

write.csv(agg_val_bau_tbl, "/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/x_scenarios/x_bau_2050_conversion_agr_tbl.csv", row.names = F)

# Soy Moratorium
# Aggregate values based on the categorical raster
agg_val_sm = zonal(conv_sm_agr, base_ras_cerr, fun = mean)

# Create the table
agg_val_sm_tbl = data.frame(agg_val_sm)

names(agg_val_sm_tbl)[names(agg_val_sm_tbl) == "zone"] <- "muni_code"
names(agg_val_sm_tbl)[names(agg_val_sm_tbl) == "value"] <- "x_value"

write.csv(agg_val_sm_tbl, "/dados/projetos_andamento/TRADEhub/GLOBIOMbr/linha_pesquisa_3/land_uses_1km/x_scenarios/x_sm_2050_conversion_agr_tbl.csv", row.names = F)