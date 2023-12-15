# Bibliotecas
library(terra)

# HFI Original
hfi = rast("/dados/bd_iis/hfi/hfi_world_2009/wildareas-v3-2009-human-footprint.tif")

# Raster base
base_ras = 0 * rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/delta_agbgC_restor_tonha_v6.1_BR.tif")

# HFI para o Brasil
hfi_br = mask(x = project(x = hfi, base_ras, method = "near"), mask = base_ras)

# Removendo valores patológicos
hfi_br[hfi_br > 50] = NA

# Densidade populacional 2010 e 2050
pop_dens_2010 = rast("/dados/projetos_andamento/TRADEhub/SSP2_1km/ssp2_total_2010.tif")
# criar pop dens_2020 tb e atualizar tb o de 2010!
pop_dens_2020 = rast("/dados/projetos_andamento/TRADEhub/Linha_2/pop_density/gpw_v4_population_density_rev11_2020_30_sec.tif")
#pop density 2050
pop_dens_2050 = rast("/dados/projetos_andamento/TRADEhub/SSP2_1km/ssp2_total_2050.tif")

# Reamostrando
pop_dens_2010_res = mask(x = project(x = pop_dens_2010, hfi_br), mask = hfi_br)
pop_dens_2020_res = mask(x = project(x = pop_dens_2020, hfi_br), mask = hfi_br)
pop_dens_2050_res = mask(x = project(x = pop_dens_2050, hfi_br), mask = hfi_br)

# creating difference map of urban layers

# 2010 -------------------------------------------------------------------------

pop_dens_dif = pop_dens_2050_res - pop_dens_2010_res 

#se negativo houve redução da densidade populacional e do HFI, se positivo houve aumento da densidade populacional e do HFI


# Updating Human Footprint Index if necessary
# creating the layer that will modify HFI
HFImodification = pop_dens_dif - pop_dens_dif
HFImodification[pop_dens_2010_res >= 10 & pop_dens_2050_res >= 10] = 0
HFImodification[pop_dens_2010_res >= 10 & pop_dens_2050_res < 10] = pop_dens_2050_res[pop_dens_2010_res >= 10 & pop_dens_2050_res < 10] - 10
HFImodification[pop_dens_2010_res < 10 & pop_dens_2050_res >= 10] = 10 - pop_dens_2010_res[pop_dens_2010_res < 10 & pop_dens_2050_res >= 10]
HFImodification[pop_dens_2010_res < 10 & pop_dens_2050_res < 10] = pop_dens_2050_res[pop_dens_2010_res < 10 & pop_dens_2050_res < 10] - pop_dens_2010_res[pop_dens_2010_res < 10 & pop_dens_2050_res < 10]

#10 is the weight of population density chosen based on the Sanderson et al 2002 article
HFI_2050 = hfi_br + HFImodification


writeRaster(x = HFI_2050, filename = "/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2050_ssp2.tif",overwrite=T,gdal=c("COMPRESS=DEFLATE"))

# writeRaster(x = HFI_2009, filename = "/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2009_ssp2.tif",overwrite=T,gdal=c("COMPRESS=DEFLATE"))


# 2020 -------------------------------------------------------------------------

pop_dens_dif = pop_dens_2020_res - pop_dens_2010_res 

#se negativo houve redução da densidade populacional e do HFI, se positivo houve aumento da densidade populacional e do HFI


# Updating Human Footprint Index if necessary
# creating the layer that will modify HFI
HFImodification = pop_dens_dif - pop_dens_dif
HFImodification[pop_dens_2010_res >= 10 & pop_dens_2020_res >= 10] = 0
HFImodification[pop_dens_2010_res >= 10 & pop_dens_2020_res < 10] = pop_dens_2020_res[pop_dens_2010_res >= 10 & pop_dens_2020_res < 10] - 10
HFImodification[pop_dens_2010_res < 10 & pop_dens_2020_res >= 10] = 10 - pop_dens_2010_res[pop_dens_2010_res < 10 & pop_dens_2020_res >= 10]
HFImodification[pop_dens_2010_res < 10 & pop_dens_2020_res < 10] = pop_dens_2020_res[pop_dens_2010_res < 10 & pop_dens_2020_res < 10] - pop_dens_2010_res[pop_dens_2010_res < 10 & pop_dens_2020_res < 10]

#10 is the weight of population density chosen based on the Sanderson et al 2002 article
HFI_2020 = hfi_br + HFImodification

writeRaster(x = HFI_2020, filename = "/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2020.tif",overwrite=T,gdal=c("COMPRESS=DEFLATE"))
