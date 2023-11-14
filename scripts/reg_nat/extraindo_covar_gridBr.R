# probabilidade reg nat

Br_grid <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/input_data/Br_grid_points.shp")

covar <- list()

# densidade vegegataco natural (1)
vegdensity <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_density_mollweide_1km.tif")

covar[[1]] <- vegdensity

# variaveis bioclim (2-6)

clim_var <- lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/climate/PCA",full.names = T),rast)

covar[[2]] <- clim_var


# distancia cidades (7)

urbdist <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/urb_distance_Br_km.tif")

covar[[3]] <- urbdist

# soc (8)
soc <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/mosaico/mean_soc_5_30cm.tif")


covar[[4]] <- soc

# ph (9)

ph <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/phh2o//mosaico/mean_ph_5_30cm.tif")


covar[[5]] <- ph

# crop density (10)

cropdensity <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/crop_density_mollweide_1km.tif")


covar[[6]] <- cropdensity

# bioma (11)

biome <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes_raster.tif")

covar[[7]] <- biome


# distancia veg nat (12)

vegdist <-  rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_distance_Br_km.tif")

covar[[8]] <- vegdist

# unlist covar (variaveis climaticas entram na seq)

covar_unlst <- unlist(covar)

var_names <- c("veg_dens",paste0("PC",seq(1,5,1)),"urb_dist","soc","pH","crop_dens","biome","veg_dist")

names(covar_unlst) <- var_names


# montando df

# matriz vazia

f <- function(x) terra::extract(x, Br_grid,ID=F)

extracted_values <- lapply(covar_unlst,f)

m <- do.call(cbind, extracted_values)

head(m)

m2 <- m

names(m2) <- var_names

df_final <- cbind(Br_grid,m2)


st_geometry(df_final) <- "NULL"

write.csv(df_final,"/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_Br_to_extrapolate.csv",row.names = F)

