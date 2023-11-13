
# probabilidade reg nat

RegNat <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/reg_nat__2000_2022_sp.shp")


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

vegdist <-  rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/veg_distance_Br_faltaMAPANT_km.tif")

covar[[8]] <- vegdist

# unlist covar (variaveis climaticas entram na seq)

covar_unlst <- unlist(covar)

var_names <- c("veg_dens",paste0("PC",seq(1,5,1)),"urb_dist","soc","pH","crop_dens","biome","veg_dist")

names(covar_unlst) <- var_names


# montando df

# matriz vazia

f <- function(x) terra::extract(x, RegNat,ID=F)

extracted_values <- lapply(covar_unlst,f)

m <- do.call(cbind, extracted_values)

names(m) <- var_names

df_final <- cbind(RegNat,m)


df_final_dist0 <- filter(df_final,veg_dist==0)

summary(as.factor(df_final_dist0$r___200))

# coberturas pra excluir:

# agua e outros usos pra excluir
# area urbana
# vegetacao nativa no ano de 2000 (inicio da contagem!). melhor nao excluir, pq pega um monte de ponto de regeneracao tb!


excluir <- list()

agua <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/agua_outros_usos_excluir_Brasil_2020.tif")

excluir[[1]] <- agua

area_urbana <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/area_urbana_Brasil_2020.tif")

excluir[[2]] <- area_urbana


areas_excluir <- lapply(excluir,f)

excluir_df <-do.call(cbind, areas_excluir)

names(excluir_df) <- c("agua","area_urbana")

df_final <- cbind(df_final,excluir_df)

# filtrar so agua e area urbana==0

df_final_filter <- df_final%>%
  filter(agua==0,area_urbana==0)

# por enquanto salvar assim, mas falta varios pontos de dist veg!

# jogar fora geometria

st_geometry(df_final_filter) <- NULL

df_final_filter_noNA <- df_final_filter %>%
  filter(complete.cases(.))


names(df_final_filter_noNA)[1] <- "reg_0_1"


write.csv(df_final_filter_noNA,"/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_run_missingdistveg.csv",row.names = F)


