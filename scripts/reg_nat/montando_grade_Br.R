library(sf)
library(terra)
library(tidyverse)

# grade Br

r_base <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes_raster.tif")
r_base <- r_base/r_base


Br <- as.points(r_base)
points_sf <- st_as_sf(Br)
points_sf $x <- st_coordinates(points_sf )[, "X"]
points_sf $y <- st_coordinates(points_sf )[, "Y"]

# extraindo areas pra excluir (contando aqui areas de veg nativa)


excluir <- list()

f <- function(x) terra::extract(x, points_sf,ID=F)

agua <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/agua_outros_usos_excluir_Brasil_2020.tif")

agua_aggregated <- aggregate(agua, fact = 10, fun = mean)

# transformar em fracao, 1km. Melhor pra excluir!

excluir[[1]] <- agua_aggregated

area_urbana <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/area_urbana_Brasil_2020.tif")

area_urbana_aggregated <- aggregate(area_urbana, fact = 10, fun = mean)

excluir[[2]] <- area_urbana_aggregated

# acho q veg nao vale a pena com o de 100. mas sim excluir a partir dos usos com 100%. Pq se nao, qqer pixel q cai nos 100 o centroide, mesmo sendo um pixel bem maior, vai ficar como 0!

veg2020 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/NatVegCover_100m.tif")


veg2020_aggregated <- aggregate(veg2020, fact = 10, fun = mean)

#plot(veg2020_aggregated)

excluir[[3]] <- veg2020_aggregated


areas_excluir <- lapply(excluir,f)

excluir_df <-do.call(cbind, areas_excluir)

# filtrar so oq eh NA para esses campos!

names(excluir_df) <- c("agua","area_urbana","veg_2020") 

excluir_df_coord <- cbind(excluir_df,points_sf[,c(3,4)])

df_filter <- excluir_df_coord%>%
  filter(agua!=1,area_urbana!=1,veg_2020!=1)

# salvar grade

st_write(df_filter,"/dados/projetos_andamento/TRADEhub/Linha_2/input_data/Br_grid_points2.shp",append=F)

