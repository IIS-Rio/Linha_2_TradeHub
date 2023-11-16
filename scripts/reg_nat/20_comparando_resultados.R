library(terra)

meu_modelo <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/model_output/prob_reg_Br_1km.tif") 

modelo_publicado <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico/ressampled_100m/nat_reg_mos.tiff")

modelo_publicado_pj <- terra::project(modelo_publicado,meu_modelo)

# Step 2: Convert raster extent to sf object
extent_vector <- as.polygons(ext(modelo_publicado_pj),crs=crs(meu_modelo))

plot(modelo_publicado_pj)
plot(extent_vector,add=T)


# Step 3: Generate random points within the sf object
num_points <- 100000  # Adjust the number of points as needed
random_points <- st_sample(st_as_sf(extent_vector), num_points)
random_points <- vect(random_points)
# Step 4: Extract raster values at the random points
values_at_points1 <- extract(meu_modelo, random_points)
values_at_points2 <- extract(modelo_publicado, random_points)

values_at_points <- cbind(values_at_points1,values_at_points2[,2])
values_at_points <- values_at_points[complete.cases(values_at_points),]

names(values_at_points) <- c("ID","meu modelo","modelo publicado")


library(ggpubr)


summary(values_at_points)

# fooodeeeeeo

ggscatter(values_at_points,x="meu modelo",y="modelo publicado")

cor(values_at_points$`meu modelo`,values_at_points$`modelo publicado`)


par(mfrow = c(1, 2))

reg_nat <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/model_output/prob_reg_Br_1kmv02.tif")


reg_nat2 <-rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/paper_renatinho/mosaico/ressampled_100m/nat_reg_mos.tiff") 

plot(reg_nat)

plot(reg_nat2)
