# pacotes
library(terra)
library(sf)
library(purrr)
#------------------------------------------------------------------------------

# ajustar pontos proporcionais a area do bioma! (os controle!)


# montando pontos com areas que tiveram restauracao

regnat_raster <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/regeneracao_natural_soma_2000_2022_col8.tif")


# pegar so oq eh 1. depois sortear uma nuvem com 0



regnat_points <- as.points(regnat_raster)

points_sf <- st_as_sf(regnat_points)


points_sf $x <- st_coordinates(points_sf )[, "X"]
points_sf $y <- st_coordinates(points_sf )[, "Y"]

# falta agora definir uma quantidade de pontos que nao regenerou!!

# gerar uma nuvem no brasil, distribuida por bioma, e dai extrair do raster. Oq for NA mudar pra 0


biomas <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp")


set.seed(123)  # Set seed for reproducibility

biomas$area <- st_area(biomas)/10^4
biomas$proportional_points <- as.integer(num_points_total * (biomas$area / sum(biomas$area)))
# gerar 4xmais pontos
num_points_total <- nrow(points_sf)*4

# Generate random points proportional to the area
random_points <- biomas %>%
  group_by(code_biome,name_biome,proportional_points) %>%
  nest() %>%
  mutate(points = map(data, ~ st_sample(.x, size = proportional_points, geo = TRUE, na.rm = TRUE))) %>%
  select(-data) %>%
  unnest(points)

random_points <- st_as_sf(random_points)

random_points$x <- st_coordinates(random_points)[, "X"]
random_points$y <- st_coordinates(random_points)[, "Y"]

# amostrar raster de reg. tudo q for NA eh o 0

# Extract raster values at the random points
extracted_values <- terra::extract(regnat_raster, random_points,xy=T)

extracted_values_norest <- extracted_values[is.na(extracted_values$regeneracao_natural_soma_2000_2022_col8),]

# converter valores pra o

extracted_values_norest$regeneracao_natural_soma_2000_2022_col8 <- 0

extracted_values_norest <- st_as_sf(extracted_values_norest,coords = c("x","y"))

st_crs(extracted_values_norest) <- st_crs(regnat_raster)

extracted_values_norest <- st_transform(extracted_values_norest,crs = st_crs(points_sf))

extracted_values_norest$x <- st_coordinates(extracted_values_norest)[, "X"]
extracted_values_norest$y <- st_coordinates(extracted_values_norest)[, "Y"]

# combinando os dados

final_data <- rbind(points_sf,extracted_values_norest[,c(2,4,5)])


# salvando

st_write(final_data,"/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/reg_nat__2000_2022_sp.shp",append=F)
