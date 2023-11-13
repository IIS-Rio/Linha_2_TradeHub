# covariaveis regeneracao natural baseados em:

# The global potential for natural regeneration in deforested tropical regions [WWW Document], 2023. https://doi.org/10.21203/rs.3.rs-3235955/v1

# The ten top-ranked covariates in the biophysical model included local forest density; distance to nearest forest; two soil variables (soil organic carbon density and soil pH); four bioclimatic variables (the first four axes of a principal components analysis based on all 19 bioclimatic variables); land use/cover; and biome 


# variaveis + importantes sao biofísicas (top 10:
# 1. local forest density # tem q fazer pros pontos q tem regeneracao
# 2. dist. nearest forest
# 3. soil organic carbon density and soil pH -OK
# 4. 4 biolcim variables (5 eixos pca com 19 bioclim do worldclim) - ok
# 5. land use/cover - parece ser: cropland density and distance to urban area - falta calcular pra urban!
# 6. biome # tem q ver se tem amostra em todos os biomas:

# depois precisa excluir tb pontos em determinados usos:
# water, bare areas, urban areas, sparse vegetation; ESA
# CCI Landcover for year 2000)


# forest density pode ser um moving window focado em cada celula do grid num buffer de 1km. No paper usaram forest density in 1km2. oq ja eh a celula do grid. ja tenho!
# calcular dist. veg nat mais proxima! facil!
# organic carbon density e PH tenho q baixar: SoilGrids250m: Global gridded soil information based on machine learning

# acesso: https://www.soilgrids.org/ mas limita a 2 graus. teria q fazer varias vezes

# We calculated the mean of each of the 12 properties within the top 30cm of soil (weighted by the depth of each layer)

# da trabalho baixar um por um

# https://rstudio-pubs-static.s3.amazonaws.com/1047230_6e3cb9b5389a4a3dac7d3cefb283cd5a.html

area_urbana <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/area_urbana_Brasil_2020.tif")
 
plot(area_urbana)

dist_veg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/Distancia_vegetacao_nativa_Brasil.tif")

br <- read_country()%>%
  st_transform(crs(dist_veg))

dist_veg_c <- crop(dist_veg,br)
dist_veg_m <- mask(dist_veg_m,br)

dist_veg_m2 <- dist_veg_m

dist_veg_m2[dist_veg_m2>1000] <- 1000

plot(dist_veg_m2)

plot(dist_veg_m>0&dist_veg_m<5000)
hist(dist_veg_m[])

# biomas br pra subir no gee

biomes <- read_biomes(year="2019")%>%
  filter(!is.na(code_biome))


st_write(biomes,"/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes.shp",append=FALSE)



