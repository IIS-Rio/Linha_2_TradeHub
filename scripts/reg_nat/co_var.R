# covariaveis regeneracao natural baseados em:

# The global potential for natural regeneration in deforested tropical regions [WWW Document], 2023. https://doi.org/10.21203/rs.3.rs-3235955/v1

# variaveis + importantes sao biofísicas (top 10:
# 1. local forest density
# 2. dist. nearest forest
# 3. soil organic carbon density and soil pH
# 4. 4 biolcim variables (4 eixos pca com 19 bioclim do worldclim)
# 5. land use/cover
# 6. biome

# forest density pode ser um moving window focado em cada celula do grid num buffer de 1km. No paper usaram forest density in 1km2. oq ja eh a celula do grid. ja tenho!
# calcular dist. veg nat mais proxima! facil!
# organic carbon density e PH tenho q baixar: SoilGrids250m: Global gridded soil information based on machine learning

# acesso: https://www.soilgrids.org/ mas limita a 2 graus. teria q fazer varias vezes

# We calculated the mean of each of the 12 properties within the top 30cm of soil (weighted by the depth of each layer)

# da trabalho baixar um por um

# https://rstudio-pubs-static.s3.amazonaws.com/1047230_6e3cb9b5389a4a3dac7d3cefb283cd5a.html

# 
 