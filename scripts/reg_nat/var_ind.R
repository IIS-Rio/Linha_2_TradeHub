# raster binario regeneracao natural - usar mapbiomas (histórico??)
# No paper "Global potential"  usaram variavel calculada pelo global forest watch:
# Pantropical tree plantation expansion (2000-2012)

# lado bom eh q ja ta pronto

url <- "https://www.arcgis.com/sharing/rest/content/items/8196f3a8707840188f357d50cfad32d8/data"

destfile = "/dados/projetos_andamento/TRADEhub/Linha_2/pantropical_tree_plant_exp/pantropical_tree_plant_exp.gpkg"

library(curl)

curl_download(url, destfile, quiet = FALSE)

plot(raster("/dados/projetos_andamento/TRADEhub/Linha_2/soil_data/soc/soc_igh_15_30.tif"))

# tentativa 2

