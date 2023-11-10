# raster binario regeneracao natural - usar mapbiomas (histórico??)

# gerei historico regeneracao natural no GEE:

# https://code.earthengine.google.com/578c15b9730324eabb29190a6b2a45fa

reg_nat <- brick("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/regeneracao_natural_2000_2022_col8.tif")

# simando valores

rs1 <- calc(reg_nat, sum)

# celulas com repetidas regeneracoes tem valor>1

rs1 <- rs1/rs1

Br <- read_country()%>%
  st_transform(crs(rs1))


# salvando raster

writeRaster(rs1,"/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/regeneracao_natural_soma_2000_2022_col8.tif")

# transformando em pontos. Aqui vale seguir o grid de 1km!? ou fazer resol menor e depois reamostrar? acho melhor fazer 100m e reamostrar qndo tiver todas as variáveis!

rs1 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/regeneracao_natural_soma_2000_2022_col8.tif")

br <- read_country()%>%
  st_transform(crs(rs1))

# transformar em mollenweide
# criar grid de 100m com pontos
# usar pra extrair valores e preparar a base pra ajuste do modelo!
# extrair tb os land-uses pra excluir!! como colunas!
plot(st_geometry(br),col="black")
plot(rs1,add=T)
