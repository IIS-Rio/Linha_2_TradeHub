library(terra)

# acho q pode ser interessante usar ecorregioes nao so pra calcular vulnerabilidade, mas tb pra calcular o it e o bd.

ecorregioes <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/ec/ecoregions_comCerrado.tif")


writeRaster(ecorregioes,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/ecoregions_wwf_plusCerrado.tif",gdal=("COMPRESS=DEFLATE"))

impl_cost <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/custos_restauracao/custos_restauracao_2023_reais_ha.tiff")

writeRaster(impl_cost,"/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/variables/restoration_implementation_cost_reais_ha.tif",gdal=("COMPRESS=DEFLATE"))

# falta carbon (potencial e estoque)
# falta hfi
# falta custo de oportunidade (Cadu sugeriu 3-6% taxa) - precisa de uma camada pra restauracao e outra pra conservacao??ver melhor isso!