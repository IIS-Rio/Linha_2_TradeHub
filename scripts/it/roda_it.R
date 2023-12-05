# Roda o IT. Fazer iterativo por regiao!!falta fazer isso!mas acho q nao precisa, ja q o resultado eh um raster de 1km!

library(terra)

HFI_2050 <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/it/hfi_br_2050_ssp2.tif")


source("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/it/intactness.R")
it = intactness(hfi = HFI_2050)


writeRaster(x = it, "/dados/projetos_andamento/TRADEhub/Linha_2/it/it_br_2050_ssp2.tif")

