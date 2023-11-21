# custos instituto escolhas

library(readxl)

custos_restauracao_Escolhas_2023 <- read_excel("/dados/projetos_andamento/TRADEhub/Linha_2/custos_restauracao/custos_restauracao_Escolhas_2023.xlsx")

# # Function to replace dots with commas
# replace_dots_with_commas <- function(x) {
#   (gsub("\\.", ",", x))
# }
# 
# # Apply the function to all columns in the dataframe
# custos_restauracao_Escolhas_2023 <- as.data.frame(lapply(custos_restauracao_Escolhas_2023, replace_dots_with_commas))

# assumindo um gradiente entre a restauracao mais custosa (plantio de mudas) e a restauração menos custosa (condução de regeneracao natural), com mão-de-obra contratada. Assumindo tb espaçamento 3x2 (1600 mudas/ha)

# equacao:
# r = (1-p)*c+p*a
# p= probabilidade regen. nat; c = total planting cost; a =  assisted regeneration cost

# abrindo raster prob regeneracao natural:


prob_reg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/model_output/prob_reg_Br_1kmv03_fito.tif") 

# arredondando pra 2 casas

prob_reg_round <- round(prob_reg,2)

a = custos_restauracao_Escolhas_2023%>%
  filter(espacamento=="3x2",
         mao_obra=="contratada")%>%
  select(conducao_regeneracao_natural)%>%
  mutate(conducao_regeneracao_natural=as.numeric(conducao_regeneracao_natural))

c = custos_restauracao_Escolhas_2023%>%
  filter(espacamento=="3x2",
         mao_obra=="contratada")%>%
  select(plantio_mudas_n_mecanizado)





restoration_cost <- (1- prob_reg_round)*as.numeric(c$plantio_mudas_n_mecanizado) +prob_reg_round*a$conducao_regeneracao_natural


writeRaster(restoration_cost,"/dados/projetos_andamento/TRADEhub/Linha_2/custos_restauracao/custos_restauracao_2023_reais_ha.tiff",gdal=c("COMPRESS=DEFLATE"))


