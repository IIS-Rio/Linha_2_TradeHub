
library(data.table)
library(terra)

source("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/reg_nat/17_script_model.R")

df <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_completo.csv")

m1 <- nat_reg_predict(df)

# plot(m1[[4]])
# cat("AUC:", auc(m1[[4]]), "\n")
# 
# plot(m1[[4]], main = "ROC Curve", col = "blue", lwd = 2)
#library("pROC")


# ficou show AUC: 0.8825602 

# extrapolar pro grid brasil. (falta calcular pra cada celula as metricas)

# grid completo Br

Br_grid <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_Br_to_extrapolate.csv")

#Br_grid <- Br_grid[,-c(1,2,3,18,19)]


Br_grid$biome <- as.factor(Br_grid$biome)

# so da pra rodar com dados sem NA.

Br_grid2 <- Br_grid[complete.cases(Br_grid),] 

# transformando em df

Br_grid2 <- as.data.frame(Br_grid2)

# aplicando scale pras variaveis continuas 

continuous_variables <- which(sapply(Br_grid2, is.numeric))

# escalando variaveis continuas

Br_grid_sc_continuous <- as.data.frame(scale(Br_grid2[, continuous_variables]))

Br_grid_sc <- Br_grid2[, -continuous_variables, drop = FALSE]

Br_grid_sc <- cbind(Br_grid_sc, Br_grid_sc_continuous)

# predizendo valores (tem os NAs!esqueci de remover!)

predicted <- predict(object = m1$model, newdata = Br_grid_sc)

# limpando NAs

# no df original

#Br_gridNAclean <- Br_grid[complete.cases(Br_grid),]

Br_grid2$predicted <- predicted$predicted


# rbase

rbase <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/biomes_raster.tif")

# transformar em vetor

Br_gridNAclean_vec <- vect(Br_grid2,geom=c("x", "y"))

Br_gridNAclean_vec$predicted <- as.numeric(Br_gridNAclean_vec$predicted)
# head(as.numeric(as.data.frame(Br_gridNAclean_vec)$predicted))
# summary(as.data.frame(Br_gridNAclean_vec)$predicted)
# str(as.data.frame(Br_gridNAclean_vec))
# head(Br_gridNAclean_vec)
prob_reg <- rastericppprob_reg <- rasterize(Br_gridNAclean_vec,rbase,field="predicted")
#plot(prob_reg)

writeRaster(prob_reg,"/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/model_output/prob_reg_Br_1kmv02.tif",gdal=c("COMPRESS=DEFLATE"))

# prob_reg <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/model_output/prob_reg_Br_1km.tif")
# 
# plot(prob_reg)

