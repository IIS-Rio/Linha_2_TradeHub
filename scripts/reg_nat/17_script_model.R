
nat_reg_predict <- function(df,var_cat){
  
  # var_cat tem que ser "biome" ou "fitofisionomias_Br"!!!
  
  # pacotes --------------------------------------------------------------------
  
  library(data.table) # abre dfs grandes
  library(dplyr)
  library(sampler) # amostragem estratificada
  library(randomForestSRC)
  library(tidyverse)
  library("pROC")
  #-----------------------------------------------------------------------------
  
  
  # define fracao amostragem do modelo (varia de regiao pra regiao)
  
    # dividindo dados 
  
  trainIndex <- sample(1:nrow(df), 0.7*nrow(df))
  trainData <- df[trainIndex, ]
  testData <- df[-trainIndex, ]
  
  # remover NAs
  
  trainData <- na.omit(trainData) # pra ajustar o modelo
  testData <- na.omit(testData) # pra avaliar o ajuste
  
  # escalar todas as variaveis preditoras
  
  train_sc <- trainData
  test_sc <- testData
  
  # transformando variaveis em fator
  
  train_sc$biome <- as.factor(train_sc$biome)
  test_sc$biome <- as.factor(test_sc$biome)
  
  train_sc$fitofisionomias_Br <- as.factor(train_sc$fitofisionomias_Br)
  test_sc$fitofisionomias_Br <- as.factor(test_sc$fitofisionomias_Br)
  # aplicando scale pras variaveis continuas 
  
  
  continuous_variables <- which(sapply(train_sc, is.numeric) & !names(train_sc) == "reg_0_1")
  
  # escalando variaveis continuas
  
  train_sc_continuous <- as.data.frame(apply(subset(train_sc, select = names(continuous_variables)),2,scale))
  
  test_sc_continuous <- as.data.frame(apply(subset(test_sc, select = names(continuous_variables)),2,scale))
  
  
  # Create a new data frame with the scaled continuous variables and the non-continuous variables
  
  train_sc <- cbind(train_sc[, -continuous_variables], train_sc_continuous)
  test_sc <- cbind(test_sc[, -continuous_variables], test_sc_continuous)
  
  if(var_cat=="bioma"){
  
  excluir <- c("x","y","agua","area_urbana","fitofisionomias_Br",)
  
  }
  
  if(var_cat=="fito"){
    
    excluir <- c("x","y","agua","area_urbana","biome","reg_0_1")
    
    
  }
  
  #excluindo variaveis 

  pred_posicao <- unlist(which(!names(train_sc) %in% excluir))
  pred_varnames <- names(train_sc[,pred_posicao])
  
  
  formula_full <- as.formula(paste("reg_0_1 ~", paste(pred_varnames, collapse = "+")))
  
  # Convert all character columns to factors
  
  train_sc <- train_sc %>% mutate_if(is.character, as.factor)
  
  # pra calcular erro por arvore precisa da opcao block.size=1! 
  
  rfModel_full <- rfsrc(formula = formula_full , data = as.data.frame(train_sc), ntree = 400,nodesize = 20,block.size = 1)
  
  actual <- test_sc$reg_0_1
  predicted <- predict(object = rfModel_full, newdata = test_sc)
  r_full <- caret::R2(pred = predicted$predicted,obs = actual) 
  r_rmse <- caret::RMSE(pred = predicted$predicted,obs = actual)
  # Create a ROC curve
  roc_curve <- roc(actual, predicted$predicted)
  #cat("AUC:", auc(roc_curve), "\n")
   roc_curve2 = list(
     auc = auc(roc_curve),  # Include AUC separately
     curve = roc_curve       # Include the entire ROC curve object if needed
   )
   
  results <- list(model=rfModel_full,r_squared=r_full,rmse=r_rmse,roc_curve)
  
  return(results)
  
}
