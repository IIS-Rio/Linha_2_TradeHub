
source("../Linha_2_TradeHub/scripts/reg_nat/script_model.R")

df <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_run_missingdistveg.csv")

m1 <- nat_reg_predict(df)

# parece q ficou bem merda. falta tb calcular auc e curva roc!
plot(m1$model)

library("pROC")


# Make predictions on the test set
predictions <- predict(m1$model, newdata = df, block.size = 1000)

# Extract the predicted probabilities for the positive class
predicted_probs <-predictions$predicted

# Create a ROC curve

roc_curve <- roc(df$reg_0_1, predicted_probs)
cat("AUC:", auc(roc_curve), "\n")
plot(roc_curve, main = "ROC Curve", col = "blue", lwd = 2)
