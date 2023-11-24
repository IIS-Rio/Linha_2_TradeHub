library(terra)

p <- "/dados/projetos_andamento/CB-BR/results/Results_Andre_Junqueira_Nov2023/results"

rster <- list.files(p,"tif",full.names = T)

Br_delta <- rast(grep("delta_agbgC_restor_tonha_v6.1_BR",x = rster,value = T))
Br_delta1km <- terra::aggregate(x =Br_delta,fact=2 )


plot(Br_delta1km)
plot(Br_delta1km<0)

# cruzar com lus e tabular

lus_1km_lst <- grep(x = list.files("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/rawdata/land-use",recursive = F,full.names = T),pattern = "discons",invert = T,value = T)

nms_lus <- grep(x = list.files("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/rawdata/land-use",recursive = F,full.names = F),pattern = "discons",invert = T,value = T)

# abrindo rasters adequando pj

f <- function(x){
  
  r <- rast(x)
  xpj <- project(r,Br_delta1km)

}

lus_1km <- lapply(lus_1km_lst,f)

# transformar % em area

f2 <- function(x)x*area

area <- res(Br_delta)[1]*res(Br_delta)[2]/10^4

lus_1km_area <- lapply(lus_1km,f2)

# multiplicar por cada lu e somar area total de negativos em cada classe em ha e relativo ao total de negativos!

Br_delta1km_negvalues <- Br_delta1km
Br_delta1km_negvalues[Br_delta1km_negvalues>=0] <- NA
Br_delta1km_negvalues[Br_delta1km_negvalues<0] <- 1


f3 <- function(lu,nm){
  
  x <- lu*Br_delta1km_negvalues
  area_lu <-  sum(x[], na.rm = TRUE)
  df <- data.frame(lu=nm,area=area_lu)
  return(df)
  
}


lus_1km_area_compt <- lapply(seq_along(lus_1km), function(i) f3(lus_1km[[i]], nms_lus[i]))

result_df <- do.call(rbind, lus_1km_area_compt)

# calcular area relativa:

area_pixels_neg <- (sum(Br_delta1km_negvalues[],na.rm = T)*area)

result_df$relative <- round(result_df$area/area_pixels_neg,3)

result_df$relative <- round(result_df$area/sum(result_df$area),3)

print(result_df)

Br_delta1km_positivos <- Br_delta1km
Br_delta1km_positivos[Br_delta1km_positivos>=0] <- 1
Br_delta1km_positivos[Br_delta1km_positivos<0] <- NA

plot(Br_delta1km_positivos)

pixels_pos <- sum(Br_delta1km_positivos[],na.rm = T)
pixels_neg <- sum(Br_delta1km_negvalues[],na.rm = T)

pixels_neg/(pixels_pos+pixels_pos)
