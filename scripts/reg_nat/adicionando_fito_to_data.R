library(data.table)

# adicionar fito pra treinar o modelo e pro grid Br

# df pra treinar o modelo

proj <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/mapbiomas/reg_nat__2000_2022_sp.shp")

df_train <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_completo.csv")

# transformar em vetor

df_train_vect <- vect(df_train,geom=c("x","y"))
crs(df_train_vect) <- "+proj=longlat +datum=WGS84"

df_train_vect <- terra::project(df_train_vect,"+proj=longlat +datum=WGS84")

#fito

fito <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/other_covariables/fitofisionomias.tiff")


# ajustar projecao (ta uma bagunca)

fito_proj <- project(fito,df_train_vect,method="mode")

fito_proj <- as.int(fito_proj)

unique(fito_proj)

plot(fito_proj==0)

# nem todas as fitofisionomias sao amostradas. alem disso, o extract gera floating numbers, nao sei pq! precisa agrupar as fitofisionomias de alguma forma!

df_train_vect2 <- terra::extract(x = fito_proj,df_train_vect,method="simple",bind=TRUE,exact=TRUE,xy=T)

head(df_train_vect2)

df_train_vect3 <- as.data.frame(df_train_vect2)
df_train_vect3$fitofisionomias_Br <- as.factor(df_train_vect3$fitofisionomias_Br)

write.csv(df_train_vect3,"/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_completo_fito.csv")


# falta adicionar pro br todo

Br_grid <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/input_data/Br_grid_points2.shp")

fito_proj <- project(fito,crs(Br_grid),method="mode")

br_fito <- terra::extract(fito_proj, Br_grid,ID=F,bind=TRUE,exact=TRUE)

# Drop the geometry column
df_final <- st_drop_geometry(br_fito)

write.csv(df_final,"/dados/projetos_andamento/TRADEhub/Linha_2/input_data/df_Br_to_extrapolate_fito.csv",row.names = F)

