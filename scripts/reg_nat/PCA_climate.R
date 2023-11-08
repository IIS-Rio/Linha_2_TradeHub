library(data.table)

# Because several of the bioclimatic variables are correlated, we used principal components analysis to reduce the set of 19 bioclimatic variables to a set of 5 principal components that capture 99.4% (Table 1) of the variation among the original 19 variables but that are orthogonal (uncorrelated) with each other. This was achieved by generating a random sample of 1 million points within land areas (the bioclimatic datasets have NoData values for marine areas, which were excluded) and extracting the cell values associated with those locations from each of the 19 bioclimatic variables into a matrix. Each raster contained over 309 million data values; hence it was necessary to base the principal components analysis on a sample of the values rather than all values. The ‘prcomp’ function 94 in R was used to calculate the principal components based on this matrix. This PCA model was then applied to all pixels in the rasters (using the ‘predict.prcomp’ function94 in R) and the first 5 components were written as new raster datasets that were used in subsequent modelling.

# baseraster downscaled

b <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_downscalled/multiband-2025-low.tif")

# definindo projecao:

pj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs "

# criar matriz a partir dos rasters. Loop pra abrir e adicionar

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/climate"
world_clim_var <- list.files(p,pattern=".tif",full.names = T)

# Initialize an empty list to store the data frames
dataframes_list2 <- list()

for (i in 1:length(world_clim_var)) {
  
  r <- raster(world_clim_var[[i]])
  
  # Read the raster into a data frame (use appropriate functions based on your raster format)
  # Here, we assume that the raster can be read as a data frame using raster::extract()
  # Replace this with the correct function for your raster type
  df <- as.data.frame(r,xy=T)
  
  # Append the data frame to the list
  dataframes_list2[[i]] <- df
}

f <- function(x)left_join(x)

#combined_dataframe <- do.call(rbind,dataframes_list)
combined_dataframe2 <- do.call(cbind,dataframes_list2)
# Select columns with names containing "_col"

selected_columns <- combined_dataframe2 %>% select(contains("BR_"))

# adicionar xy


selected_columns <- cbind(selected_columns,combined_dataframe2[,c(1,2)])

# salvando

write.csv(selected_columns,"/dados/projetos_andamento/TRADEhub/Linha_2/climate/matrix_worldclim_BR.csv",row.names = F)

# eliminar NAs. Ficou cheio de xy tb

matriz_world_clim <- selected_columns[complete.cases(selected_columns), ]

pca <- prcomp(matriz_world_clim[,1:19])

# Extract the scores (transformed data) for the first 5 principal components for the original data

scores_5pcs <- pca$x[, 1:5]

# adicionando x e y

scores_5pcs <- cbind(scores_5pcs,matriz_world_clim[,c(20,21)])

# transformando em ponto

scores_5pcs_sp <- st_as_sf(x = scores_5pcs,coords = c("x", "y"))

head(scores_5pcs_sp)

example_raster <- raster(crs = crs(r), resolution = c(0.008333333, 0.008333333), ext = extent(r),vals=0)

component=c(paste0("PC",seq(1,5,1)))

for(j in 1:length( component)){
  
  teste <-raster::rasterize(scores_5pcs_sp,field=component[[j]],y = example_raster)
  
  writeRaster(teste,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/climate/PCA/",component[[j]],".tif"),overwrite=T)

}
