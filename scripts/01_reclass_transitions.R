#-------------------------------------------------------------------------------

# agregação dos usos do solo conforme descrito:

# NatVeg: vegetação nativa
# Restor: restauração de vegetação nativa (nao tem essa classe)
# CrpLnd: agricultura (todas as culturas agrícolas simuladas no modelo)
# GrsLnd: pastagens para livestock
# PltFor: florestas plantadas (exemplo: produção de papel, polpa de celulose)
# NatLnd: terras não produtivas e/ou abandonadas
# Others: outras classes de uso da terra que não mudam ao longo das simulações

# Para:
#   
# 1.  Vegetação: NatVeg + Restor | 2. Agricultura: CrpLnd + PltFor | 3. Pastagem: GrsLnd | 4. Ignoradas: NatLnd + Others

#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(sf)
library(dplyr)
library(tidyverse)

#-------------------------------------------------------------------------------

# caminho
p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_original_transitions/2023-September-20/GPKG"

#-------------------------------------------------------------------------------
# reclassificando gpkgs
#-------------------------------------------------------------------------------

# listando gpks

gpks <- list.files(p,full.names = T)

# so os nomes pra salvar

nms_gpks <- list.files(p,full.names = F)

for(i in 1:length(gpks)){
  
  # abrindo gpk
  
  lu <- st_read(gpks[i])

  # mucando estrutura
  
  #lu_long <- pivot_longer(lu,cols = c(3:65))
  lu_long <- lu %>%pivot_longer(cols = where(is.numeric))
  
  lu_long$name <-   gsub(lu_long$name,pattern = paste(c("CrpLnd","PltFor"),collapse = "|"),replacement = "Agrclture")
  
  
  # summarizing 
  
  lu_long_sum <- lu_long%>%
    #st_group_by(geometry) %>%
    group_by(name,geom,Country,ID)%>%
    summarise(value=sum(value))
  
  # voltando pra wide
  
  lu_wide <- pivot_wider(lu_long_sum )
  
  # salvando gpk
  
  st_write(lu_wide,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions/GPKG/rec_",nms_gpks[i]))

}


# testando

teste <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions/GPKG/rec_MATRIX_LUC_FC.gpkg")

#-------------------------------------------------------------------------------
# reclassificando csvs
#-------------------------------------------------------------------------------

p2 <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_original_transitions/2023-September-20/CSV"

# listando gpks

csvs <- list.files(p2,full.names = T)

# so os nomes pra salvar

nms_csvs <- list.files(p2,full.names = F)

# abrindo csv

for(i in 1:length(csvs)) {
    
    lu <- read.csv(csvs[i],header = F)
  
    # renaming
    
    names(lu) <- c("Country","ID","from","to","scen","year","area")
    
    # substituindo nomes
    
    lu$from <-   gsub(lu$from,pattern = paste(c("CrpLnd","PltFor"),collapse = "|"),replacement = "Agrclture")
    
    lu$to <-   gsub(lu$to,pattern = paste(c("CrpLnd","PltFor"),collapse = "|"),replacement = "Agrclture")
    
    
    # summarizing (ta estranho, nao ta mudando muito o n de linhas)
    
    lu_sum <- lu%>%
      #st_group_by(geometry) %>%
      group_by(Country,ID,from,to,scen,year)%>%
      summarise(area=sum(area))
    
    # salvando gpk
    
    write.csv(lu_sum,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions/CSV/rec_",nms_csvs[i]),row.names = F)
}