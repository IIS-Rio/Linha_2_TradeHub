#-Obs --------------------------------------------------------------------------

# os dados para uma mesma célula tem transicoes intermediárias. Pra nós só interessam
# as transicoes finais. 

# no baseline sao 11 celulas com "problemas"
# 106 com problema no fcnz

#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(sf)
library(dplyr)
library(tidyverse)

#-------------------------------------------------------------------------------

# caminho transicoes

p3 <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions/GPKG/"

# caminho area

p4 <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated/GPKG"

#-------------------------------------------------------------------------------

#- gpkgs ja reclassificados ----------------------------------------------------


# listando gpks transicao

transitions_rec_files <- list.files(p3,full.names = T)

# so os nomes pra salvar

nms_scenarios <- list.files(p3,full.names = F)

# listando gpks area

area_rec_files <- list.files(p4,full.names = T)

# criando listas pra guardar

lista_grids <- list()


# abrindo transicao

transicao_rec <- st_read(transitions_rec_files[i])

# mudando formato

transition_rec_long <- transicao_rec %>%pivot_longer(cols = where(is.numeric))%>%
  # adicionar coluna com o ano
  mutate(year = as.numeric(substr(name, nchar(name) - 3, nchar(name))),
       name = substr(name, 1, nchar(name) - 4))
# testar fazer pra todas as celulas, sem considerar so as problematicas

# readequando nomes

# GrsLnd pra Grslnd
# NatLnd pra Natlnd
# NatVeg pra Natveg

transition_rec_long$name <-   gsub(transition_rec_long$name,pattern = paste(c("GrsLnd"),collapse = "|"),replacement = "Grslnd")

transition_rec_long$name <-   gsub(transition_rec_long$name,pattern = paste(c("NatLnd"),collapse = "|"),replacement = "Natlnd")

transition_rec_long$name <-   gsub(transition_rec_long$name,pattern = paste(c("NatVeg"),collapse = "|"),replacement = "Natveg")

# ids unicos

IDs <- unique(transition_rec_long$ID)

# split coluns

transition_rec_long <- transition_rec_long %>%
  mutate(from = str_extract(name, "^[A-Z][a-z]+"),
         to = str_extract(name, "[A-Z][a-z]+$"))%>%
  # apagar coluna
  select(-4)


# contador de celulas com problema

c <- 0

  for(j in 1:length(IDs)){
    
    grid <- filter(transition_rec_long,ID==IDs[j])
  
  # subset areas de transicao com valores iguais (assumindo que os problemas so acontecem qndo areas iguai sao transicionadas ao mesmo tempo - tem q ser no mesmo ano). Se esse filtro for igual a zero, ir pra proxima celula.
  
    areas_iguais <- grid%>%
      group_by(value,year)%>%
      filter(n() > 1& value!=0)
    
    if(nrow(areas_iguais)!=0){
  
  # aqui eu tenho que determinar qual land-use tem área final 0
  
  # abrindo area final
  
  area_final <- st_read(area_rec_files[i])%>%
    # adicionar coluna com o ano
    mutate(year = as.numeric(substr(name, nchar(name) - 3, nchar(name))),
           name = substr(name, 1, nchar(name) - 4))
    
  area_final$name <-   gsub(area_final$name,pattern = paste(c("GrsLnd"),collapse = "|"),replacement = "Grslnd")
  
  area_final$name <-   gsub(area_final$name,pattern = paste(c("NatLnd"),collapse = "|"),replacement = "Natlnd")
  
  area_final$name <-   gsub(area_final$name,pattern = paste(c("NatVeg"),collapse = "|"),replacement = "Natveg")
  
  # filtrando celula 
  
  area_grid <- filter(area_final,ID==IDs[j])%>%
    # retendo apenas areas final==0
    filter(value==0)
  
  # tem q filtrar ano
  
  # determinando ano (tem q ser sempre ano anterior)
  year_to_subset <- unique(areas_iguais$year) - 5
  
  # qndo a area igual nao eh do ano que a classe com valor 0, nao precisa corrigir

  area_grid_ano_anterior <- area_grid %>%
    filter(year  %in% year_to_subset )
  
  if(nrow(area_grid_ano_anterior)==0){
    
    grid_corrected_sum <- grid 
    
  }else{
  
  # reter apenas transicao da área que nao é 0
  
  # padrao pra reter
  
  class2retain <- grep(pattern = paste(area_grid_ano_anterior$name,collapse = "|"),x = areas_iguais$to,value = T,invert = T)
    
  area_transicao_final <- filter(areas_iguais, to %in% area_grid_ano_anterior)
  
  if(nrow(area_transicao_final)==0){
    
    grid_corrected_sum <- grid
    
  }else { 
    
  # falta subsituit o to 
  
  
    
    area_transicao_final$to <- class2retain
  
  # corrigir nome
  
  #names(area_transicao_final)[4] <- "old_trans_nm"
  
  # pegar o subset do grid, subtrair as classes com transicao simultanea e adicionar a versao correta
  
  # drop geometry
  
  st_geometry(areas_iguais) <- NULL
  
  grid_reduced <- anti_join(x = grid,y = areas_iguais)
  
  
  # padronizar nome
  
  #names(grid_reduced)[4] <- "old_trans_nm"
  
  # projetar area transicao
  
  area_transicao_final_sf <- st_as_sf(area_transicao_final)
  
  st_crs(area_transicao_final_sf) <- st_crs(grid_reduced)
  
  
  # adicionar transicao corrigida
  
  grid_corrected <- rbind(grid_reduced,area_transicao_final_sf)
  
  # juntando as classes (aqui ta errado pq fica com uma coluna a menos!tem q tirar a coluna old_trans_nm
  
  
  grid_corrected_sum <- grid_corrected%>%
    group_by(Country,ID,geom,year,from,to)%>%
    summarise(value=sum(value))
  
  c = c +1
  print(c)
  }
  
  }
  }else{
      
      grid_corrected_sum <- grid 
        
    }
  
  # guarando na lista pra depois gerar o novo dataframe
  
  lista_grids[[j]] <- grid_corrected_sum
  
  }

# juntando tudo novamente


transition_rec_long_corrigido <- do.call(rbind,lista_grids)


# pro fcpluz nz nao precisa rodar, pq so da problema com restored, q ja dei merge com veg nat.

st_write(transition_rec_long_corrigido,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions_final_use/adjusted_",nms_scenarios[i]))


st_write(transition_rec_long,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions_final_use/adjusted_",nms_scenarios[i]))
