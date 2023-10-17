# taxa de transicao eh o valor da transicao/area da classe do ano anterior


#- pacotes ---------------------------------------------------------------------

library(sf)
library(dplyr)
library(tidyverse)

#-------------------------------------------------------------------------------

# caminho transicoes

p5 <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions_final_use"

# caminho area

p4 <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated/GPKG"

#-------------------------------------------------------------------------------

#- gpkgs ja reclassificados ----------------------------------------------------


# listando gpks transicao

transitions_rec_files <- list.files(p5,full.names = T)

# so os nomes pra salvar

transitions_rec_nms <- list.files(p5,full.names = F)

# listando area total(so 3 cenarios vao ser usados base,fcnz,fcplusnz)

area_total <- list.files(p4,full.names = T)[c(1,3,5)] #esses cenarios

# listas pra salvar resultados

# transicoes completas, juntando os dados corrigidos aos antigos

novas_transicoes <- list()

for(i in 1:length(transitions_rec_files)){
    
  # abrindo transicao
  
  transicao <- st_read(transitions_rec_files[i])
  
  area <- st_read(area_total[i])%>%
    # adicionar coluna com o ano
    mutate(year = as.numeric(substr(name, nchar(name) - 3, nchar(name))),
           name = substr(name, 1, nchar(name) - 4))
  
  # corrigindo nomes
  
  area$name <-   gsub(area$name,pattern = paste(c("GrsLnd"),collapse = "|"),replacement = "Grslnd")
  
  area$name <-   gsub(area$name,pattern = paste(c("NatLnd"),collapse = "|"),replacement = "Natlnd")
  
  area$name <-   gsub(area$name,pattern = paste(c("NatVeg"),collapse = "|"),replacement = "Natveg")
  
  
  # subset por ano pra calcular as taxas
  
  anos <- seq(2025,2050,5)

  for(z in 1:length(anos)){
  
    transicao_s <- filter(transicao,year==anos[z])
    
    area_s <- filter(area,year==anos[z]-5)%>%
      rename(from=name,
             area_from=value)
    
    # juntando area da classe
    
    st_geometry(transicao_s) <- NULL
    st_geometry(area_s) <- NULL
    
    trans_area <- left_join(transicao_s,area_s[,c(1:4)])
      
    # calculando taxa
    
    trans_area$taxa <- trans_area$value/trans_area$area_from
    
    # substituir NAN por 0
    
    trans_area$taxa[is.nan(trans_area$taxa)] <- 0
    
    # separando as taxas >1
    
    trans_area_maior1 <- filter(trans_area,taxa>1)
    
    # Ids com problema!
    
    IDs <- unique(trans_area_maior1$ID)
    
    # taxas recalculadas qndo >1
    
    recalculando_taxas <- list()
    
    for(j in 1:length(IDs))  {
      
      # filtar o df completo, nao so a taxa anomala
      trans_area_maior1_ID <- filter(trans_area,ID==IDs[j])
      
      # os valores de area_from tem q ser atualizados. fazer so pra divisao problematica
      # aqui pode ser >1. se for igual a 1 ai ta certo.
      
      lu_errado <- trans_area_maior1_ID$from[trans_area_maior1_ID$taxa>1]
      
      # lista vazia pra guardar qndo tem > 1 taxa errada
      
      #lista_area_maior_1 <- list() # apagar!
      
      if(length(lu_errado)==1){      
        
      # somar area inicial + ganho area
      
        area_final <- unique(trans_area_maior1_ID$area_from[trans_area_maior1_ID$from==lu_errado]) + sum(trans_area_maior1_ID$value[trans_area_maior1_ID$to==lu_errado]) 
      
      # se decontar conversoes da errado!  
      #- sum(trans_area_maior1_ID$value[trans_area_maior1_ID$from==lu_errado])
        
        # atualizando area apenas da celula com taxa maior zero
        
        trans_area_maior1_ID$area_from[trans_area_maior1_ID$from==lu_errado&trans_area_maior1_ID$taxa>1] <- area_final
        
        trans_area_maior1_ID$taxa <- trans_area_maior1_ID$value/trans_area_maior1_ID$area_from
        
        # removendo NANS novamente
        
        trans_area_maior1_ID$taxa[is.nan(trans_area_maior1_ID$taxa)] <- 0
        
        # guardando em lista vazia (rever como indexar aqui")
        
        recalculando_taxas[[j]] <- trans_area_maior1_ID
        #print(c(IDs[j],nrow(trans_area_maior1_ID),j)) # checando
      
      }
      
      else{
        
        for(uso in 1:length(lu_errado)){
          
          area_final <- unique(trans_area_maior1_ID$area_from[trans_area_maior1_ID$from==lu_errado[uso]]) + sum(trans_area_maior1_ID$value[trans_area_maior1_ID$to==lu_errado[uso]])
          
          trans_area_maior1_ID$area_from[trans_area_maior1_ID$from==lu_errado[uso]&trans_area_maior1_ID$taxa>1] <- area_final
          
          trans_area_maior1_ID$taxa <- trans_area_maior1_ID$value/trans_area_maior1_ID$area_from
          
          # removendo NANS novamente
          
          trans_area_maior1_ID$taxa[is.nan(trans_area_maior1_ID$taxa)] <- 0
          
          #lista_area_maior_1[[uso]] <- trans_area_maior1_ID
        
        }
        
        #trans_area_maior1_ID_combined <- do.call(rbind,lista_area_maior_1)
        # guardando em lista vazia (rever como indexar aqui")
        recalculando_taxas[[j]] <- trans_area_maior1_ID
      }
    
    }
    
    # remontando df
    
    recalculando_taxas_df <- do.call(rbind,recalculando_taxas) # aqui tem problemas
    
    # df completo, excluidos os ids onde recalculamos área
    
    trans_area_sub <- trans_area[!trans_area$ID %in% recalculando_taxas_df$ID,]
    
    # juntando dados corretos
    
    trans_area2 <- rbind(trans_area_sub,recalculando_taxas_df)
    
    # guarando anos com taxa calculada
    novas_transicoes[[z]] <- trans_area2
    #print(c(anos[z],nrow(trans_area2))) 
  }
  
  # montando df novamente 
  novas_transicoes_df <- do.call(rbind,novas_transicoes)
  # falta adicionar ano 2020
  df_2020 <- transicao[transicao$year==2020,]
  # adicionando area_from e taxa
  df_2020 <- df_2020%>%
    mutate(area_from=0,
           taxa=0)
  # removendo geometria
  st_geometry(df_2020) <- NULL
  # juntando
  df_final <- rbind(novas_transicoes_df,df_2020)
  # adicionando parte espacial
  df_geo <- left_join(df_final,transicao)
  # salvando
  write_sf(df_geo,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated_transitions/GPKGs_transition_rate/rate_",transitions_rec_nms[i]))
  }

