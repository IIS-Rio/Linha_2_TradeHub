# pacotes ----------------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------

# caminho resultados

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_up/baseline_2020/"

# listando tabelas

base_2020_agg <- grep(pattern = ".csv",list.files(p,"agg",full.names = T,recursive = T),value = T) # so tem 58! sacooooooo

base_2020_cum <- list.files(p,"cum",full.names = T,recursive = T) # so tem 58! sacooooooo

# loop pra salvar os resultados

tabelas <- list(base_2020_agg,base_2020_cum)
tabelas_final <- list()

for(i in seq_along(tabelas)){
  
  df_results <- lapply(tabelas[[i]],read.csv)
  
  #nms2split <- list.files(base,".csv",full.names = F,recursive = T)
  
  nms_splitted <- str_split(string =tabelas[[i]],pattern = "/")
  
  nms <- list()
  
    for(n in seq_along(nms_splitted)){
      
      nm <- nms_splitted[[n]][9]
      nms[n] <- nm
    }
  
  nms <- unlist(nms)
  names(df_results) <- nms
  
  # adicionando nome ecorregiao
  df_updated <- list()
  for(df in seq_along(df_results)){
    
    df2up <- df_results[[df]]
    df2up$ecoregion <- names(df_results)[[df]]
    df2up$scenario <- "base_2020"
    df_updated[[df]] <- df2up
    
    
  }
  
  # precisa incluir ecorregiao nos dados
  # combining in one df
  
  df_cbnd <- do.call(rbind,df_updated)
  
  # eliminando duplicados
  
  df_cbnd <- unique(df_cbnd)
  
  # filtrando resultados de interesse (scen baseline so gerou uma linha)
 
  # format long
  
  clmns2keep <- grep(".val",x = names(df_cbnd))
  
  df_long <- pivot_longer(df_cbnd,cols = c(clmns2keep,17,18,19,22))[,c(14:17)]
  
  df_long2 <- df_long%>%
    # criando coluna com ano (acho q tem q excluir o q nao eh future lu)
    mutate(variable=if_else(name %in% c("VEG","AGR","PAS","area"),true="lulc","metrics"))%>%
    # filtrando dados indesejaveis
    filter(name!="target_value")%>%
    mutate(year=if_else(scenario=="base_2020",2020,2050))
  
  tabelas_final[[i]] <- df_long2
  
}

# tabelas finais. Fazer valor cumulativo menos agregado


tabela_final <- tabelas_final[[2]]%>%
  rename(cum_value=value)%>%
  left_join(tabelas_final[[1]],by=c("ecoregion","name","scenario","variable","year"))%>%
  rename(agg_value=value)%>%
  mutate(value_2020=cum_value-agg_value)


tabela_final <- as.data.frame(tabela_final)

write.csv(tabela_final,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_results_ecoregions_","base_2020",".csv"),row.names = F)


