
#pacotes ------------------------------------------------------------------------

library(tidyverse)
#-------------------------------------------------------------------------------


# caminho resultados

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results"

# listando tabelas
tbls <- list.files(p,".csv",full.names = T,recursive = T)


base_2020 <- grep(pattern = "baseline_2020",tbls,value = T)
fcnz <- grep(pattern = "fcnz",tbls,value = T)
base_2050 <- grep(pattern = "base/results/post_processed/tables/global",tbls,value = T)
base_2050 <- base_2050[!base_2050 %in% base_2020]
scenarios <- list(base_2020,base_2050,fcnz)
scen_name <- c("base_2020","base_2050","fcnz")
# loop pra salvar os resultados
#c=1


for(i in seq_along(scenarios)){

    df_results <- lapply(scenarios[[i]],read.csv)

    #nms2split <- list.files(base,".csv",full.names = F,recursive = T)

    nms_splitted <- str_split(string = scenarios[[i]],pattern = "/")

    nms <- list()

    if(scen_name[i]!="base_2020"){
      for(n in seq_along(nms_splitted)){

        nm <- nms_splitted[[n]][7]
        nms[n] <- nm
    }
      }else{
          for(n in seq_along(nms_splitted)){
        
            nm <- nms_splitted[[n]][8]
            nms[n] <- nm
            }}

    nms <- unlist(nms)
    names(df_results) <- nms

    # adicionando nome ecorregiao
    df_updated <- list()
    for(df in seq_along(df_results)){

      df2up <- df_results[[df]]
      df2up$ecoregion <- names(df_results)[[df]]
      df2up$scenario <- scen_name[i]
      df_updated[[df]] <- df2up


    }

    # precisa incluir ecorregiao nos dados
    # combining in one df

    df_cbnd <- do.call(rbind,df_updated)

    # eliminando duplicados

    df_cbnd <- unique(df_cbnd)

    # filtrando resultados de interesse (scen baseline so gerou uma linha)

    if(scen_name[i]!="base_2020"){
    df_cbnd <- filter(df_cbnd ,scenario_name=="Future land-use")
}else{df_cbnd <- df_cbnd <- df_cbnd}
    
    # format long

    clmns2keep <- grep(".val",x = names(df_cbnd))

    df_long <- pivot_longer(df_cbnd,cols = c(clmns2keep,17,18,19,22))[,c(14:17)]

    df_long2 <- df_long%>%
      # criando coluna com ano (acho q tem q excluir o q nao eh future lu)
      mutate(variable=if_else(name %in% c("VEG","AGR","PAS","area"),true="lulc","metrics"))%>%
      # filtrando dados indesejaveis
      filter(name!="target_value")%>%
      mutate(year=if_else(scenario=="base_2020",2020,2050))

    

  write.csv(df_long2,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_results_ecoregions_",scen_name[i],".csv"),row.names = F)

  #  c=c+1
  
  
  
  }


