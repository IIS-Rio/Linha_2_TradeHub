
#pacotes ------------------------------------------------------------------------

library(tidyverse)
library(stringr)
#-------------------------------------------------------------------------------

# logica de todas as metricas. O denominador eh o cenario de referencia logo valores <1 indicam que o denominador eh maior que o cenario q esta sendo comparado. Porem, tem valores negativos e positivos: qndo eh positivo mas o valor absoluto do denominador eh maior, isso indica q houve melhora. se for menor eh piora.

# qndo da negativo e o valor do denominador eh menor = piora
# qndo da positivo e o valor do denominador eh maior = melhora

# criar uma tabela indicando se houve melhora ou piora


# caminho resultados

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_up2/"

# listando tabelas

# base_2020 <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/results_up/baseline_2020",".csv",full.names = T,recursive = T)
# # base_2020 <- grep(pattern = "baseline_2020",tbls,value = T)
tbls <- list.files(p,recursive = T,".csv",full.names = T)
fcnz <- grep(pattern = "fcnz",tbls,value = T) # tem repetido
fcnzplus <- grep(pattern = "fcplusnz",tbls,value = T)
base_2050 <- grep(pattern = "base",tbls,value = T)

scenarios <- list(base_2050,fcnz,fcnzplus)
scen_name <- c("base_2050","fcnz","fcnzplus")


for(i in seq_along(scenarios)){

    df_results <- lapply(scenarios[[i]],read.csv)

    #nms2split <- list.files(base,".csv",full.names = F,recursive = T)

    nms_splitted <- str_split(string = scenarios[[i]],pattern = "/")

    nms <- list()
      for(n in seq_along(nms_splitted)){
            nm <- nms_splitted[[n]][8]
            nms[n] <- nm
            }

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

    df_cbnd <- filter(df_cbnd ,scenario_name=="Future land-use")

    # format long

    clmns2keep <- grep(".val",x = names(df_cbnd))

    df_long <- pivot_longer(df_cbnd,cols = c(clmns2keep,17,18,19,22))[,c(14:17)]

    df_long2 <- df_long%>%
      # criando coluna com ano (acho q tem q excluir o q nao eh future lu)
      mutate(variable=if_else(name %in% c("VEG","AGR","PAS","area"),true="lulc","metrics"))%>%
      # filtrando dados indesejaveis (exclui cb e oc q sao calculados por fora)
      filter(name!="target_value",
             name %in% c("it.val","bd.val","ec.val"))%>%
      mutate(year=if_else(scenario=="base_2020",2020,2050),
             # adicionando valor 0 pra regioes q nao tiveram mudança no it
             value=if_else(is.na(value),0,value))
      
    

  write.csv(df_long2,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_bio_results_ecoregions_",scen_name[i],".csv"),row.names = F)

  #  c=c+1
  
  
  
  }


