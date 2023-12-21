
#pacotes ------------------------------------------------------------------------

library(tidyverse)
#-------------------------------------------------------------------------------

# logica de todas as metricas. O denominador eh o cenario de referencia logo valores <1 indicam que o denominador eh maior que o cenario q esta sendo comparado. Porem, tem valores negativos e positivos: qndo eh positivo mas o valor absoluto do denominador eh maior, isso indica q houve melhora. se for menor eh piora.

# qndo da negativo e o valor do denominador eh menor = piora
# qndo da positivo e o valor do denominador eh maior = melhora

# criar uma tabela indicando se houve melhora ou piora


# caminho resultados

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results"

# listando tabelas

base_2020 <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/results/baseline_2020",".csv",full.names = T,recursive = T)
# base_2020 <- grep(pattern = "baseline_2020",tbls,value = T)
tbls <- list.files(p,recursive = T,".csv",full.names = T)
fcnz <- grep(pattern = "fcnz",tbls,value = T) # tem repetido
fcnzplus <- grep(pattern = "fcplusnz",tbls,value = T)
base_2050 <- grep(pattern = "base",tbls,value = T)
base_2050 <- grep(pattern = "baseline_2020",base_2050,value = T,invert = T)#tem repetido

scenarios <- list(base_2050,fcnz,fcnzplus)
scen_name <- c("base_2050","fcnz","fcnzplus")
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


# checando resultados

library(readr)
fcnzplus <- read_csv("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_results_ecoregions_fcnzplus.csv")

table(fcnzplus$name)# 51 regioes

fcnz <- read_csv("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_results_ecoregions_fcnz.csv")

table(fcnz$name)# 60 regioes - eh o correto

base2050 <- read_csv("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_results_ecoregions_base_2050.csv")

table(base2050$name)# 64 regioes - tem 4 a mais.

summary(fcnz) # tem 2 NAs!
summary(base2050) # tem 2 NAs tb!
summary(fcnzplus) # tem 1 NA

# ver qual eh NA

baseNA <- filter(base2050,is.na(value)) # 102,68
fcnzNA <- filter(fcnz,is.na(value)) #102,68
fcnzplusNA <- filter(fcnzplus,is.na(value)) #102

# quais sao essas ecorregioes?

eco <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

# 68 = Beni savanna. Essa ta na lista pra descartar!!
# 102 = Caqueta moist forests

ecoregion <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")

fre <- terra::freq(ecoregion)

plot(ecoregion==102)# essa ecorregiao deveria ser calculada, nao sei pq nao foi!
# tem q calcular, mas foi so pro it
