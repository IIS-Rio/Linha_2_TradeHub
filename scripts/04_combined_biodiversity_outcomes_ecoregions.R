
#pacotes ------------------------------------------------------------------------

library(tidyverse)

#-------------------------------------------------------------------------------


# caminho resultados

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results"

# listando tabelas

df_results <- lapply(list.files(p,".csv",full.names = T,recursive = T),read.csv)

nms2split <- list.files(p,".csv",full.names = F,recursive = T)

nms_splitted <- str_split(string = nms2split,pattern = "/")

nms <- list()

for(n in seq_along(nms_splitted)){
  
  nm <- nms_splitted[[n]][1]
  nms[n] <- nm
}

nms <- unlist(nms)



names(df_results) <- nms

# adicionando nome ecorregiao
df_updated <- list()
for(df in seq_along(df_results)){
  
  df2up <- df_results[[df]]
  df2up$ecoregion <- names(df_results)[[df]]
  df2up$scenario <- "base"
  df_updated[[df]] <- df2up
}



# precisa incluir ecorregiao nos dados


# combining in one df

df_cbnd <- do.call(rbind,df_updated)

# eliminando duplicados

df_cbnd <- unique(df_cbnd)

# format long

clmns2keep <- grep(".val",x = names(df_cbnd))

df_long <- pivot_longer(df_cbnd,cols = clmns2keep)[,c(1,12,13,14,16,18:21)]

df_long2 <- df_long%>%
  # criando coluna com ano
  mutate(year=if_else(scenario_name=="Future land-use",true=2050,2020))%>%
  # filtrando dados indesejaveis
  filter(name!="target_value")

# separando values 2 c ol


df_future <- filter(df_long2,year==2050)
df_2020 <- filter(df_long2,year==2020)
names(df_2020)[9] <- "value_2020"
df_final <- cbind(df_future,df_2020[,c(9)])


