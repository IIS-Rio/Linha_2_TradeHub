p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_national"

lst_tbls <- list.files(p,full.names = T,recursive = T,pattern = ".csv")

scenarios <- c("baseline","fcnz","fcnzplus")

lst_tbls2 <- list()

for(i in seq_along(lst_tbls)){
  
  df <- fread(lst_tbls[[i]])%>%
    filter(scenario_name=="Future land-use")
  # format long
  
  clmns2keep <- grep(".val",x = names(df))
  
  df_l <- df%>%select(clmns2keep)%>%
    pivot_longer(1:5)%>%
    mutate(scenarios=scenarios[i],
           year="2050")%>%
    select(-1)%>%
    filter(name %in% c("bd.val","it.val","ec.val"))
    
    lst_tbls2[[i]] <- df_l
  
}


combined_df <- do.call(rbind,lst_tbls2)

write.csv(combined_df,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_national_results_raw.csv",row.names = F)
