# pacotes ----------------------------------------------------------------------
#library(scales)
#library(ggpubr)
#library(ggmap)
#library(RColorBrewer)
#library(sf)
#library(geobr)
#library(ggthemes)
library(tidyr)
library(data.table)
#-------------------------------------------------------------------------------

# resultados biodiv

bio_tbl <- do.call(rbind,lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables",full.names = T,pattern = "plangea"),fread))%>%
  rename(ecoregion_ID=ecoregion)

# carbon

cb_tbl <-lapply(list.files("/dados/projetos_andamento/TRADEhub/Linha_2/cb",full.names = T),fread)

cb_em <- cb_tbl[[1]]%>%
  pivot_longer(2)%>%
  mutate(variable="carbon balance",
         year=2050,
         value=value*-1)%>%
  rename(scenario=scen)

cb_seq <- cb_tbl[[2]]%>%
  pivot_longer(2)%>%
  mutate(variable="carbon balance",
         year=2050,
         value=value*-1)%>%
  rename(scenario=scen)

# calculando net carbon

net_cb <- cb_em %>%
  rename(emited=value)%>%
  left_join(cb_seq[,c(1,2,4)],by=c("scenario","ecoregion_ID"))%>%
  rename(seq=value)%>%
  mutate(value=emited+seq,
         name="net_CO2")


final_tbl <- rbind(bio_tbl,cb_seq,cb_em,net_cb[,c(1:3,5,6,8)])

# excluir ecorregioes

excluir <- c(13,257,266,68,208,677,326)

final_tbl2 <- final_tbl%>%filter(!ecoregion_ID%in%excluir)

# normalizando valores metricas (mantendo todos e eliminando valores)

bio_tbl_sc <- bio_tbl %>%
  # Remove extreme values for ratio 2050/base 2050
  mutate(value_skewed = if_else(value < -11.189, -11, if_else(value > 1, 1, value)))

hist(bio_tbl_sc$value)
hist(bio_tbl_sc$value_sc)
hist(bio_tbl_sc$value_skewed)


# salvando resultados

# write.csv(df_comb,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_rescaled.csv",row.names = F)

write.csv(final_tbl2,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_raw.csv",row.names = F)

write.csv(bio_tbl_sc,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_results.csv",row.names = F)
