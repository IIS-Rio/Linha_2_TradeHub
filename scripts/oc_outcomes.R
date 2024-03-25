library(data.table)
library(tidyverse)

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/oc"

# oc

oc_restoration <- fread(file.path(p,"oc_restored_ecoregion.csv"))

excluir <- c(13,257,266,68,208,677,326)

oc_restoration <- filter(oc_restoration,!ecoregion_ID %in%excluir)

length(unique(oc_restoration$ecoregion_ID))


write.csv(oc_restoration,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/oc_reais_ecoregions.csv",row.names = F)

# restoration ic

ic <- fread(file.path(p,"implementation_costs_ecoregion.csv"))

ic <- filter(ic,!ecoregion_ID %in%excluir)

write.csv(ic,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/ic_reais_ecoregions.csv",row.names = F)
