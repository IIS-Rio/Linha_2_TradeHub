p <- "/dados/projetos_andamento/TRADEhub/Linha_2/oc"

oc_restoration <- fread(file.path(p,"oc_restored_ecoregion.csv"))

excluir <- c(13,257,266,68,208,677,326)

oc_restoration <- filter(oc_restoration,!ecoregion_ID %in%excluir)

length(unique(oc_restoration$ecoregion_ID))


write.csv(oc_restoration,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/oc_reais_ecoregions.csv",row.names = F)
