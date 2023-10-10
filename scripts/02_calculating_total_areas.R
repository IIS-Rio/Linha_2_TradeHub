#-pacotes-----------------------------------------------------------------------

library(sf)
library(tidyverse)

#-------------------------------------------------------------------------------

#- obs -------------------------------------------------------------------------

# agregação dos usos do solo conforme descrito:

# NatVeg: vegetação nativa
# Restor: restauração de vegetação nativa (nao tem essa classe)
# CrpLnd: agricultura (todas as culturas agrícolas simuladas no modelo)
# GrsLnd: pastagens para livestock
# PltFor: florestas plantadas (exemplo: produção de papel, polpa de celulose)
# NatLnd: terras não produtivas e/ou abandonadas
# Others: outras classes de uso da terra que não mudam ao longo das simulações

# Para:
#   
# 1.  Vegetação: NatVeg + Restor | 2. Agricultura: CrpLnd + PltFor | 3. Pastagem: GrsLnd | 4. Ignoradas: NatLnd + Others
#--------------------------------------------------------------------------------


# calculo da área total de cada classe de uso da terra

# caminho dos rasters de lu

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_original/GPKG"


# listando arquivos

l_lus <- list.files(p,full.names = T)

# nomes pra salvar

# so os nomes pra salvar

nms_lus <- list.files(p,full.names = F)




for (i in 1:length(l_lus)) {
  
  # abrindo
  lu <- st_read(l_lus[i])
  
  # mucando estrutura
  
  lu_long <- lu %>%pivot_longer(cols =10:51)
  
  lu_long$name <-   gsub(lu_long$name,pattern = paste(c("CrpLnd","PltFor"),collapse = "|"),replacement = "Agrclture")
  
  # summarizing 
  
  lu_long_sum <- lu_long%>%
    #st_group_by(geometry) %>%
    group_by(name,geom,Country,ID)%>%
    summarise(value=sum(value))
  
  # # voltando pra wide
  # 
  # lu_wide <- pivot_wider(lu_long_sum )
  
  
  st_write(lu_long_sum,paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated/GPKG/total_area_agg_",nms_lus[i]))
  
  }

grep(pattern = "res",x = unique(lu_long_sum$name))
