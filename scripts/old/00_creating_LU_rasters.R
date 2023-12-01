#-------------------------------------------------------------------------------

# antes disso, transformação em raster!!

# agregação dos usos do solo conforme descrito:

# NatVeg: vegetação nativa
# Restor: restauração de vegetação nativa
# CrpLnd: agricultura (todas as culturas agrícolas simuladas no modelo)
# GrsLnd: pastagens para livestock
# PltFor: florestas plantadas (exemplo: produção de papel, polpa de celulose)
# NatLnd: terras não produtivas e/ou abandonadas
# Others: outras classes de uso da terra que não mudam ao longo das simulações

# Para:
#   
# 1.  Vegetação: NatVeg + Restor | 2. Agricultura: CrpLnd + PltFor | 3. Pastagem: GrsLnd | 4. Ignoradas: NatLnd + Others


# os dados estão todos em "/dados/projetos_andamento/TRADEhub/Linha_2"

#-------------------------------------------------------------------------------

#- pacotes ---------------------------------------------------------------------

library(sf)
library(raster)
library(tidyverse)

#-------------------------------------------------------------------------------


# cenarios

cen <- c("BASE","FC","FCNZ","FCplus","FCplusNZ")

# listando cenarios

l_scen <- list.files(path ="/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_original/GPKG",full.names = T )

# abrindo shapes

cen_sp <- lapply(X = l_scen,st_read)

# projeta

cen_sp_pj <- lapply(cen_sp,function(x)st_transform(x,proj))

# repara geometria

cen_sp_pj <-lapply(cen_sp_pj,function(x)st_make_valid(x))

# calcula area em m^2(vale pra todos)

area <- st_area(cen_sp_pj[[1]])


# # plotando uma celula complea
# 
# full_cell <- f_pj[8,]
# st_area(full_cell)
# plot(st_geometry(f_pj[8,]))

# plotando celula com area maxima

# cell_max <- filter(f_pj,area>=max(f_pj$area))
# plot(st_geometry(cell_max))
# st_area(cell_max)

# summary(f_pj$area/10^6)

# as celulas nao tem areas iguais...pego a maior? todas tb sao maiores que 50x50
# assumindo a maior, se fosse quadrado, seria area maior/2


# calculando lateral, assumindo quadrado

cell_side <-sqrt(as.numeric(max(area)))
# isso eh a lateral em km2
cell_side/10^3 # da 55 km mais ou menos. ok

# r base pra rasterizar, vale pra todos

r_base <- raster(extent(cen_sp_pj[[1]]), resolution = c(cell_side, cell_side))

# aqui tem q ser um loop ja, excluindo apenas 

for (j in 1:length(cen_sp_pj)) {
  
  f_pj <- cen_sp_pj[[j]]
  classes <- setdiff(names(f_pj), c("geom", "ID", "Country",paste0("CRarea",seq(2020,2050,5))))
  for(i in 1:length(classes)){
    # Get the last four elements to get the year
    year <- substr(classes[i], nchar(classes[i]) - 3, nchar(classes[i]))
    #column with cell area
    cell_area <- (paste0("CRarea",year))
    #subset do grid
    f_s <- f_pj %>% select(c(classes[i],cell_area))
    # calculando share
    f_s <- f_s %>% mutate(share = .[[1]] / .[[2]])
    # rasterizar
    f_r <- rasterize(f_s,r_base,field="share")
    crs(f_r) <- proj
    # criando pasta com o cenario
    p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_rasterized"
    dest <- file.path(p,cen[j])
    dir.create(dest)
    raster::writeRaster(f_r,filename = paste0(dest,"/",cen[j],"_",classes[i],".tif"),overwrite=T)
    }

  }









