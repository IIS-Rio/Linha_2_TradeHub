#-------------------------------------------------------------------------------

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

#-------------------------------------------------------------------------------

# caminho
p <- "/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_rasterized"
# cenarios
cen <- c("BASE","FC","FCNZ","FCplus","FCplusNZ")
# anos
anos <- seq(2020,2050,5)
# listando rasters por cenario


for(i in 1:length(cen)){
  
  r_l <- list.files(file.path(p,cen[i]),full.names = T)
  for(j in 1:length(anos)){
  # selecionando pra um mesmo ano
  
  r_ano <- grep(pattern =anos[j],r_l,value = T)
  
  raster2save <- list()
  
  
  # precisa agregar cropland e planted forest/ignoradas: natland + others
  
  agri <- grep(paste(c("CrpLnd","PltFor"),collapse = "|"),r_ano,value = T)
  
  # transformando em raster e somando
  
  agri_r <- lapply(agri,raster)
  agri_sum <- Reduce("+",agri_r)
  
  raster2save[[1]] <- agri_sum
  
  # ignoradas
  
  ign <- grep(paste(c("NatLnd","Others"),collapse = "|"),r_ano,value = T)
  ign_r <- lapply(ign,raster)
  ign_sum <- Reduce("+",ign_r)
  
  raster2save[[2]] <- ign_sum
  
  # nat veg
  
  veg <- grep(paste(c("NatVeg"),collapse = "|"),r_ano,value = T)
  
  raster2save[[3]] <- raster(veg)
  
  # pastagem
  
  past <- grep(paste(c("GrsLnd"),collapse = "|"),r_ano,value = T)
  
  raster2save[[4]] <-raster(past)
  
  names(raster2save) <- c("agriculture","ignored","natural_vegetation","grassland")
  
  # salvando
  
  for(c in 1:length(raster2save)){
    dest <- paste0("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated","/",cen[i])
    dir.create(dest)
  
    writeRaster(raster2save[[c]],paste0(dest,"/",cen[i],"_",names(raster2save)[c],"_",anos[j],".tif"))
              
  }
  
  }
  
  

}


# testando soma1

l <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated/BASE",full.names = T)

lg <- grep("2020",l,value = T)
s <- Reduce("+",lapply(lg,raster))
plot(round(s,2))
