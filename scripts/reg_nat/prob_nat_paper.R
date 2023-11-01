
library(raster)
library(geobr)
library(sf)

br <- read_country()%>%
  st_transform(crs(r))


r1 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-40_-34_-15_-5.tif")
r2 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-60_-50_-15_-5.tif")
r3 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-80_-70_-5_5.tif")#
r4 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-40_-34_-5_5.tif")
r5 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-40_-38_-25_-15.tif")
r6 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-50_-40_-15_-5.tif")
r7 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-50_-40_-25_-15.tif")
r8 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-50_-40_-5_5.tif")
r9 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-60_-50_-25_-15.tif")
r10 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-60_-50_-5_5.tif")
r11 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-70_-60_-15_-5.tif")
r12 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-70_-60_-25_-15.tif")
r13 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-70_-60_-5_5.tif")
r14 <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/prob_reg_natural/pnv_pct_30m_tile_-80_-70_-15_-5.tif")



plot(st_geometry(br))
plot(r1,add=T)
plot(r2,add=T)
plot(r3,add=T)
plot(r4,add=T)
plot(r5,add=T)
plot(r6,add=T)
plot(r7,add=T)
plot(r8,add=T)
plot(r9,add=T)#
plot(r10,add=T)
plot(r11,add=T)
plot(r12,add=T)
plot(r13,add=T)
plot(r14,add=T)


# ok!