
library(ggthemes)
library(ggmap)
library(viridis)

ecoregions <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")

cat <- as.data.frame(unique(values(ecoregions)))
names(cat) <- "ID"
legenda <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

cat <- left_join(cat,legenda)%>%filter(!is.nan(ID))

levels(ecoregions) <-cat
# refatorando

# limpando categorias sem importancia em area!

freq <- freq(ecoregions)
names(freq)[2] <- "Nome"
freq <- left_join(freq,legenda)
# remover os q nao tem area representativa

freq_sub <- filter(freq,count>4000)

plot(ecoregions=="Humid Chaco")

# vale ver o tamanho das ecoregioes cortadas. desconsiderar as q estao largamente fora

ecoregions_boundaries <- vect("/dados/bd_iis/terrestrial_ecoregions_olson/official/wwf_terr_ecos.shp")

# subset

ecoregions_boundaries <- st_as_sf(ecoregions_boundaries)  
ecoregions_boundariesdf <- ecoregions_boundaries 
coregions_boundariesdf <- sf::st_drop_geometry(ecoregions_boundariesdf)
eco_sub <-filter(ecoregions_boundaries,ECO_NAME=="Guianan savanna")

Br <- read_country()%>%st_transform(crs(eco_sub))
biomes <- read_biomes()%>% st_transform(crs(ecoregions_sub))
# Beni savanna ok descartar
# espinal tb
# dry chaco tb
# Southern Cone Mesopotamian savanna
# Humid Chaco

# ecoregioes q sao importantes fora

# Guianan Highlands moist forests
# Guianan piedmont and lowland moist forests
# continua...

plot(Br)
plot(st_geometry(eco_sub),add=T)

# subset ecoregions 

excluir <- c(13,257,266,68,208,677,326) # IDs para excluir

ecoregions_sub <- ecoregions

# da pra plotar com recorte de bioma. isso facilitaria a vizualizacao! (menos cores)
ecoregions_sub[ecoregions_sub%in%excluir] <- NA

color_palette <- terrain.colors(60)
plot(ecoregions_sub, col=color_palette, main="ecoregions", legend=F)
plot(st_geometry(biomes[1:6,]),add=T)

# associar ecoregiao ao bioma em q ela tem maior area inserida!

# Convert raster to sf for plotting with ggplot2

eco_df <- as.data.frame(ecoregions_sub,xy=T)

eco_map <- eco_df %>%
  # removendo NAs
  filter(!is.na(Nome))%>%
  ggplot() +
  geom_sf(data=biomes, fill="lightgray",color="black")+
  geom_tile(aes(x = x, y = y, fill = Nome)) +
  geom_sf(data=biomes, fill="NA",color="black")+
  #scale_fill_viridis_c(option = "magma", direction = -1,
  #                     labels = comma,trans = "log10") +
  scale_fill_viridis()+
  #labs( fill = "R$/ha")+
  theme_map()
  #theme(text=element_text(size=7),legend.position = c(0.02,0.70), legend.box = "horizontal")+
  # box da legenda transparente(nao funciona)
  #theme(legend.key = element_rect(fill = "transparent"))
