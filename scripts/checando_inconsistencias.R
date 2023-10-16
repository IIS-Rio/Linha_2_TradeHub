# segundo csv enviado pelo pietro, algumas celulas tem area de transicao > que a area total da classe inicial que sofre a conversao na mesma celula. Por ex. essas do cenário base:

# ID	year	cod_from	Country	from	to	scen	area	fid	cod_to	transition	area_total	taxa
# CR255182	2050	3	Brazil	GrsLnd	Agrclture	BASE	2.15	1463	2	3 2	0	Inf
# CR255214	2050	3	Brazil	GrsLnd	Agrclture	BASE	11.64	1495	2	3 2	0	Inf
# CR266218	2050	3	Brazil	GrsLnd	Agrclture	BASE	27.55	2203	2	3 2	0	Inf
# CR267217	2050	3	Brazil	GrsLnd	Agrclture	BASE	32.71	2251	2	3 2	0	Inf
# CR268214	2045	3	Brazil	GrsLnd	Agrclture	BASE	64.94	2296	2	3 2	0	Inf
# CR268217	2045	3	Brazil	GrsLnd	Agrclture	BASE	13	2299	2	3 2	0	Inf
# CR269216	2040	3	Brazil	GrsLnd	Agrclture	BASE	27.13	2344	2	3 2	0	Inf
# CR269216	2050	3	Brazil	GrsLnd	Agrclture	BASE	34.17	2344	2	3 2	0	Inf
# CR270215	2050	3	Brazil	GrsLnd	Agrclture	BASE	54.82	2389	2	3 2	0	Inf
# CR271214	2050	3	Brazil	GrsLnd	Agrclture	BASE	27.94	2434	2	3 2	0	Inf
# CR272214	2050	3	Brazil	GrsLnd	Agrclture	BASE	47.74	2477	2	3 2	0	Inf

# raster que eu gerei com os dados

r_base <- raster("/dados/projetos_andamento/TRADEhub/Linha_2/land_uses_aggregated/BASE/BASE_agriculture_2020.tif")


# abrindo baseline mapbiomas do projeto antigo

pastureland <- raster("/dados/projetos_andamento/TRADEhub/GLOBIOMbr/rawdata/land-use/pasture_1km.tif")

# ajustando projecao

pastureland_pj <- projectRaster(pastureland,r_base )

# focar em uma celula do grid!

transition_subset <- filter(transition_long_sum,ID=="CR255182")
transition_subset_pj <- st_transform(transition_subset,crs(pastureland_pj))

# cortando

past_crop <- crop(pastureland_pj,transition_subset_pj)
past_mask <- mask(past_crop,transition_subset_pj)


plot(past_mask)
plot(st_geometry(transition_subset_pj),add=T)

area_celula <- 0.02543694*(55659.39*55659.39)/10^4/1000






