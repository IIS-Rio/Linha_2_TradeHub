library(tidyverse)
#install.packages("devtools")
#devtools::install_github("ricardo-bion/ggradar")
library(ggradar)
library(data.table)
#-------------------------------------------------------------------------------
# fish_encounters <- fish_encounters
# fish_encounters %>%
#   pivot_wider(names_from = station, values_from = seen)



# ecoregions <- st_read("/dados/projetos_andamento/TRADEhub/Linha_2/results_spatial/ecoregions_bio_metricsv02.shp")%>%st_drop_geometry(ecoregions)
# names(ecoregions) <- c("ecoID","mtc_nm","rt20fcnz","vlbse50","rt20bse","vlbse20","rt20fcnzplus","vlfcnz50","rt50fcnz","vlfcnzplus50","rt50fcnzplus","eco_nm" ) 

df_comb <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_rescaled.csv")

# fcnzplus_wide <-ecoregions%>%
#   mutate(rt50fcnzplus=(rt50fcnzplus - min(rt50fcnzplus)) / (max(rt50fcnzplus) - min(rt50fcnzplus)))

# por cenario

base <- df_comb %>%
  filter(scenario=="baseline")

summary(base$value)

# colunas com as metricas, linhas com ecorregioes. daria pra incluir bioma e fazer ecorregioes por bioma. Mas acho q vai ficar poluido


base_wide <-pivot_wider(base,names_from = mtc_nm)

# fcnzplus_wide <- fcnzplus_wide%>%pivot_wider(names_from = ecoID,values_from = rt50fcnzplus)
# 
# fcnzplus_bd <- filter(fcnzplus_wide,mtc_nm=="bd.val")

# estrutrua; uma coluna pra cada ecorregiao. 
# uma linha pra cada cenario

# elimina na do it pra uma regiao (102 nao calcula it)

base_wide <- base_wide[complete.cases(base_wide),] 

# so da pra fazer com valor positivo!
# acho q so faria sentido o radar se for pro brasil todo (ou por bioma?)


base_wide_agg <- base_wide %>%
  group_by(ratio, scenario) %>%
  summarise(across(c(4, 5, 6), sum))%>%
  mutate(it.val=it.val*-1/100,
         bd.val=bd.val*-1/100,
         ec.val=ec.val*-1/100)


# isso valeria pra mostrar os outcomes por cenario total!

base_wide_agg[,c(2:5)]%>%
    ggradar(background.circle.colour = "white",
          gridline.min.linetype = 1,
          gridline.mid.linetype = 1,
          gridline.max.linetype = 1)


df_agg <- df_comb%>%
  group_by(mtc_nm,ratio,scenario)%>%
  summarise(across(c(4), sum))%>%
  mutate(value_sc=value/100)


# plotando cenarios 2050

df_agg%>%
  filter(ratio=="compared2_2050")%>%
  group_by(scenario) %>%
  select(-c(2, 4)) %>%
  pivot_wider(names_from = mtc_nm,values_from = value_sc)%>%
  #select(c(2,3,4,5))%>%
  ggradar(background.circle.colour = "white",
          gridline.min.linetype = 1,
          gridline.mid.linetype = 1,
          gridline.max.linetype = 1,
          grid.min = 0,
          grid.mid = 0.2,
          grid.max = 0.5)



#?ggradar



for(i in seq_along(base_wide$ecoID)){
  p <- base_wide[i,c(1,6,7,8)]%>%
    mutate(it.val=it.val*-1,
           bd.val=bd.val*-1,
           ec.val=ec.val*-1)%>%
    ggradar(background.circle.colour = "white",
          gridline.min.linetype = 1,
          gridline.mid.linetype = 1,
          gridline.max.linetype = 1)+
    theme(legend.position = "none")
  plot_list[[i]] <- p
    }



ggarrange(plotlist = plot_list )
plot_list[[1]]
plot_list[[59]]
