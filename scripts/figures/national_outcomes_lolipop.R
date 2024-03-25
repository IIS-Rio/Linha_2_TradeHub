# título: preparaçao de barplots - national results - tradehub linha 2
#' data: 08/02/2023
#' ---

# prepare r --------------------------------------------------- -----

options(scipen = 999)

# pacotes -----------------------------------------------------------
library(scales)
library(ggpubr)
library(RColorBrewer)
library(sf)
library(geobr)
library(ggthemes)
library(tidyr)
library(data.table)
library(ggrepel)
library(raster)
library(sf)
library(tibble)
library(ggspatial)
library(sp)
library(geobr)
library(terra)
library(dplyr)
library(spData)
library(tidyverse)
library(qs)
library(tmap)
library(tmaptools)
library(rnaturalearth)
library(rmapshaper)
library(rgdal)
library(maptools)
#----------------------------------------------------------------------
# opening results

### biodiversity metrics - national scale

df_comb <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_national_results_raw.csv")

# m <- c("bd.val","ec.val","it.val")
# label <- c("Reduction of extinction debt","Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact")

# Ordering the factors to graph display 
df_comb$scenarios<-factor(df_comb$scenarios, levels=c("baseline","fcnz","fcnzplus"))

df_comb$name<-factor(df_comb$name, levels=c("bd.val","ec.val","it.val"))


# creating graph

# Calculate maximum values for each level of the name column
max_values <- df_comb %>%
  group_by(name) %>%
  summarize(max_value = max(value))

# Merge the maximum values with the original dataframe
df_comb <- df_comb %>%
  left_join(max_values, by = "name") %>%
  mutate(scaled_variables = value / max_value) %>%
  select(-max_value)


bio_metrics <- df_comb%>%
  mutate(condition=if_else(value>0,"improved",if_else(value<0,"worsened","no changes")))%>%
  ggdotchart(x = "scenarios", y = "scaled_variables", 
           color = "condition",                               
           palette = c("#619CFF","#F8766D" ), # Custom color palette
           sorting = "descending",     # Sort value in descending order
           add = "segments",           # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 1), # Change segment color and size
           #group = "cyl",                                # Order by groups
           dot.size = 2,                                 # Large dot size
           #label = round(data$new_ID,1),                        # Add mpg values as dot labels
           #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
           ggtheme = theme_minimal()                        # ggplot2 theme
           ,ylab = "Scaled values",xlab="",title="")+
  geom_hline(yintercept = 0, linetype = 2, color = "black")+
  facet_wrap(~name,labeller = custom_labeller,)+
  coord_flip()+
  theme(axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        strip.text.y = element_text(size = 7),
        strip.text.x = element_text(size = 7),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 8),
        legend.position = "top",
        axis.title = element_text(size=7)) 


# saving file
ggsave(plot = bio_metrics,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/biodiversity_Lollipop_National.png", width = 15, height = 8,units = "cm",scale = 1)

### carbon metrics - national scale

# opening results

df_carb <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_raw.csv")

df_carb2<-
  df_carb%>%
  filter(!name %in% c("bd.val","it.val","ec.val"))%>%
group_by(scenario,name,variable) %>% # interacting info about scenario and co2 conditions
summarise(sum_value=sum(value))%>%
  mutate(sum_value=sum_value)

custom_labeller <- function(variable, value) {
  if(variable == "name") {
    value <- ifelse(value == "CO2_emited", "emission", 
                    ifelse(value == "CO2_sequestered", "sequestration", "net"))
  }
  return(value)
}


df_carb2$scenario <- factor(df_carb2$scenario,levels =(c("baseline_2050","fcnz_2050","fcnzplus_2050")))

carbon <- df_carb2%>%
  mutate(sum_value=sum_value/10^6)%>%
  ggdotchart(x = "scenario", y = "sum_value", 
             #color = "condition",                               
             #palette = c("#619CFF","#F8766D" ), # Custom color palette
             #sorting = "descending",     # Sort value in descending order
             add = "segments",           # Add segments from y = 0 to dots
             add.params = list(color = "lightgray", size = 1), # Change segment color and size
             #group = "cyl",                                # Order by groups
             dot.size = 2,                                 # Large dot size
             #label = round(data$new_ID,1),                        # Add mpg values as dot labels
             #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
             ggtheme = theme_minimal()                        # ggplot2 theme
             ,ylab = "Megatons CO2",xlab="",title="")+
  scale_x_discrete(labels=rev(c("baseline","fcnz","fcnzplus")))+
  geom_hline(yintercept = 0, linetype = 2, color = "black")+
  facet_wrap(~name,labeller = custom_labeller,)+
  coord_flip()+
  theme(axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        strip.text.y = element_text(size = 7),
        strip.text.x = element_text(size = 7),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 8),
        legend.position = "top",
        axis.title = element_text(size=7)) 


# saving file
ggsave(plot = carbon,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/carbon_Lollipop_National.png", width = 15, height = 8,units = "cm")

