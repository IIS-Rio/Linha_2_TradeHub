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

m <- c("bd.val","ec.val","it.val")
label <- c("Reduction of extinction debt","Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact")

# Extract unique IDs
unique_metrics <- unique(df_comb$name)

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_mets <- seq_len(length(unique_metrics))

# Create a mapping between old and new IDs
id_mapping <- data.frame(old_ID = unique_metrics, new_ID = new_mets)

df_comb2 <- merge(df_comb, id_mapping, by.x = "name", by.y = "old_ID", all.x = TRUE)

# Ordering the factors to graph display 
df_comb2$scenarios<-factor(df_comb2$scenarios, levels=c("baseline","fcnz","fcnzplus"))
df_comb2$name<-factor(df_comb2$name, levels=c("bd.val","ec.val","it.val"))

# creating graph
plt_list1 <- list()
for(i in seq_along(m)){
  results_nat <- df_comb2%>%
    filter(name==m[i])%>%
  ggplot(aes(x=name, y=value, fill=scenarios)) +
    scale_fill_manual(values = c("red", "darkblue", "darkgreen")) +
  geom_bar(aes(x=name, y=value), stat = "identity", position = position_dodge())+
  theme_classic()+
    labs(title = label[i],
         x = "",
         y = "",
         fill = "Scenarios") +
  theme(axis.line =element_line(color = "black"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #strip.background = element_blank(),  # Make facet label background white.
          strip.text.x = element_text(size = 12), plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), margin=margin(t = 8, r = 0, b = 0, l = 0), face="bold"),
          axis.text.x = element_text(size=11, hjust=1, colour="black"),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          legend.text = element_text(size = 11),
          plot.title = element_text(size = 12),
          legend.position = "top") +
    # facet_grid(.~scenarios) +
    coord_flip()+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
    guides(fill=guide_legend(nrow=1,byrow=TRUE,title = ""))

  plt_list1[[i]] <-  results_nat
}

combined <- ggarrange(plotlist = plt_list1,common.legend = T,nrow = 3)

combined

# saving file
ggsave(plot = combined,filename = "/dados/pessoal/thais/Figuras_TradeHub/national_biodiversity_barplots.png", width = 19, height = 30,units = "cm")


# figura como a que o Chico gerou (separando por cenarios em colunas diferentes - não ficou legal essa representacao porque na escala nacional só há uma)
plt_list2 <- list()
for(i in seq_along(m)){
  results_nat <- df_comb2%>%
    filter(name==m[i])%>%
    # criando coluna com cores
    mutate(condition=if_else(value>=0,"improved","worsened"))%>%
    #scaled_value = ifelse(value >= 0,
    #                           rescale(value, to = c(0, 1)),
    #                           rescale(value, to = c(-1, 0)))
    #scaled_value=value/max(value)) %>%
    ggplot(aes(x = as.factor(new_ID), y = value, fill = condition)) +
    geom_bar(stat = "identity", ) +
    labs(title = label[i],
         x = "",
         y = "",
         fill = "Condition") +
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 0, hjust = 1,size=7),
          axis.text.x = element_text(angle = 0, hjust = 1,size=7),
          strip.text.y = element_text(size = 7),
          strip.text.x = element_text(size = 7),
          legend.text = element_text(size = 7),
          plot.title = element_text(size = 10)) +
    facet_wrap(~scenarios, scales = "free_y",strip.position = "top")+
    coord_flip() +
    scale_fill_manual(values = c("blue","red")) +
    labs(y = "")  # Remove y-axis label here
  
  plt_list2[[i]] <-  results_nat
}


combined2 <- ggarrange(plotlist = plt_list2,common.legend = T,nrow = 3)

combined2

### carbon metrics - national scale

# opening results

df_carb <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/combined_results_raw.csv")

df_carb2<-
  df_carb%>%
group_by(sce.name=(interaction(scenario, name))) %>% # interacting info about scenario and co2 conditions
summarise(sum_value=sum(value))

# Extract unique IDs
unique_metrics_co2 <- unique(df_carb2$sce.name)
unique_metrics_co2<-unique_metrics_co2[c(4:9, 16:18)] # removing bd metrics from object

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_mets_co2 <- seq_len(length(unique_metrics_co2))

# Create a mapping between old and new IDs
id_mapping_co2 <- data.frame(old_ID = unique_metrics_co2, new_ID = new_mets_co2)

df_carb3 <- merge(df_carb2, id_mapping_co2, by.x = "sce.name", by.y = "old_ID", all.x = TRUE) 

df_carb3<-
  df_carb3[-c(1:3, 10:15),] %>%
  mutate(scenarios=rep(c("baseline","fcnz","fcnzplus"),3)) %>%
  mutate(name=c(rep("emited", 3), rep("sequestered", 3), rep("net", 3))) %>%
  mutate(sign = sign(sum_value))

c <- c("emited","sequestered","net")
label_co2 <- c("CO2 emited", "CO2 sequestered", "Net CO2")


# creating graph
plt_list_c <- list()
for(i in seq_along(c)){
  results_c_nat <- df_carb3%>%
    filter(name==c[i])%>%
    ggplot(aes(x=name, y=log(abs(sum_value))*sign, fill=scenarios)) +
    scale_fill_manual(values = c("red", "darkblue", "darkgreen")) +
    geom_bar(aes(x=name, y=log(abs(sum_value))*sign), stat = "identity", position = position_dodge())+
    ylim(c(-25, 25)) +
    theme_classic()+
    labs(title = label_co2[i],
         x = "",
         y = "",
         fill = "Scenarios") +
    theme(axis.line =element_line(color = "black"), 
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          #strip.background = element_blank(),  # Make facet label background white.
          strip.text.x = element_text(size = 12), plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
          axis.title.y = element_blank(),
          axis.title.x = element_text(size = rel(1.2), margin=margin(t = 8, r = 0, b = 0, l = 0), face="bold"),
          axis.text.x = element_text(size=11, hjust=1, colour="black"),
          axis.text.y = element_blank(),
          axis.ticks.y= element_blank(),
          legend.text = element_text(size = 11),
          plot.title = element_text(size = 12),
          legend.position = "top") +
    # facet_grid(.~scenarios) +
    coord_flip()+
    geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
    guides(fill=guide_legend(nrow=1,byrow=TRUE,title = ""))
  
  plt_list_c[[i]] <-  results_c_nat
}

combined_carb <- ggarrange(plotlist = plt_list_c,common.legend = T,nrow = 3)

combined_carb

# saving file
ggsave(plot = combined_carb,filename = "/dados/pessoal/thais/Figuras_TradeHub/national_carbon_barplots2.png", width = 24, height = 16,units = "cm")

