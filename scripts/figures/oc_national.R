library(ggpubr)
library(tidyverse)
library(data.table)

df_oc <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/oc_reais_ecoregions.csv")

ic <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/ic_reais_ecoregions.csv")


# vale a pena pensar em areas que foram ganhas?? conversao gerando ganho? da pra pensar numa figura

df_oc2<-
  df_oc%>%
  group_by(scen) %>% 
  summarise(sum_value=sum(oc_restoration))


df_ic<-
  ic%>%
  group_by(scen) %>% # interacting info about scenario and co2 conditions
  summarise(sum_value=sum(ric_convserved))



# df_oc_l <- pivot_longer(df_oc2,2)

df_oc2$scen <- factor(df_oc2$scen,levels =(c("baseline_2050","fcnz_2050","fcnzplus_2050")))


custom_labeller <- function(variable, value) {
  if(variable == "name") {
    value <- ifelse(value == "bd.val", "Reduction of\n extinction debt", 
                    ifelse(value == "ec.val", "Reduction of\n ecoregion vulnerability", 
                           "Reduction of\n ecosystem\n integrity impact"))
  }
  return(value)
}


oc <- df_oc2%>%
  filter(scen!="baseline_2050")%>%
  mutate(billion=sum_value/10^9)%>%
  ggdotchart(x = "scen", y = "billion", 
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
             ,ylab = "Opportunity cost of forgone production (billion BRL)",xlab="",title="")+
  scale_x_discrete(labels=(c("fcnz","fcnzplus")))+
#  geom_hline(yintercept = 0, linetype = 2, color = "black")+
  coord_flip()+
  theme(axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        strip.text.y = element_text(size = 7),
        strip.text.x = element_text(size = 7),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 8),
        legend.position = "top",
        axis.title = element_text(size=7))
# +
#   scale_y_continuous(trans = "log10",breaks = c(0,100,179,462))


# saving file
ggsave(plot = oc,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/oc_National.png", width = 8, height = 8,units = "cm")


df_oc3 <- df_oc2%>%
  filter(scen!="baseline_2050")%>%
  mutate(billion_reais=sum_value/10^9)

write.csv(df_oc3,"/dados/pessoal/francisco/Linha_2_TradeHub/tables/oc_rest_national.csv",row.names = F)


# ric

ric <- df_ic%>%
  filter(scen!="baseline_2050")%>%
  mutate(billion=sum_value/10^9)%>%
  ggdotchart(x = "scen", y = "billion", 
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
             ,ylab = "Restoration implementation costs (billion BRL)",xlab="",title="")+
  scale_x_discrete(labels=(c("fcnz","fcnzplus")))+
  #  geom_hline(yintercept = 0, linetype = 2, color = "black")+
  coord_flip()+
  theme(axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        strip.text.y = element_text(size = 7),
        strip.text.x = element_text(size = 7),
        legend.text = element_text(size = 7),
        plot.title = element_text(size = 8),
        legend.position = "top",
        axis.title = element_text(size=7))


ggsave(plot = ric,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ric_National.png", width = 8, height = 8,units = "cm")


df_ic_2 <- df_ic%>%
  filter(scen!="baseline_2050")%>%
  mutate(billion_reais=sum_value/10^9)

write.csv(df_ic_2,"/dados/pessoal/francisco/Linha_2_TradeHub/tables/rc_rest_national.csv",row.names = F)


# combinando as duas


pannel <- ggarrange(oc,ric)

ggsave(plot = pannel,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ricANDocNational.png", width = 8, height = 8,units = "cm")
