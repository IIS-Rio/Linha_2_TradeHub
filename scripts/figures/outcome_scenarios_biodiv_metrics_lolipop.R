# - pacotes --------------------------------------------------------------------

library(ggpubr)

#-------------------------------------------------------------------------------

# abrindo resultados

bio <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/bio_results.csv")


# Extract unique IDs
unique_ids <- unique(bio$ecoregion_ID)

# Create a new sequential ID column ranging from 1 to the number of unique IDs
new_ids <- seq_len(length(unique_ids))

# Create a mapping between old and new IDs
id_mapping <- data.frame(old_ID = unique_ids, new_ID = new_ids)

bio2 <- merge(bio, id_mapping, by.x = "ecoregion_ID", by.y = "old_ID", all.x = TRUE)

bio2 <- mutate(bio2,condition=if_else(value>0,"improved",if_else(value<0,"worsened","no changes")))

# lolipop chart
# 1 pra cada metrica

m <- c("bd.val","ec.val","it.val")
label <- c("Reduction of extinction debt","Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact")
scenarios <- c("baseline","fcnz","fcnzplus")


bio2$condition <- factor(bio2$condition,levels = c("improved","worsened","no changes"))

custom_labeller <- function(variable, value) {
  if(variable == "scenario") {
    value <- ifelse(value == "base_2050", "Baseline", 
                    ifelse(value == "fcnz", "FCNZ", "FCNZplus"))
  }
  return(value)
}

# Calculate maximum values for each level of the name column
max_values <- bio2 %>%
  group_by(name) %>%
  summarize(max_value = max(value))

# Merge the maximum values with the original dataframe
bio2 <- bio2 %>%
  left_join(max_values, by = "name") %>%
  mutate(scaled_value = value / max_value) %>%
  select(-max_value)

plt_list <- list()
for (i in seq_along(label)){
  fig <-bio2%>%
          filter(name==m[i])%>%
          #mutate(scaled_value=value/max(value))%>%
        ggdotchart(x = "new_ID", y = "scaled_value", 
           color = "condition",                               
           palette = c("#619CFF","#F8766D","gray" ), # Custom color palette
           sorting = "descending",     # Sort value in descending order
           add = "segments",           # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 1), # Change segment color and size
           #group = "cyl",                                # Order by groups
           dot.size = 2,                                 # Large dot size
           #label = round(data$new_ID,1),                        # Add mpg values as dot labels
           #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
           ggtheme = theme_minimal()                        # ggplot2 theme
,ylab = "Scaled values",xlab="Ecoregions",title=label[i])+
        geom_hline(yintercept = 0, linetype = 2, color = "black")+
        facet_wrap(~scenario,labeller = custom_labeller)+
    coord_flip()+
    theme(axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          strip.text.y = element_text(size = 7),
          strip.text.x = element_text(size = 7),
          legend.text = element_text(size = 7),
          plot.title = element_text(size = 8),
          legend.position = "top",
          axis.title = element_text(size=7)) 
  plt_list [[i]] <-fig 
}

bd <- plt_list [[1]]
# +
#   theme(axis.text.x = element_text(size=7),
#         axis.text.y = element_text(size=7),
#         strip.text.y = element_text(size = 7),
#         strip.text.x = element_text(size = 7),
#         legend.text = element_text(size = 7),
#         plot.title = element_text(size = 8),
#         legend.position = "top") 


ec <- plt_list [[2]]

it <- plt_list [[3]]


# falta ajustar o 2 pra ec!!

ggsave(plot = bd,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bd_Lollipop.png",width = 15,height = 15,units = "cm")

ggsave(plot = ec,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ec_Lollipop.png",width = 15,height = 15,units = "cm")

ggsave(plot = it,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/it_Lollipop.png",width = 15,height = 15,units = "cm")


panel <- ggarrange(plotlist = plt_list,nrow=3,common.legend = T)

ggsave(plot = panel ,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/biodiversity_Lollipop.png",width = 17,height = 22,units = "cm")

#eco names

eco_table <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

names(eco_table)[2] <-"ecoregion_ID" 

bio3 <- left_join(bio2,eco_table)

bio3$condition <- factor(bio3$condition,levels = c("improved","worsened","no changes"))



# Calculate maximum values for each level of the name column
max_values <- bio3 %>%
  group_by(name,scenario) %>%
  summarize(max_value = max(abs(value)))

# Merge the maximum values with the original dataframe
bio4 <-bio3 %>%
  left_join(max_values, by = c("name","scenario")) %>%
  mutate(scaled_value = value / max_value,
         scaled_value=if_else(is.infinite(scaled_value),0,scaled_value),
         scaled_value=if_else(is.nan(scaled_value),0,scaled_value)) %>%
  select(-max_value)


plt_list2 <- list()
for (i in seq_along(label)){
  fig <-bio4%>%
    filter(name==m[i])%>%
    #mutate(scaled_value=value/max(value))%>%
    ggdotchart(x = "Nome", y = "scaled_value", 
               color = "condition",                               
               palette = c("#619CFF","#F8766D","gray" ), # Custom color palette
               #sorting = "descending",     # Sort value in descending order
               add = "segments",           # Add segments from y = 0 to dots
               add.params = list(color = "lightgray", size = 1), # Change segment color and size
               #group = "cyl",                                # Order by groups
               dot.size = 2,                                 # Large dot size
               #label = round(data$new_ID,1),                        # Add mpg values as dot labels
               #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
               ggtheme = theme_minimal()                        # ggplot2 theme
               ,ylab = "Scaled values",xlab="Ecoregions",title=label[i])+
    geom_hline(yintercept = 0, linetype = 2, color = "black")+
    facet_wrap(~scenario,labeller = custom_labeller,scales = "free_x")+
    coord_flip()+
    theme(axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          strip.text.y = element_text(size = 7),
          strip.text.x = element_text(size = 7),
          legend.text = element_text(size = 7),
          plot.title = element_text(size = 8),
          legend.position = "top",
          axis.title = element_text(size=7)) 
  plt_list2 [[i]] <-fig 
}
bd2 <- plt_list2[[1]]
ec2 <- plt_list2 [[2]]
it2 <- plt_list2 [[3]]


ggsave(plot = bd2,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bd_Lollipop_econms.png",width = 19,height = 15,units = "cm")
ggsave(plot = ec2,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ec_Lollipop_econms.png",width = 19,height = 15,units = "cm")
ggsave(plot = it2,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/it_Lollipop_econms.png",width = 19,height = 15,units = "cm")


# plotando impacto acumulado

# Calculate the sum of scaled_values for each scenario and name
bio4[, sum_scaled := sum(scaled_value), by = .(scenario, name)]

# Calculate the ratio based on condition
bio4[, ratio := ifelse(condition == "improved", scaled_value / sum_scaled, scaled_value / abs(sum_scaled))]


# Print the updated data.table
print(bio4)

# Calculate cumulative sum of ratio
# Calculate cumulative sum of ratio per scenario and name
bio4 <- bio4 %>%
  group_by(scenario, name) %>%
  arrange(ratio) %>%
  mutate(cumulative_ratio = cumsum(ratio))

# olhando na tabela qual ecoregiao esta no limiar!
#bd = Madeira-Tapajós!
# ec ...

# o plot nao deu certo, mas da pra colocar manualmente nos plots ja gerados, so
# as variaveis que acumulam 50% do impacto!

plt_list3 <- list()
for (i in seq_along(label)){
  fig <-bio4%>%
    filter(name==m[i])%>%
    #mutate(scaled_value=value/max(value))%>%
    ggdotchart(x = "Nome", y = "ratio", 
               color = "condition",                               
               palette = c("#619CFF","#F8766D","gray" ), # Custom color palette
               #sorting = "descending",     # Sort value in descending order
               add = "segments",           # Add segments from y = 0 to dots
               add.params = list(color = "lightgray", size = 1), # Change segment color and size
               #group = "cyl",                                # Order by groups
               dot.size = 2,                                 # Large dot size
               #label = round(data$new_ID,1),                        # Add mpg values as dot labels
               #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
               ggtheme = theme_minimal()                        # ggplot2 theme
               ,ylab = "Scaled values",xlab="Ecoregions",title=label[i])+
    geom_hline(yintercept = 0, linetype = 2, color = "black")+
    #geom_line(aes(x = reorder(Nome, cumulative_ratio), y = cumulative_ratio), color = "black", linetype = 2) +
    facet_wrap(~scenario,labeller = custom_labeller,scales = "free_x")+
    coord_flip()+
    theme(axis.text.x = element_text(size=7),
          axis.text.y = element_text(size=7),
          strip.text.y = element_text(size = 7),
          strip.text.x = element_text(size = 7),
          legend.text = element_text(size = 7),
          plot.title = element_text(size = 8),
          legend.position = "top",
          axis.title = element_text(size=7))
  plt_list3 [[i]] <-fig 
}
bd3 <- plt_list3[[1]]+
  geom_vline(xintercept = 11, linetype = "dotted", color = "red") 
ec3 <- plt_list3 [[2]]+
  geom_vline(xintercept = 11, linetype = "dotted", color = "red")
it3 <- plt_list3 [[3]]
