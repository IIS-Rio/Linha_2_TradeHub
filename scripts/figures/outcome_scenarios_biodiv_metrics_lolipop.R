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

plt_list <- list()
for (i in seq_along(label)){
  fig <-bio2%>%
          filter(name==m[i])%>%
          mutate(scaled_value=value/max(value))%>%
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


# plt_list <- list()
# 
# m <- c("bd.val","ec.val","it.val")
# label <- c("Reduction of extinction debt","Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact")
# 
# plt_list <- list()
# 
# for(i in seq_along(scenarios)){
# 
# data <- bio2 %>%
#   filter( #name == "bd.val",
#          #scenario==scenarios[i]) %>%
#   mutate( condition = if_else(value >= 0, "improved", "worsened"),
#           scaled_value=value/max(value))
# 
# data$new_ID <- factor(data$new_ID, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42", "43", "44", "45", "46", "47", "48", "49", "50", "51", "52", "53", "54", "55", "56", "57", "58", "59", "60"))
# 
# plot <- data%>%
#   ggplot(aes(x = new_ID, y = scaled_value, fill = condition)) +
#   geom_bar(stat = "identity") +
#   theme_minimal() +
#   theme(
#     axis.line = element_blank(),  # Remove axis lines
#     axis.text.y = element_blank(),  # Remove axis text
#     axis.title = element_blank(), # Remove axis titles
#     legend.position = "top",
#     legend.key = element_blank()
#   ) +
#   coord_polar(theta = "x") +  # Transform to polar coordinates
#   scale_x_discrete(labels = data$new_ID) +
#   scale_fill_manual(values = c("blue", "red")) +
#   labs(title = label[p], fill = "Condition")
#   
# plt_list[[i]] <- plot
# 
# }
# 
# 
# ggarrange(plotlist = plt_list,common.legend = T)
# plt_list[1]
# 
# set.seed(123)
# data <- data.frame(
#   values = runif(540),  # Assuming 3 scenarios * 60 individual IDs * 3 names = 540 observations
#   scenarios = rep(c("scenario1", "scenario2", "scenario3"), times = 180),  # 3 scenarios
#   ID = factor(rep(1:60, each = 9)),  # 60 individual IDs
#   name = rep(c("name1", "name2", "name3"), each = 180)  # 3 names
# )
# 
# # Create a new variable combining ID and scenario
# data$ID_scenario <- paste(data$ID, data$scenarios, sep = "_")
# 
# # Plotting
# ggplot(data, aes(x = ID_scenario, y = values, fill = scenarios)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   facet_wrap(~name) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))+coord_flip()
# 
# 
# data%>%
#   filter(name=="name1")%>%
#   ggplot(aes(x = ID_scenario, y = values)) + 
#   geom_bar(aes(fill =  scenarios),stat = "identity",position = "dodge")+
#   coord_flip()
# 
# 
# # dado hipotetico
# 
# data <- bio2%>%
#   filter(name=="bd.val")%>%
#   mutate(x=paste(new_ID,scenario,sep = "_"),
#          condition=if_else(value>=0,"improved","worsened"),
#          scaled_value=value/max(value))
# 
# data$ID_scenario <- factor(paste(data$new_ID, data$scenario, sep = "_"), levels = unique(paste(data$new_ID, data$scenario, sep = "_")))
# 
# 
# x <- ggplot(data,aes(x = ID_scenario, y = value_sc)) + 
#   geom_bar(aes(fill =  condition),stat = "identity",position = "dodge")+
#   coord_flip()
# 
# x+coord_polar(theta = "x")+facet_wrap(~scenario)

# testar com nome das ecoregioes!!!

#eco names

eco_table <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

names(eco_table)[2] <-"ecoregion_ID" 

bio3 <- left_join(bio2,eco_table)

plt_list2 <- list()
for (i in seq_along(label)){
  fig <-bio3%>%
    filter(name==m[i])%>%
    mutate(scaled_value=value/max(value))%>%
    ggdotchart(x = "Nome", y = "scaled_value", 
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
  plt_list2 [[i]] <-fig 
}
bd2 <- plt_list2[[1]]
ec2 <- plt_list2 [[2]]
it2 <- plt_list3 [[3]]


ggsave(plot = bd2,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bd_Lollipop_econms.png",width = 19,height = 15,units = "cm")
ggsave(plot = ec2,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ec_Lollipop_econms.png",width = 19,height = 15,units = "cm")
ggsave(plot = it2,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/it_Lollipop_econms.png",width = 19,height = 15,units = "cm")
