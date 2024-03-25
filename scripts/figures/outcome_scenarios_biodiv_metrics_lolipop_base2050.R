# - pacotes --------------------------------------------------------------------

library(ggpubr)
library(data.table)
library(tidyverse)
library(scales)

# penalidade pra notacao
options(scipen=10000)
#-------------------------------------------------------------------------------

# ajustar esse troco!

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


eco_table <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")
# 
names(eco_table)[2] <-"ecoregion_ID" 
# 
bio3 <- left_join(bio2,eco_table)
# 
bio3$condition <- factor(bio3$condition,levels = c("improved","worsened","no changes"))

# # Calculate maximum values for each level of the name column
max_values <- bio3 %>%
  group_by(name,scenario) %>%
  summarize(max_value = max(abs(value)))

# # Merge the maximum values with the original dataframe
bio4 <-bio3 %>%
  left_join(max_values, by = c("name","scenario")) %>%
  mutate(scaled_value = value / max_value,
         scaled_value=if_else(is.infinite(scaled_value),0,scaled_value),
         scaled_value=if_else(is.nan(scaled_value),0,scaled_value)) %>%
  select(-max_value)

# Calculate the sum of scaled_values for each scenario and name
bio4[, sum_scaled := sum(scaled_value), by = .(scenario, name)]
 
# Calculate the ratio based on condition
bio4[, ratio := ifelse(condition == "improved", scaled_value / sum_scaled, scaled_value / abs(sum_scaled))]

# Calculate cumulative sum of ratio per scenario and name

bio4 <- bio4 %>%
  group_by(scenario, name) %>%
  arrange(ratio) %>%
  mutate(cumulative_ratio = cumsum(ratio))

# data frame com o limiar de cada metrica, pro baseline

trshld <- bio4%>%
  filter(scenario=="base_2050",
         cumulative_ratio<=-0.49&cumulative_ratio>=-0.51)

# determinando posicao eixo y no OLHOMETRO! e adicionando no df
 
trshld$y_pos <- c(5,9,11)

# calculando razao em funcao do base 2050

baseline2050 <- bio4%>%filter(scenario=="base_2050")
restscen <- bio4%>%filter(scenario!="base_2050")

bio4 <- bio4%>%
  group_by(name,Nome)%>%
  mutate(ratio_base2050 = value / abs(value[scenario == "base_2050"]),
         ratio_base2050=if_else(is.nan(ratio_base2050),0,ratio_base2050),
        condition=if_else(ratio_base2050<0,"worsened",if_else(ratio_base2050>0,"improved","no change")))

# separando por metrica e cenário ao invez de tudo junto

# bd

metricas <- unique(bio4$name)
scenarios <- c("fcnz","fcnzplus" )
labels <- c("Reduction of ecoregion vulnerability","Reduction of ecosystem integrity impact","Reduction of extinction debt")


plt_lst <- list()
c=1
for(m in seq_along(metricas)){
  for(s in seq_along(scenarios)){
    df <-bio4%>%
      filter(name==metricas[m],
             scenario==scenarios[s]
             )
    # arranjando orderm:
    df$condition <- factor(df$condition,levels = c("improved","worsened","no change"))

    if(length(unique(df$condition))<3){
      if(scenarios[s]=="fcnzplus"){
    
      plot <-df%>%
        #mutate(ratio_base2050=log10(ratio_base2050))%>%
        ggdotchart(x = "Nome", y = "ratio_base2050", #scaled_value
            color = "condition",                               
            palette = c("#619CFF","gray" ), # Custom color palette
           sorting = "descending",     # Sort value in descending order
           add = "segments",           # Add segments from y = 0 to dots
           add.params = list(color = "lightgray", size = 1), # Change segment color and size
           #group = "cyl",                                # Order by groups
            dot.size = 2,                                 # Large dot size
           #label = round(data$new_ID,1), labels
          #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
          ggtheme = theme_minimal(),                        # 
          ylab = "ratio restoration scenario/base 2050",xlab="Ecoregions",title=paste0(scenarios[s],": ",labels[m]))+
          geom_hline(yintercept = 0, linetype = 2, color = "black")+
          # add treshold
          #geom_vline(xintercept =trshld$y_pos[trshld$name=="bd.val"], color = "black", linetype = 2) +
          #facet_wrap(~scenario,labeller = custom_labeller,scales = "free_x")+
          coord_flip()+
        #scale_y_continuous(trans = "log10",labels = comma) +  # Set y-axis to log10 scale  
        theme(axis.text.x = element_text(size=7),
                axis.text.y = element_text(size=7),
                strip.text.y = element_text(size = 7),
                strip.text.x = element_text(size = 7),
                legend.text = element_text(size = 7),
                plot.title = element_text(size = 8),
                legend.position = "top",
                axis.title = element_text(size=7))+
        scale_y_continuous(trans = "pseudo_log")
      }else{plot <-df%>%
        #mutate(ratio_base2050=log10(ratio_base2050))%>%
        ggdotchart(x = "Nome", y = "ratio_base2050", #scaled_value
                   color = "condition",                               
                   palette = c("#619CFF","#F8766D" ), # Custom color palette
                   sorting = "descending",     # Sort value in descending order
                   add = "segments",           # Add segments from y = 0 to dots
                   add.params = list(color = "lightgray", size = 1), # Change segment color and size
                   #group = "cyl",                                # Order by groups
                   dot.size = 2,                                 # Large dot size
                   #label = round(data$new_ID,1), labels
                   #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
                   ggtheme = theme_minimal(),                        # 
                   ylab = "ratio restoration scenario/base 2050",xlab="Ecoregions",title=paste0(scenarios[s],": ",labels[m]))+
        geom_hline(yintercept = 0, linetype = 2, color = "black")+
        # add treshold
        #geom_vline(xintercept =trshld$y_pos[trshld$name=="bd.val"], color = "black", linetype = 2) +
        #facet_wrap(~scenario,labeller = custom_labeller,scales = "free_x")+
        coord_flip()+
        #scale_y_continuous(trans = "log10",labels = comma) +  # Set y-axis to log10 scale  
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=7),
              strip.text.y = element_text(size = 7),
              strip.text.x = element_text(size = 7),
              legend.text = element_text(size = 7),
              plot.title = element_text(size = 8),
              legend.position = "top",
              axis.title = element_text(size=7))+
        scale_y_continuous(trans = "pseudo_log")
      }
      plt_lst[[c]] <- plot
    }else{
      plot <-df%>%
        ggdotchart(x = "Nome", y = "ratio_base2050", #scaled_value
                   color = "condition",                               
                   palette = c("#619CFF","#F8766D","gray" ), # Custom color palette
                   sorting = "descending",     # Sort value in descending order
                   add = "segments",           # Add segments from y = 0 to dots
                   add.params = list(color = "lightgray", size = 1), # Change segment color and size
                   #group = "cyl",                                # Order by groups
                   dot.size = 2,                                 # Large dot size
                   #label = round(data$new_ID,1), labels
                   #font.label = list(color = "black", size = 7,vjust = 0.5),# Adjust label parameters
                   ggtheme = theme_minimal(),                        # 
                   ylab = "ratio restoration scenario/base 2050",xlab="Ecoregions",title=paste0(scenarios[s],": ",labels[m]))+
        geom_hline(yintercept = 0, linetype = 2, color = "black")+
        # add treshold
        #geom_vline(xintercept =trshld$y_pos[trshld$name=="bd.val"], color = "black", linetype = 2) +
        #facet_wrap(~scenario,labeller = custom_labeller,scales = "free_x")+
        coord_flip()+
        theme(axis.text.x = element_text(size=7),
              axis.text.y = element_text(size=7),
              strip.text.y = element_text(size = 7),
              strip.text.x = element_text(size = 7),
              legend.text = element_text(size = 7),
              plot.title = element_text(size = 8),
              legend.position = "top",
              axis.title = element_text(size=7))+
        scale_y_continuous(trans = "pseudo_log")
      
      plt_lst[[c]] <- plot
    
    }
  c = c+1
  }
  
}


# plt_lst[[1]]
# plt_lst[[2]]
# plt_lst[[3]]
# plt_lst[[4]]
# plt_lst[[5]]
# plt_lst[[6]]
# 
# arranjando figuras

ec_p <- ggarrange(plotlist = plt_lst[c(1,2)],common.legend = T,nrow = 2)

it_p <- ggarrange(plotlist = plt_lst[c(3,4)],common.legend = T,nrow = 2)

bd_p <- ggarrange(plotlist = plt_lst[c(5,6)],common.legend = T,nrow = 2)


ggsave(plot = ec_p,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/ec_ratio_base2050_ecoregions.png",width = 15,height = 25,units = "cm")

ggsave(plot = it_p,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/it_ratio_base2050_ecoregions.png",width = 15,height = 25,units = "cm")

ggsave(plot = bd_p,filename = "/dados/pessoal/francisco/Linha_2_TradeHub/figures/bd_ratio_base2050_ecoregions.png",width = 15,height = 25,units = "cm")




