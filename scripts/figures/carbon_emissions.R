# pensar num stacked_barplot com uma barra por cenario, stackeados por ecorregiao?
# nesse caso, as ecorregioes principais precisariam estar destacadas. tipo as responsaveis por x% das emissoes totais! isso eh um norte pra qual direcionar tb nos mapas todos.


# make_cumulative_Table tem q ser usado pra rodar o cenario atual!!!
# tem q fazer a tabela cumulativa menos a agregada.

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/cb"

cb_seq <- read.csv(file.path(p,"CO2_sequestered_per_ecoregion.csv"))
cb_em <- read.csv(file.path(p,"CO2_emited_per_ecoregion.csv"))
# ecorregioes pra excluir:

excluir <- c(13,257,266,68,208,677,326) # IDs para excluir


cb_seq <- filter(cb_seq,!ecoregion_ID %in% excluir)%>%
  mutate(CO2_sequestered=CO2_sequestered*-1)

cb_em <- filter(cb_em,!ecoregion_ID %in% excluir)%>%
  mutate(CO2_emited=CO2_emited*-1)

# juntar num df so (vou fazer sem diferenciar por ecoregiao)

cb <- left_join(cb_em,cb_seq)%>%
  mutate(net=CO2_emited+CO2_sequestered )%>%
  pivot_longer(cols = c(2,4,5))%>%
  mutate(value_mega=value/10^6)


cb_agg <- left_join(cb_em,cb_seq)%>%
  group_by(scen)%>%
  summarise(CO2_emited=sum(CO2_emited),
            CO2_sequestered=sum(CO2_sequestered)) %>%
  mutate(net=CO2_emited+CO2_sequestered )%>%
  pivot_longer(cols = c(2,3,4))%>%
  mutate(value_mega=value/10^6)

cb$name <- factor(cb$name,levels=c("CO2_emited","CO2_sequestered","net"))
cb_agg$name <- factor(cb_agg$name,levels=c("CO2_emited","CO2_sequestered","net"))


label_positions <- cb %>%
  mutate(name = case_when(
    name == "CO2_emited" ~ "emited",
    name == "CO2_sequestered" ~ "sequestered",
    TRUE ~ "net"
  )) %>%
  group_by(scen, name) %>%
  summarize(label_y = max(value_mega))

label_positions$name <- factor(label_positions$name,levels = c("emited","sequestered","net"))


# precisa ajustar essa merda!!invertter os sinais!! e acrescentar ecorregioes com mesma paleta! mas eh mais facil fazer sem ser stackeado!

# plot sem agregar

cb %>%
  ggplot(aes(x = scen, y = value_mega, fill = ecoregion_ID,group = name)) +
  geom_bar(stat = "identity", position = "dodge") +
  # geom_text(data = label_positions, aes(x = scen, y = label_y, label = name),
  #           position = position_dodge2(width = 0.9), vjust = -0.5, size = 3, inherit.aes = FALSE) +
  labs(title = "CO2 Emission, Sequestration, and Net Balance",
       x = "Scenario",
       y = "CO2 balance (Mega tons)") +
  scale_fill_viridis_c() +  # You can choose a different color scale
  scale_y_continuous(labels = scales::comma_format())+
  guides(fill = FALSE)+
  facet_wrap("name")+
  theme_classic()

# plot agregado

cbalance <- cb_agg %>%
  ggplot(aes(x = scen, y = value_mega, group = name)) +
  geom_bar(stat = "identity", position = "dodge",fill ="lightblue") +
  # geom_text(data = label_positions, aes(x = scen, y = label_y, label = name),
  #           position = position_dodge2(width = 0.9), vjust = -0.5, size = 3, inherit.aes = FALSE) +
  labs(title = "CO2 Emission, Sequestration, and Net Balance",
       x = "Scenario",
       y = "CO2 balance (M tons)") +
  scale_y_continuous(labels = scales::comma_format())+
  guides(fill = FALSE)+
  facet_wrap("name")+
  theme_classic()

ggsave(plot = cbalance,filename =  "/dados/pessoal/francisco/Linha_2_TradeHub/figures/carbon_balance.png",width = 30,height = 18,units = "cm")
