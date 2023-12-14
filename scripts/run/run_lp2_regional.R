# rodando plangea

# pacotes ----------------------------------------------------------------------

devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")

library(jsonlite)

#-------------------------------------------------------------------------------

# caminho pra salvar as analises por regiao

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results"
  
# uma pasta por regiao

regioes <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

# f <- function(x)dir.create(file.path(p,x))
# lapply(regioes$ID,f)


# configurando o JSON pra rodar pra varias regioes
# cfg <- aux_read_cfg(file.path("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa.json"))
# updates cfg with plangea_path

# i=1
# j=1

# piloto com 1 scenario so

scen <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050",pattern = "fcnz")

#reg <- regioes$ID

base <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020"
source <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use_regional"
dest <- "/dados/projetos_andamento/TRADEhub/Linha_2/results/"

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/JSON/L2_ecoregions.json")

reg <- regioes$ID

# definir regioes para serem excluidas com base na area mto pequena
# parou na 208

excluir <- c(13,257,266,68,208,677,326) 

reg <- reg[!reg %in% excluir]

# nao rodar de novo as que ja foram

reg2 <- reg[which(reg==291):60]

# definindo tasks

tasks <- expand.grid(reg,scen)
names(tasks) <- c("reg","scen")

plangea_run <- function(reg,scen, dest, cfg){
  
      # definir aqui. testar ser results
      cfg$io$base_path <- paste0(dest,reg,"/",scen,"/") #fazer so pra 1 regiao pra ver
  
      # aqui seria o caminho uso presente
      cfg$io$lu_relative_path <- paste0("land_use_regional/ecoregion_",reg,"/")
  
  
      # aqui uso futuro
      cfg$io$future_lu_relative_path <- paste0("land_use/2050/",scen,"/")
  
      plangea(cfg = cfg)

}


# run in parallel

num_clusters <- 10

cl <- makeCluster(num_clusters)


# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c') %dopar% {
  plangea_run(reg = tasks$reg[i], scen = tasks$scen[i] ,dest, cfg)
}

# Stop the parallel cluster
stopCluster(cl)
