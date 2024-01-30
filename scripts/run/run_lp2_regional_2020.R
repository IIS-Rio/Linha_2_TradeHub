# rodando plangea

# pacotes ----------------------------------------------------------------------

devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")

library(jsonlite)

#-------------------------------------------------------------------------------

# caminho pra salvar as analises por regiao

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_up/"
  
# uma pasta por regiao

regioes <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

# f <- function(x)dir.create(file.path(p,x))
# lapply(regioes$ID,f)

scen <-"base"

#base <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020"

source <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use_regional"
dest <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_up/baseline_2020/"

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/JSON/L2_ecoregions_current.json")

reg <- regioes$ID

# definir regioes para serem excluidas com base na area mto pequena

excluir <- c(13,257,266,68,208,677,326) 

reg <- reg[!reg %in% excluir]

# regioes que nao estao rodando
reg <- c(48,766)
# nao rodar de novo as que ja foram

# definindo tasks

tasks <- expand.grid(reg,scen)
names(tasks) <- c("reg","scen")

plangea_run <- function(reg,scen, dest, cfg){
  
      # definir aqui. testar ser results
      cfg$io$base_path <- paste0(dest,reg,"/",scen,"/") 
  
      # aqui seria o caminho uso presente
      cfg$io$lu_relative_path <- paste0("land_use_regional/ecoregion_",reg,"/")
  
      plangea(cfg = cfg)

}



# Setting up the progress bar --------------------------------------------------

#iterations = nrow(tasks)

# Progress bar object
# pb_l = progress::progress_bar$new(
#   format = "Loading scenario [:bar] :percent in :elapsed",
#   total = iterations, clear = FALSE, width = 70)
# 
# progress_number = 1:iterations
# progress = function(n) {pb_l$tick(tokens = list(sp = progress_number[n]))}
# opts = list(progress = progress)


# run in parallel --------------------------------------------------------------

num_clusters <- 10

cl <- makeCluster(num_clusters)
doSNOW::registerDoSNOW(cl)
# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c',.packages = c('devtools', 'progress')
        #,.options.snow = opts
        ) %dopar% {
          suppressWarnings(suppressMessages(devtools::load_all("/dados/pessoal/francisco/plangea-pkg/", quiet = TRUE)))
          plangea_run(reg = tasks$reg[i], scen = tasks$scen[i] ,dest, cfg)
        }

# Stop the parallel cluster
stopCluster(cl)

