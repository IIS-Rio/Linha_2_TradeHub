# rodando plangea

# pacotes ----------------------------------------------------------------------

devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")
library(jsonlite)
library(doSNOW)
#-------------------------------------------------------------------------------

# rodando de novo depois de corrigir baseline 2020

# caminho pra salvar as analises por regiao

#p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_up2/"
  
scen=c("base","fcnz","fcplusnz")

base <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020"

source <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050"

dest <- "/dados/projetos_andamento/TRADEhub/Linha_2/results_national/"

cfg = jsonlite::fromJSON("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/JSON/L2_ecoregions.json")



tasks <- expand.grid(scen)
names(tasks) <- c("scen")

# isso aqui eh so pra rodar alguns q faltaram
#tasks <- tasks[c(-5,-2),]

plangea_run <- function(reg, scen, dest, cfg) {
  
      # pasta destino
      cfg$io$base_path <- paste0(dest,scen,"/") #fazer so pra 1 regiao pra ver
  
      # aqui seria o caminho uso presente
      cfg$io$lu_relative_path <-"land_use/baseline_2020/"
  
  
      # aqui uso futuro
      cfg$io$future_lu_relative_path <- paste0("land_use/2050/",scen,"/")
  
      plangea(cfg = cfg)

}


# Setting up the progress bar
iterations = nrow(tasks)

# Progress bar object
pb_l = progress::progress_bar$new(
  format = "Loading scenario [:bar] :percent in :elapsed",
  total = iterations, clear = FALSE, width = 70)

progress_number = 1:iterations
progress = function(n) {pb_l$tick(tokens = list(sp = progress_number[n]))}
opts = list(progress = progress)

# run in parallel

num_clusters <- 10

cl <- makeCluster(num_clusters)
doSNOW::registerDoSNOW(cl)
# Run tasks in parallel
foreach(i = 1:nrow(tasks), .combine = 'c',.packages = c('devtools', 'progress'),
        .options.snow = opts,
        .errorhandling = "remove") %dopar% {
          
  suppressWarnings(suppressMessages(devtools::load_all("/dados/pessoal/francisco/plangea-pkg/", quiet = TRUE)))
  plangea_run(reg = tasks$reg[i], scen = tasks$scen[i], dest, cfg)
  
}

# Stop the parallel cluster
stopCluster(cl)
