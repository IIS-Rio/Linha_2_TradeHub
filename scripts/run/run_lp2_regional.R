# rodando plangea

# pacotes ----------------------------------------------------------------------

devtools::load_all("/dados/pessoal/francisco/plangea-pkg/")

library(jsonlite)

#-------------------------------------------------------------------------------

# caminho pra salvar as analises por regiao

p <- "/dados/projetos_andamento/TRADEhub/Linha_2/results"
  
# uma pasta por regiao

regioes <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")

f <- function(x)dir.create(file.path(p,x))
lapply(regioes$ID,f)


# configurando o JSON pra rodar pra varias regioes
# cfg <- aux_read_cfg(file.path("/dados/pessoal/francisco/TradeHub/json/globiom_iiasa.json"))
# updates cfg with plangea_path

# i=1
# j=1

# piloto com 1 scenario so

scen <- list.files("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/2050",pattern = "base")

reg <- regioes$ID

base <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use/baseline_2020"
source <- "/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/land_use_regional"
dest <- "/dados/projetos_andamento/TRADEhub/Linha_2/results/"
# cfg$io$plangea_path = plangea_path

# criando objeto com indices do JSON

# oq precisa ta no base, oq precisa ta no relative e oq precisa ta no future??


cfg = jsonlite::fromJSON("/dados/pessoal/francisco/Linha_2_TradeHub/scripts/JSON/L2_ecoregions.json")

#excluir <-frequencia$Freq[frequencia$Freq<862]
excluir <- c("13","257","266","68","208","")
reg <- reg[!reg %in% excluir]
# rodar faltantes
reg <- reg[38:66]
# tem q adiconar o lugar pra salvar

for(i in 1:seq_along(reg)){
  for(j in 1:length(scen)){
    
    # definir aqui. testar ser results
    cfg$io$base_path <- paste0(dest,reg[i],"/",scen[j],"/") #fazer so pra 1 regiao pra ver
    
    # aqui seria o caminho uso presente
    cfg$io$lu_relative_path <- paste0("land_use_regional/ecoregion_",reg[i],"/")
    
    
    # aqui uso futuro
    cfg$io$future_lu_relative_path <- paste0("land_use/2050/",scen[j],"/")
    
    plangea(cfg = cfg)    
    
  }
  
}

# parou no 257 nao sei pq: erro:

#Error in harmonize_pa(cfg, file_log = harmonize_log, verbose = verbose,  : 
# Computed PA have NA values in the master_index
# Calls: .rs.sourceWithProgress ... eval -> eval -> plangea -> harmonize -> harmonize_pa

# agora parou no 326

# deu errado a mascara ai
# ecoregions <- rast("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/ecoregions_wwf_plusCerrado.tif")
# 
# legenda <- read.csv("/dados/projetos_andamento/TRADEhub/Linha_2/rawdata/subregions/dicionario_Cerrado_wwf.csv")
# 
# eco257 <- ecoregions==677
# plot(eco257)
# unique(values(eco257))
# frequencia <- as.data.frame(table(values(ecoregions)))
# 
# # tem q ver os q tem freq. mto baixa!
# # usar isso pra filtrar as regioes
# 
