#fixing metric values

# fcnzplus
#------------------
# ec
#------------------
# 557 (36) - change to 0
# 291 - change to 0
# 23 - change to 0
# 2 -  deveria ter melhorado! nao entendo pq piorou, mas restaurou quase 10%
# 292 - change to 0
# 640 -  change to 0
# 352 - change to 0
# 469 - change to 0
# 420 - change to 0
# 690 - change to 0
# 598
# fcnz

list_fcnz_ec <- c(102,557,291,23,2,292,640,352,469,420,690,598)

fcnzplus <- fread("/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_bio_results_ecoregions_fcnzplus.csv")

fcnzplus$value[fcnzplus$name=="ec.val"&fcnzplus$ecoregion %in% list_fcnz_ec] <- 0

fcnzplus$value[fcnzplus$name=="bd.val"&fcnzplus$ecoregion==557] <- 0

fcnzplus$value[fcnzplus$name=="bd.val"&fcnzplus$ecoregion==640] <- 0

write.csv(fcnzplus,"/dados/projetos_andamento/TRADEhub/Linha_2/result_tables/plangea_bio_results_ecoregions_fcnzplus.csv",row.names = F)
