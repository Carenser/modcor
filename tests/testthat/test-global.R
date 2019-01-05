# library(furrr)
# library(tidyverse)
# library(lubridate)
# library(fuzzyjoin)
# library(plotly)
#
# options(warning.length = 5000L, tibble.print_max = Inf)
#
# map(.x = list.files(path = paste0(getwd(),'/R'), pattern = '[^main.R]',full.names = TRUE),.f = function(x){source(file = x, prompt.echo = FALSE, verbose = FALSE, print.eval = FALSE, echo = FALSE)})
#
# # Chargement des données  -------------------------------------------------
#
# time.Start <- Sys.time()
# cat('Chargement des données')
#
# tbl_cdc = read_cdc(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '(MA|NEBEF).*_(20180127|20180203|20180210|20180217|20180224).*.csv', full.names = TRUE), dossiers = NULL)
#
# tbl_eff = read_eff(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '(OA_GRD|PEC_GRD)', full.names = TRUE), dossiers = NULL)
#
# tbl_sites = read_perim(dossiers = paste0(getwd(),'/data-raw/201802'))
#
# tbl_entt = read_entt(dossiers =  paste0(getwd(),'/data-raw/201802'))
#
# tbl_homol = read_homol(dossiers =  paste0(getwd(),'/data-raw/201802'))
#
# tbl_indhist = read_indispo(dossiers  =  paste0(getwd(),'/data-raw/', c('201712','201801','201802')))
#
# tbl_prev = read_prev(dossiers =  paste0(getwd(),'/data-raw/201802'))
#
# decompt <- difftime(Sys.time(), time.Start, units = "auto")
# print(decompt)
#
# # Calcul des chroniques d'effacement ou report ----------------------------
#
# time.Start <- Sys.time()
# cat('Calcul des chroniques de référence en sequentiel')
#
# tbl_cdcRef = create_ref(
#   tbl_cdc = dplyr::filter(tbl_cdc, CODE_ENTITE %in% c('EDETEQI003','EDETEQI002','GDF10TC1','EDEPENE003'))
#   , tbl_sites = tbl_sites
#   , tbl_eff = dplyr::filter(tbl_eff, CODE_ENTITE %in% c('EDETEQI003','EDETEQI002','GDF10TC1','EDEPENE003'))
#   , tbl_entt = tbl_entt
#   , tbl_homol = tbl_homol
#   , tbl_indhist = tbl_indhist
#   , tbl_prev = tbl_prev
#   , lgl_details = TRUE
# )
#
# decompt <- difftime(Sys.time(), time.Start, units = "auto")
# print(decompt)
# #
# # time.Start <- Sys.time()
# # cat('Calcul des chroniques de référence en parallèle')
# #
# # tbl_cdcRef = create_ref(
# #   tbl_cdc = tbl_cdc
# #   , tbl_sites = tbl_sites
# #   , tbl_eff = tbl_eff
# #   , tbl_entt = tbl_entt
# #   , tbl_homol = tbl_homol
# #   , tbl_indhist = tbl_indhist
# #   , tbl_prev = tbl_prev
# #   , lgl_parallel = TRUE
# # )
# #
# # decompt <- difftime(Sys.time(), time.Start, units = "auto")
# # print(decompt)
#
#
# Export au format prévu dans les règles SI -------------------------------
# 
# time.Start <- Sys.time()
# cat("Ecriture des fichiers csv de chroniques d'effacement ou report")
# write_modcor(tbl_chron = tbl_cdcRef, char_path = paste0(getwd(),'/data-raw'), lgl_all = TRUE)
# decompt <- difftime(Sys.time(), time.Start, units = "auto")
# print(decompt)
# 
# q = tbl_cdcRef %>%
#   ggplot(mapping = aes(x = HEURE, y = CHRONIQUE, col = CODE_SITE)) +
#   geom_line()
# ggplotly(q, tooltip = c('CHRONIQUE','HEURE'))
# 
# p = tbl_cdcRef %>%
#   gather(key = 'TYPE', value = 'PUISSANCE', - MECANISME, -CODE_ENTITE, -CODE_SITE, -CODE_EIC_GRD, - DATE, -HEURE, -PAS) %>%
#   ggplot(mapping = aes(x = HEURE, y = PUISSANCE, col = CODE_SITE, linetype = TYPE)) +
#   geom_line()
# ggplotly(p, tooltip = c('PUISSANCE','HEURE'))
