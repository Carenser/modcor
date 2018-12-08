library(tidyverse)
library(lubridate)
library(fuzzyjoin)

map(.x = list.files(path = paste0(getwd(),'/R'), pattern = '[^Main.R]',full.names = TRUE),.f = source)

test_eff = LoadEffacements(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = 'OA_GRD|PEC_GRD', full.names = TRUE), dossiers = NULL)

test_perim = LoadPerimetre(dossiers = paste0(getwd(),'/data-raw/201802'))

test_entt = LoadListeEntt(dossiers =  paste0(getwd(),'/data-raw/201802'))

test_homol = LoadSitesHomol(dossiers =  paste0(getwd(),'/data-raw/201802'))

test_cdc_prev = LoadPrev(dossiers =  paste0(getwd(),'/data-raw/201802'))

#test_cdc_ma = LoadCdC(fichiers = list.files(path = paste0(getwd(),'/data-raw/201801'), pattern = 'CRMA_0001', full.names = TRUE))

test_cdc_nebef = LoadCdC(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/201802'), pattern = 'NEBEF_CRS', full.names = TRUE))

#test_cdc = LoadCdC(fichiers = list.files(path = paste0(getwd(),'/data-raw/201801'), pattern = 'NEBEF_CRS|CRMA_0001', full.names = TRUE))

test_semi_perim = fuzzy_semi_join(
  x = distinct(dplyr::filter(test_perim,TYPE_CONTRAT == 'CARD'), DEBUT, FIN, CODE_ENTITE)
  , y = mutate(test_eff,DATE=as_date(DEBUT,tz='CET'))
  , by = c('CODE_ENTITE','DEBUT'='DATE','FIN'='DATE')
  , match_fun = list(`==`,`<=`,`>`)
)


test_semi_perim = interval_left_join(
  x = mutate(test_perim,start=DEBUT,end=FIN)
  , y = mutate(test_entt,start=DEBUT,end=FIN)
  , by = c('CODE_ENTITE','DEBUT','FIN')
)
