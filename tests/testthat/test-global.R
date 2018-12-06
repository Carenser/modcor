library(tidyverse)
library(lubridate)

test_eff = LoadEffacements(dossiers = paste0(getwd(),'/data-raw/201801'))

test_perim = LoadPerimetre(dossier = paste0(getwd(),'/data-raw/201801'))

test_entt = LoadListeEntt(dossiers =  paste0(getwd(),'/data-raw/201802'))

test_cdc = LoadCdC(fichiers = list.files(path = paste0(getwd(),'/data-raw/201801'), pattern = 'CRMA_0001', full.names = TRUE))

test_cdc = LoadCdC(fichiers = list.files(path = paste0(getwd(),'/data-raw/201801'), pattern = 'NEBEF_CRS|CRMA_0001', full.names = TRUE))

