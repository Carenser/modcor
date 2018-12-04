library(tidyverse)
library(lubridate)

test_eff = LoadEffacements(dossier = paste(DIR_DATA,'201801',sep='/'))

test_perim = LoadPerimetre(dossier = paste(DIR_DATA,'201801',sep='/'))

test_cdc = LoadCdC(fichiers = list.files(path = dossier, pattern = 'NEBEF_CRS|CRMA_0001', full.names = TRUE))
