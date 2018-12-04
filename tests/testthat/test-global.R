library(tidyverse)
library(reshape2)
library(lubridate)

library(data.table)

test_eff = LoadEffacements(dir = paste(DIR_DATA,'201801',sep='/'))

test_perim = LoadPerimetre(dir = paste(DIR_DATA,'201801',sep='/'))

test_cdc = LoadCdC(files = list.files(path = dir, pattern = 'NEBEF_CRS|CRMA_0001', full.names = TRUE))
