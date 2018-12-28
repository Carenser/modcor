### Modele Corrigé - Enedis La F<brique Numerique - Décembre 2018
## Outils de calcul des chroniques d'effacement et de report des sites soumis au modèle corrigé

#Chargement des arguments du BATCH
args <- commandArgs(trailingOnly = TRUE)
dir_data <- gsub(x = args[1], pattern = "\\\\", replacement = "/")
# dir_script <- gsub(x = args[2], pattern = "\\\\", replacement = "/")

dir_data = '~/R/MyPack/modcor/data-raw/201802'

# installation des librairies nécessaires
if(!'modcor' %in% installed.packages()[,"Package"])
  install.packages('modcor', dependencies = TRUE)

# chargement des packages
if(!'modcor' %in% (.packages()))
  library('modcor', quietly = TRUE, character.only = TRUE)

file.path(R.home("bin"), "R")

#Sauvegarde du journal d'exécution
file_log = paste(dir_data, format(Sys.time(),'modcor_%Y%m%d%H%M%S.log'), sep = '/')
file.create(file_log, showWarnings = FALSE)
file_con = file(description = file_log, open = 'a+b')

sink(file = file_con, append = T, split = F, type = "message")
sink(file = file_con, append = T, split = T, type = "output")

message('Chargement des données')
time.Start <- Sys.time()

tbl_eff = read_eff(dossiers = dir_data)

tbl_cdc = read_cdc(dossiers = dir_data, fichiers = NULL)

tbl_sites = read_perim(dossiers = dir_data, fichiers = NULL)

tbl_entt = read_entt(dossiers = dir_data)

tbl_homol = read_homol(dossiers = dir_data)

tbl_indhist = read_indispo(dossiers = dir_data)

tbl_prev = read_prev(dossiers = dir_data)

decompt <- difftime(Sys.time(), time.Start, units = "auto")
message(capture.output(decompt))

# Calcul des chroniques d'effacement ou report ----------------------------

message("Calcul des chroniques d'effacement ou report")
time.Start <- Sys.time()

tbl_cdcRef = create_ref(
  tbl_cdc = tbl_cdc
  , tbl_sites = tbl_sites
  , tbl_eff = tbl_eff
  , tbl_entt = tbl_entt
  , tbl_homol = tbl_homol
  , tbl_indhist = tbl_indhist
  , tbl_prev = tbl_prev
)

decompt <- difftime(Sys.time(), time.Start, units = "auto")
message(capture.output(decompt))

# Export au format prévu dans les règles SI -------------------------------

message("\nEcriture des fichiers csv de chroniques d'effacement ou report")
time.Start <- Sys.time()
write_modcor(tbl_chron = tbl_cdcRef, char_path = paste0(getwd(),'/data-raw'))
decompt <- difftime(Sys.time(), time.Start, units = "auto")
message(capture.output(decompt))

sink(file = NULL,type = 'message')
sink(file = NULL,type = 'output')
close(file_con)
