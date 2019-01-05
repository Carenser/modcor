### Modele Corrigé - Enedis La F<brique Numerique - Décembre 2018
## Outils de calcul des chroniques d'effacement et de report des sites soumis au modèle corrigé
options(encoding = 'latin1')

#Chargement des arguments du BATCH
args <- commandArgs(trailingOnly = TRUE)
dir_data <- gsub(x = args[1], pattern = "\\\\", replacement = "/")
dir_script <- gsub(x = args[2], pattern = "\\\\", replacement = "/")
dir_log <- gsub(x = args[3], pattern = "\\\\", replacement = "/")

#Sauvegarde du journal d'exécution
file_log = paste(dir_log, format(Sys.time(),'modcor_%Y%m%d%H%M%S.log'), sep = '/')
dir.create(dir_log, showWarnings = FALSE)
invisible(capture.output(file.create(file_log, showWarnings = FALSE)))
file_con = file(description = file_log, open = 'a+b')

sink(file = file_con, append = T, split = F, type = "message")
sink(file = file_con, append = T, split = T, type = "output")

list_packages = c('tidyverse', 'lubridate', 'fuzzyjoin', 'furrr')

# installation des librairies nécessaires
for(package in list_packages)
{
  if(!package %in% installed.packages()[,"Package"])
    install.packages(package, dependencies = TRUE)
  
  # chargement des packages
  if(!package %in% (.packages()))
    library(package, quietly = TRUE, character.only = TRUE)
}

# chargement du package modcor
if(!'modcor' %in% installed.packages()[,"Package"])
  install.packages(paste0(dir_script,"/modcor_3.0.tar.gz"), repos = NULL)

if(!'modcor' %in% (.packages()))
  library('modcor', quietly = TRUE, character.only = TRUE)

# # chargement des fonctions modcor
# invisible(
#   capture.output(
#     map(
#       .x = list.files(path = dir_script, pattern = '[^main.R]', full.names = TRUE)
#       , .f = function(x)
#       {
#         func = source(file = x, verbose = FALSE,echo = FALSE,print.eval = TRUE)
#       }
#     )
#   )
# )

# filtre sur les périodes avec fichier de CdC associé sans chronique d'effacement
tbl_onhold = get_onhold(dir_data)

if(nrow(tbl_onhold)==0)
{
  cat("Aucun fichier à traiter !\nSupprimez les fichiers CRMODECOR ou ajoutez les fichiers CRMA ou NEBEF_CRS, puis relancez le script.")
  
}else{
  
  time.Start <- Sys.time()
  
  # boucle sur les semaines sans chronique d'effacement
  for(mois in unique(tbl_onhold$mois_validite)){
    
    cat('\nMois de', format(as_date(mois),'%B %Y'),'\n')
    
    cat('\n\tChargement des effacements\n')
    tbl_eff = read_eff(dossiers = paste(dir_data, format(as_date(mois) - months(0:3),'%Y%m'), sep = '/'))
    
    cat("\n\tChargement des journées d'indisponibilité\n")
    tbl_indhist = read_indispo(dossiers = paste(dir_data, format(as_date(mois) - months(0:3), '%Y%m'), sep = '/'))
    
    cat('\n\tChargement des périmètres\n')
    tbl_sites = read_perim(dossiers = paste(dir_data, format(as_date(mois),'%Y%m'), sep = '/'))
    
    cat("\n\tChargement des listes d'entités\n")
    tbl_entt = read_entt(dossiers = paste(dir_data, format(as_date(mois),'%Y%m'), sep = '/'))
    
    cat('\n\tChargement de la liste des sites homologués\n')
    tbl_homol = read_homol(dossiers = paste(dir_data, format(as_date(mois),'%Y%m'), sep = '/'))
    
    cat('\n\tIdentification des périodes de consommation nécéssaires au calcul\n')
    char_pattern = get_infos(
      tbl_eff = dplyr::filter(tbl_eff, as_date(DEBUT, tz = 'CET') %within% (as_date(mois)%--%rollback(as_date(mois) + months(1))))
      , tbl_entt
      , tbl_sites
      , tbl_homol
      , tbl_indhist = bind_rows(
        tbl_indhist
        , fuzzy_inner_join(
          y = transmute(tbl_sites, MECANISME, CODE_ENTITE, CODE_SITE, DEBUT, FIN)
          , x = transmute(tbl_eff, MECANISME, CODE_ENTITE, DATE = as_date(DEBUT, tz = 'CET'))
          , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
          , match_fun = list(`==`,`==`,`>=`,`<`)
        ) %>%
          transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, CODE_SITE, DATE)
      ) %>%
        distinct()
    )
    
    for(semaine in unique(subset(tbl_onhold, mois_validite == as_date(mois) , date_validite)[[1]]))
    {
      cat('\n\tsemaine du', format(as_date(semaine),'%A %d %B %Y') , 'au', format(as_date(semaine) + days(6),'%A %d %B %Y'),'\n')
      
      mecanisme =  unique(subset(tbl_onhold, mois_validite == as_date(mois) & date_validite == as_date(semaine), mecanisme)[[1]])
      
      # identification des chroniques de prévision ou de consommation à charger
      pattern_cdc =  
        char_pattern %>%
        dplyr::filter(MECANISME %in% mecanisme & int_overlaps(DEBUT%--%FIN, (as_date(semaine)%--%(as_date(semaine) + days(6))))) %>%
        transmute(
          pattern = pmap_chr(.l = list(x = MECANISME, y = sem)
                             , .f = function(x,y)
                             {
                               case_when(
                                 x == 'MA' ~ str_c('CRMA.*_(', format(as_date(as_datetime(unique(y), tz = 'UTC'), tz = 'UTC'), '%Y%m%d'),')', collapse = '|')
                                 , x == 'NEBEF' ~ str_c('NEBEF_CRS.*_(', format(as_date(as_datetime(unique(y), tz = 'UTC'), tz = 'UTC'), '%Y%m%d'),')', collapse = '|')
                                 , TRUE ~ ""
                               )
                             }
          )
        ) %>%
        distinct() %>%
        unlist(use.names = FALSE) %>%
        {paste0(.,collapse = '|')}
      
      pattern_prev = 
        char_pattern %>%
        dplyr::filter(
          METHODE == 'PREVISION' & MECANISME %in% mecanisme & int_overlaps(DEBUT%--%FIN, (as_date(semaine)%--%(as_date(semaine) + days(6))))
        ) %>%
        transmute(
          pattern = pmap_chr(
            .l = list(x = MECANISME, y = as_date(DEBUT,tz='CET'), z = METHODE)
            , .f = function(x,y,z)
            {
              if_else( z == 'PREVISION', str_c(x, '_PREV.*_', format(as_date(y), '%Y%m%d'), sep = ''), "empty", "empty")
            }
          )
        ) %>%
        distinct() %>%
        unlist(use.names = FALSE) %>%
        {if_else(length(.) == 0, "empty", paste0(.,collapse = '|'))}
      
      if(pattern_prev!='empty')
        cat('\n\t\tChargement des chroniques de prévision\n')
      
      tbl_prev = read_prev(fichiers = list.files(path = paste(dir_data, format(as_date(mois),'%Y%m'), sep = '/'), pattern = pattern_prev, full.names = TRUE), dossiers = NULL)
      
      if(pattern_cdc != "")
      {
        cat('\n\t\tChargement des chroniques de consommation\n')
        
        tbl_cdc = read_cdc(
          fichiers = list.files(path = paste(dir_data, format(as_date(mois) - months(0:6),'%Y%m'), sep = '/'), pattern = pattern_cdc, full.names = TRUE)
          , dossiers = NULL
        )
        
        cat("\n\t\tCalcul des chroniques d'effacement ou report\n")
        
        tbl_cdcRef = create_ref(
          tbl_cdc = tbl_cdc
          , tbl_sites = tbl_sites
          , tbl_eff = dplyr::filter(tbl_eff, as_date(DEBUT) %within% (as_date(semaine)%--%(as_date(semaine) +days(6))))
          , tbl_entt = tbl_entt
          , tbl_homol = tbl_homol
          , tbl_indhist = bind_rows(
            tbl_indhist
            , fuzzy_inner_join(
              y = transmute(tbl_sites, MECANISME, CODE_ENTITE, CODE_SITE, DEBUT, FIN)
              , x = transmute(tbl_eff, MECANISME, CODE_ENTITE, DATE = as_date(DEBUT,tz = 'CET'))
              , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
              , match_fun = list(`==`,`==`,`>=`,`<`)
            ) %>%
              transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, CODE_SITE, DATE) %>%
              distinct()
          )
          , tbl_prev = tbl_prev
        )
        
        cat("\n\t\tEcriture des fichiers csv de chroniques d'effacement ou report\n")
        
        int_nbFiles = write_modcor(tbl_chron = tbl_cdcRef, char_path = paste(dir_data, format(as_date(mois),'%Y%m'), sep = '/'))
        
        #cat(int_nbFiles, ' fichier(s) sauvegardé(s)\n')
      }
    }
    
    decompt <- difftime(Sys.time(), time.Start, units = "auto")
    cat('\n',capture.output(decompt),'\n')
  }
}

sink(file = NULL,type = 'message')
sink(file = NULL,type = 'output')
close(file_con)
