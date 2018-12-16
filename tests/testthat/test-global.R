library(tidyverse)
library(lubridate)
library(fuzzyjoin)

options(warning.length = 5000L, tibble.print_max = Inf)

map(.x = list.files(path = paste0(getwd(),'/R'), pattern = '[^Main.R]',full.names = TRUE),.f = function(x){source(file = x, prompt.echo = FALSE, verbose = FALSE, print.eval = FALSE, echo = FALSE)})

# Chargement des données  -------------------------------------------------

tbl_cdc = LoadCdC(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '.*_20180210_.*.csv', full.names = TRUE), dossiers = NULL)

tbl_eff = LoadEffacements(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '(OA_GRD|PEC_GRD)_201802', full.names = TRUE), dossiers = NULL)

#tbl_eff = LoadEffacements(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '(OA_GRD|PEC_GRD)', full.names = TRUE), dossiers = NULL)

tbl_sites = LoadPerimetre(dossiers = paste0(getwd(),'/data-raw/201802'))

tbl_entt = LoadListeEntt(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_homol = LoadSitesHomol(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_indhist = LoadIndHist(dossiers  =  paste0(getwd(),'/data-raw/201802'))

tbl_prev = LoadPrev(dossiers =  paste0(getwd(),'/data-raw/201802'))

list_data = fuzzy_left_join(
  x = mutate(tbl_eff, DATE = as_date(DEBUT,tz='CET'))
  , y = tbl_entt
  , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
  , match_fun = list(`==`,`==`,`>=`,`<`)
) %>%
  #(méthode du rectangle par défaut).
  replace_na(list(METHODE = 'RECTANGLE')) %>%
  transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO) %>%
  fuzzy_left_join(
    y = tbl_sites
    , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
    , match_fun = list(`==`,`==`,`>=`,`<`)
    #ajout des sites rattachés aux entités
  ) %>%
  transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT) %>%
  fuzzy_left_join(
    y = tbl_homol
    , by = c('MECANISME','CODE_SITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
    , match_fun = list(`==`,`==`,`>=`,`<`)
  ) %>% # Un site ne peut être rattaché à une entité certifiée par des méthodes autres que RECTANGLE que s'il est homologué à ces méthodes
  dplyr::filter(!(METHODE.x != 'RECTANGLE' & METHODE.y != METHODE.x)) %>%
  transmute(
    MECANISME = MECANISME.x
    , CODE_ENTITE
    , CODE_SITE = CODE_SITE.x
    , CAPA_MAX_H_SITE
    , TYPE_CONTRAT
    , METHODE = METHODE.x
    #si la méthode est HISTORIQUE, la variante par défaut est MOY10J (utile ?)
    , VARIANTE = if_else(condition = METHODE.x == 'HISTORIQUE' & is.na(VARIANTE), true = 'MOY10J', false =  VARIANTE)
    , DEBUT = DEBUT.x
    , FIN = FIN.x
    , SIGNE
    , DMO
  ) %>%
  distinct() %>% #sites homologués à d'autres méthodes que le rectangle
  split(list(.$MECANISME, .$CODE_SITE, .$METHODE), drop = TRUE)

prog = list_data[grep(names(list_data),pattern = 'NEBEF.PRM30000520044831.PREVISION',value = T)][[1]]
#prog = test[grep(names(test),pattern = '(MA|NEBEF).PRM[0-9]{14}.[^RECTANGLE]',value = T)][[1]][1:4,]

call(
  name = paste('CR', unique(prog[['METHODE']]), sep = '_') #application de la méthode de contrôle du réalisé de l'entité
  , eff = prog
  , cdc = fuzzy_semi_join(
    # On récupère au plus 60 jours d'historique de CdC par site soumis à la méthode HISTORIQUE (à optimiser?)
    x = mutate(dplyr::filter(tbl_cdc,MECANISME == unique(prog[['MECANISME']]) & CODE_ENTITE == unique(prog[['CODE_ENTITE']]) & CODE_SITE == unique(prog[['CODE_SITE']])), DATE = as_date(HORODATE, tz = 'CET'))
    , y = mutate(prog, DEBUT_ETENDU = as_date(DEBUT,tz='CET') - if_else(METHODE == 'HISTORIQUE', days(60), days(1)), FIN_ETENDU = as_date(FIN,tz='CET') + days(2))
    , by = c('MECANISME','CODE_ENTITE','CODE_SITE', 'DATE' = 'DEBUT_ETENDU', 'DATE' = 'FIN_ETENDU')
    , match_fun = list(`==`,`==`,`==`,`>=`,`<`)
  )
  # On récupère les prévision de CdC par site soumis à la méthode par PREVISION
  , prev = dplyr::filter(
    tbl_prev
    , MECANISME == unique(prog[['MECANISME']]) &
      CODE_ENTITE == unique(prog[['CODE_ENTITE']]) &
      CODE_SITE == unique(prog[['CODE_SITE']]) &
      HORODATE %within% as.list(prog[['DEBUT']] %--% prog[['FIN']])
  )
) %>%
  eval() %>% View()

test_results2 =
  list_data[grep(names(list_data),pattern = 'NEBEF.PRM30000520044831.PREVISION',value = T)] %>%
  {
    purrr::map_dfr(
      .x = .
      , tbl_ts = tbl_cdc
      , tbl_prev = tbl_prev
      , .f = function(tbl_prog, tbl_ts, tbl_prev)
      {
        call(
          name = paste('CR', unique(tbl_prog[['METHODE']]), sep = '_') #application de la méthode de contrôle du réalisé de l'entité
          , eff = tbl_prog
          , cdc = fuzzy_semi_join(
            # On récupère au plus 60 jours d'historique de CdC par site soumis à la méthode HISTORIQUE (à optimiser?)
            x = mutate(dplyr::filter(tbl_ts,MECANISME == unique(tbl_prog[['MECANISME']]) & CODE_ENTITE == unique(tbl_prog[['CODE_ENTITE']]) & CODE_SITE == unique(tbl_prog[['CODE_SITE']])), DATE = as_date(HORODATE, tz = 'CET'))
            , y = mutate(tbl_prog, DEBUT_ETENDU = as_date(DEBUT,tz='CET') - if_else(METHODE == 'HISTORIQUE', days(60), days(1)), FIN_ETENDU = as_date(FIN,tz='CET') + days(2))
            , by = c('MECANISME','CODE_ENTITE','CODE_SITE', 'DATE' = 'DEBUT_ETENDU', 'DATE' = 'FIN_ETENDU')
            , match_fun = list(`==`,`==`,`==`,`>=`,`<`)
          )
          # On récupère les prévision de CdC par site soumis à la méthode par PREVISION
          , prev = dplyr::filter(
            tbl_prev
            , MECANISME == tbl_prog[['MECANISME']] &
              CODE_ENTITE == tbl_prog[['CODE_ENTITE']] &
              CODE_SITE == tbl_prog[['CODE_SITE']] &
              HORODATE_UTC %within% as.list(prog[['DEBUT']] %--% prog[['FIN']])
          )
        ) %>%
          eval()
      }
    )
  }

# AUTRES ------------------------------------------------------------------


test_semi_perim =

  test_perim %>% split(.$CODE_ENTITE) %>%
  {
    pmap_df(
      .l = list(x = ., .y = names(.), z = test_entt)
      , .f = function(x,y,z){
        dplyr::filter(test_entt, CODE_ENTITE == y) %>%
          interval_right_join(x, by = c('DEBUT','FIN'))
      }
    )
  }

(
  x =
    , y = test_entt
  , by = c('DEBUT','FIN','CODE_ENTITE')
  , by = c('DEBUT','FIN')
)



test_eff = LoadEffacements(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = 'OA_GRD|PEC_GRD', full.names = TRUE), dossiers = NULL)

test_perim = LoadPerimetre(dossiers = paste0(getwd(),'/data-raw/201802'))

test_entt = LoadListeEntt(dossiers =  paste0(getwd(),'/data-raw/201802'))

test_homol = LoadSitesHomol(dossiers =  paste0(getwd(),'/data-raw/201802'))

test_indispo = LoadIndHist(dossiers  =  paste0(getwd(),'/data-raw/201802'))

test_cdc_prev = LoadPrev(dossiers =  paste0(getwd(),'/data-raw/201802'))

#test_cdc_ma = LoadCdC(fichiers = list.files(path = paste0(getwd(),'/data-raw/201801'), pattern = 'CRMA_0001', full.names = TRUE))

#test_cdc_nebef = LoadCdC(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/201802'), pattern = 'NEBEF_CRS.*_', full.names = TRUE))

test_cdc_nebef = LoadCdC(fichiers = paste0(getwd(),'/data-raw/201802/NEBEF_CRS_GRD_20180127_17X100A100A0001A_20180328104119.csv'))

#test_cdc = LoadCdC(fichiers = list.files(path = paste0(getwd(),'/data-raw/201801'), pattern = 'NEBEF_CRS|CRMA_0001', full.names = TRUE))

test_semi_perim = fuzzy_semi_join(
  x = distinct(dplyr::filter(test_perim,TYPE_CONTRAT == 'CARD'), DEBUT, FIN, CODE_ENTITE)
  , y = mutate(test_eff,DATE=as_date(DEBUT,tz='CET'))
  , by = c('CODE_ENTITE','DEBUT'='DATE','FIN'='DATE')
  , match_fun = list(`==`,`<=`,`>`)
)
