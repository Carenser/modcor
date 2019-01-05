#' get information about MA/NEBEF data saved in a directory
#'
#' @param dir_data a character string giving the data repository
#' @import tidyverse
#' @import lubridate
#' @return a tibble dataframe giving information about load consumption and curtailement timeseries available
#' @export
#'
#' @examples
get_onhold = function(dir_data){
  
  list.files(
    recursive = TRUE
    , full.names = TRUE
    , path = dir_data
    , pattern = "^(MA|NEBEF)_CRMODECORRIGE_[0-9]{6,8}_[0-9A-Z]{16}_[0-9]{14}.csv|^CRMA_[0-9]{4}_[0-9]{8}_[0-9]{6}_[0-9]{8}.csv|^NEBEF_CRS_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv"
  ) %>%
  {stringr::str_match(string =  ., pattern = ".*/(MA|NEBEF)_CRMODECORRIGE_([0-9]{6,8})_[0-9A-Z]{16}_([0-9]{14}).csv$|.*/CRMA_[0-9]{4}_([0-9]{8}_[0-9]{6})_([0-9]{8}).csv$|.*/NEBEF_CRS_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$")} %>%
    dplyr::as_tibble() %>%
    dplyr::transmute(
      dossier =  stringr::str_extract(string = V1, pattern = '([/]?[^/]+[/]{1})+')
      , fichier = stringr::str_remove(string = V1, pattern = '([/]?[^/]+[/]{1})+')
      , mecanisme = dplyr::case_when(str_detect(string = V1, pattern = 'CRMA|MA') ~ 'MA', str_detect(string = V1,pattern = 'NEBEF') ~ 'NEBEF', TRUE ~ NA_character_)
      , type = dplyr::case_when(str_detect(string = V1, pattern = 'CRMODECORRIGE') ~ 'chron_eff', TRUE ~ 'chron_conso')
      , horodate_creation = coalesce(ymd_hms(V4,tz = 'CET'), ymd_hms(V5,tz = 'CET'), ymd_hms(V8,tz = 'CET'))
      , date_validite = coalesce(as_date(V3), as_date(V6), as_date(V7))
      , source = if_else(condition = nchar(date_validite) == 6 & type == 'chron_eff', true = "RTE", false = "GRD", missing = "GRD")
      , mois_validite = as_date(str_c(stringr::str_extract(string = dossier, pattern = '/20[0-9]{4}/$'),'01'))
    ) %>%
    #Filtre sur les dossiers correspondants a des mois (Cas eventuel de dossier 'archive')
    dplyr::filter(str_detect(string = dossier, pattern = "/20[0-9]{4}/$")) %>% 
    #Filtre des fichiers avec periode correspondant au mois du dossier
    dplyr::filter(
      ( source == 'GRD' &
          int_overlaps(
            int1 = interval(
              start = mois_validite
              , end = rollback(mois_validite + months(1))
            )
            , int2 = date_validite%--%(date_validite + days(6))
          )
      ) | 
        (source =='RTE' & mois_validite == date_validite)
    ) %>%
    #Tri des fichiers par type, mecanisme, Dossier (mois), periode et horodate de creation decroissante
    dplyr::arrange(mecanisme, desc(dossier), type, desc(date_validite), desc(horodate_creation)) %>% # On trie les fichiers par mécanisme, période de validité et horodate de création
    #Dedoublonnage des fichiers de CdC en selectionnant les plus recents par mecanisme, Dossier, type, source et periode
    dplyr::distinct(mecanisme, dossier, type, date_validite, .keep_all = TRUE) %>%
    add_row(type = c('chron_eff','chron_conso')) %>%
    select(-fichier) %>%
    spread(key = type, value = horodate_creation) %>%
    dplyr::filter(!is.na(source)) %>%
    dplyr::filter(source == 'GRD' & is.na(chron_eff) & !is.na(chron_conso))
}
