LoadPerimetre<-function(fichiers = NULL, dossiers){

  if(is.null(fichiers))
  {
    fichiers = list.files(full.names = TRUE, path = dossiers,pattern =  "^(MA|NEBEF)_REFST_TLRLV_GRD_[0-9]{6}_[0-9A-Z]{16}_[0-9]{14}.csv$")

    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  if(is.null(dossiers))
  {
    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  #Si aucun fichier n'est conforme à la nomenclature alors pas de traitement
  if(!any(stringr::str_detect(string = fichiers,pattern = "^(MA|NEBEF)_REFST_TLRLV_GRD_[0-9]{6}_[0-9A-Z]{16}_[0-9]{14}.csv$")))
  {
    stop("aucun fichier de périmètre conforme à la nomenclature prévue dans les règles SI MA ou NEBEF")

  }else{

    stringr::str_match(string = fichiers, pattern =  "^(MA|NEBEF)_REFST_TLRLV_GRD_([0-9]{6})_[0-9A-Z]{16}_([0-9]{14}).csv$") %>%
      dplyr::as_tibble() %>%
      dplyr::transmute(
        dossier = dossiers
        , fichier = V1
        , mecanisme = V2
        , horodate_creation = ymd_hms(V4,tz = 'CET')
        , date_validite = as_date(str_c(V3,'01'))
      ) %>%
      dplyr::filter(str_detect(string = fichier, pattern = "^(MA|NEBEF)_REFST_TLRLV_GRD_([0-9]{6})_[0-9A-Z]{16}_([0-9]{14}).csv$")) %>% # On ne conserve que les fichiers de périmètre
      dplyr::arrange(mecanisme, date_validite, desc(horodate_creation)) %>% # On trie les fichiers par mécanisme, période de validité et horodate de création
      dplyr::distinct(mecanisme, date_validite,.keep_all = TRUE) %>% # On ne conserve que le dernier fichier reçu pour une période donnée (pour NEBEF on ne conserve que le dernier fichier créé)
      {
        purrr::pmap_dfr(
          .l = list(
            x = str_c(.$dossier,.$fichier,sep='')
            , y = .$mecanisme
            , z = .$date_validite
          )
          , .f = function(x,y,z){
            suppressWarnings(
              readr::read_delim(
                file = x
                , delim = ';'
                , locale = locale(decimal_mark = ',')
                , comment = '<EOF>'
                , col_types =
                  list(
                    # CODE_EIC_GRD
                    # , TYPE_SITE
                    # , ID_SITE
                    CAPA_MAX_H_SITE = 'i'
                    , CAPA_MIN_H_SITE = 'i'
                    , CAPA_MAX_B_SITE = 'i'
                    , CAPA_MIN_B_SITE = 'i'
                    , CAPA_MAX_RPH_SITE = 'i'
                    , CAPA_MAX_RSH_SITE = 'i'
                    , CAPA_MAX_RPB_SITE = 'i'
                    , CAPA_MAX_RSB_SITE = 'i'
                    , PS = 'd' #Linky avec PS décimales
                    # , CODE_EIC_RE
                    # , CODE_EIC_FOURNISSEUR
                    # , BAREME
                    # , CATEGORIE
                    # , TYPE_CDC
                    # , ORIGINE_DONNEE
                    # , OBJET_MESURE
                    # , TYPE_CONTRAT
                    # , CODE_EDA
                    # , CODE_EDE
                    , DATE_CONTRACTUALISATION = col_date(format = '%Y%m%d')
                    # , CODE_EDR
                    # , DEROGATION_MODELECORRIGE
                    # , CODE_EIF
                    , .default = 'c'
                  )
                , skip = 2
              ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                dplyr::transmute(
                  CODE_ENTITE = dplyr::case_when(y == 'MA' ~ CODE_EDA, y == 'NEBEF' ~ CODE_EDE, TRUE ~ NA_character_)
                  , CODE_SITE = stringr::str_c(TYPE_SITE,ID_SITE,sep='')
                  , CAPA_MAX_H_SITE
                  , TYPE_CONTRAT
                  , CATEGORIE
                ) %>% #On ajoute les colonnes de début et fin de période et le mécanisme
                tibble::add_column(DEBUT = lubridate::as_date(z), FIN = lubridate::as_date(z) + lubridate::days_in_month(as_date(z)), MECANISME = y, .before = 1) %>%
                dplyr::filter(CODE_ENTITE != 'N') # On supprime les lignes sans entité du mécanisme
            )
          }
        )
      }
  }
}

#' Chargement des fichiers de programme d'effacements retenus et d'ordres d'activations consolidés en J+3 aux formats prévues dans les règles SI décrivant les flux en provenance de RTE à destination des GRD
#'
#' @param dossiers le(s) nom(s) de répertoire contenant les fichiers passés en paramètre
#'
#' @return un dataframe comprenant 5 colonnes : CODE_ENTITE, DEBUT, FIN, SIGNE, DMO
#' @export
#' @import reshape2
#' @import tidyverse
#' @examples
LoadEffacements <- function(dossiers, fichiers = NULL)
{

  # dmo<-oa2[!duplicated(oa2[,c("CODE_EDA","HORODATE")]),c("CODE_EDA","HORODATE","DMO")]#on conserve le DMO du premier ordre
  # oa3<-aggregate(PUISSANCE~CODE_EDA+HORODATE,oa2,sum)
  # oa4<-merge(oa3,dmo,by=c("CODE_EDA","HORODATE"))
  # names(oa4)[names(oa4)=="CODE_EDA"]<-"CODE_ENTITE"
  # oa4<-oa4[order(oa4$CODE_ENTITE,oa4$HORODATE,oa4$PUISSANCE),]
  # avant<-rbind(c(NA,NA),oa4[-nrow(oa4),c("CODE_ENTITE","PUISSANCE")])
  # apres<-rbind(oa4[-1,c("CODE_ENTITE","PUISSANCE")],c(NA,NA))
  # oa5<-cbind(oa4[,c("HORODATE","CODE_ENTITE","PUISSANCE","DMO")],avant,apres)
  # names(oa5)[5:8]<-c("CODE_ENTITE_avant","PUISSANCE_avant","CODE_ENTITE_apres","PUISSANCE_apres")
  # debut<-oa5[oa5$PUISSANCE>0 & (oa5$PUISSANCE_avant==0 | oa5$CODE_ENTITE!=oa5$CODE_ENTITE_avant | 1:nrow(oa5)==1),]
  # fin<-oa5[oa5$PUISSANCE>0 & (oa5$PUISSANCE_apres==0 | oa5$CODE_ENTITE!=oa5$CODE_ENTITE_apres | 1:nrow(oa5)==nrow(oa5)),]
  # oa<-data.frame(CODE_ENTITE=debut$CODE_ENTITE,debut=debut$HORODATE,fin=fin$HORODATE+60,DMO=debut$DMO,SIGNE=sign(debut$PUISSANCE))

  if(is.null(fichiers))
  {
    fichiers = list.files(full.names = TRUE, path = dossiers,pattern = "^OA_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$|^PEC_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$")

    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  if(is.null(dossiers))
  {
    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  #Si aucun fichier n'est conforme à la nomenclature alors pas de traitement
  if(!any(stringr::str_detect(string = fichiers,pattern = "^OA_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$|^PEC_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$")))
  {
    stop("aucun fichier d'activation conforme à la nomenclature prévue dans les règles SI MA ou NEBEF")

  }else{

    stringr::str_match(string =  fichiers, pattern = "^OA_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$|^PEC_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$") %>%
      dplyr::as_tibble() %>%
      dplyr::transmute(
        dossier = dossiers
        , fichier = V1
        , mecanisme = dplyr::case_when(str_detect(string = V1,pattern = 'OA') ~ 'MA',str_detect(string = V1,pattern = 'PEC') ~ 'NEBEF', TRUE ~ NA_character_)
        , horodate_creation = coalesce(ymd_hms(V3,tz = 'CET'),ymd_hms(V5,tz = 'CET'))
        , date_validite = coalesce(as_date(V2),as_date(V4))
      ) %>%
      dplyr::filter(str_detect(string = fichier, pattern = "^OA_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$|^PEC_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$")) %>% # On ne conserve que les fichiers de CdC
      dplyr::arrange(mecanisme, date_validite, desc(horodate_creation)) %>% # On trie les fichiers par mécanisme, période de validité et horodate de création
      dplyr::distinct(mecanisme, date_validite,.keep_all = TRUE) %>% # On ne conserve que le dernier fichier reçu pour une période donnée
      {
        purrr::pmap_dfr(
          .l = list(
            x = str_c(.$dossier,.$fichier,sep='')
            , y = .$mecanisme
            , z = .$date_validite
          )
          , .f = function(x,y,z){

            if(stringr::str_detect(string = x, pattern = "OA_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$")){ # Traitement des fichiers MA

              readr::read_delim(
                file = x
                , delim = ';'
                , locale = locale(decimal_mark = ',')
                , comment = '<EOF>'
                , col_types =
                  list(
                    ACTIVATION_DEBUT = 'c'
                    , ACTIVATION_FIN = 'c'
                    , PUISSANCE = 'd'
                    , PART_AJUSTEMENT = 'd'
                    , DMO = 'i'
                    , .default = 'c'
                  )
                , skip = 2
              ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                dplyr::transmute(
                  CODE_ENTITE = CODE_EDA
                  , DEBUT = parse_datetime(x = ACTIVATION_DEBUT, format = '%Y%m%d%H%M%S', locale = locale(tz = 'CET'))
                  , FIN = parse_datetime(x = ACTIVATION_FIN, format = '%Y%m%d%H%M%S', locale = locale(tz = 'CET'))
                  , SIGNE = dplyr::case_when(stringr::str_to_upper(SENS_AJUSTEMENT) == 'HAUSSE' ~ +1, stringr::str_to_upper(SENS_AJUSTEMENT) == 'BAISSE' ~ -1)
                  , DMO
                  , PUISSANCE = PUISSANCE * SIGNE #On affecte un signe aux puissances activés temporairement avant consolidation des OA
                ) %>% #On renomme la table avec des noms communs aux différents mécanismes
                tibble::add_column(MECANISME = y,.before = 1) %>% #On ajoute la colonne mécanisme
                dplyr::group_by(MECANISME, CODE_ENTITE, DEBUT, FIN) %>% #On regroupe les OA par entité, début et fin
                dplyr::summarise( #On consolide les OA portant exactement sur la même période
                  SIGNE = sign(sum(PUISSANCE))
                  , DMO = max(DMO)
                  #, PUISSANCE = abs(sum(PUISSANCE))
                ) %>%
                dplyr::ungroup() %>%
                dplyr::filter(SIGNE !=0) #On supprime les OA désactivés par contrepassation

            }else{

              if(stringr::str_detect(string = x, pattern = "PEC_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$"))
              { # Traitement des fichiers NEBEF


                suppressWarnings( # Warning lié au format : ajout d'une colonne vide en fin de ligne
                  readr::read_delim(
                    file = x
                    , delim = ';'
                    , locale = locale(date_format = '%Y%m%d', decimal_mark = ',', tz = 'CET')
                    , comment = '<EOF>'
                    , col_types =
                      list(
                        CODE_EDE = 'c'
                        , TYPE_EDE = 'c'
                        , NB_PTS_CHRONIQUE = 'i'
                        , .default = 'd'
                      )
                    , skip = 2
                  )
                ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                  dplyr::select(CODE_EDE,NB_PTS_CHRONIQUE,starts_with('VAL')) %>% # On supprime les colonnes inutiles
                  dplyr::rename(CODE_ENTITE = CODE_EDE) %>% #On renomme la table avec des noms communs aux différents mécanismes
                  tidyr::gather(- CODE_ENTITE, - NB_PTS_CHRONIQUE, key = 'MINUTE', value = 'PUISSANCE') %>% #On transpose la table en ligne
                  dplyr::mutate(DATE = as_date(z), MINUTE = parse_integer(str_extract(string = MINUTE, pattern = '[0-9]+')) - 1) %>% # On interprète le nom de la colonne VAL en numérique
                  dplyr::filter(MINUTE < NB_PTS_CHRONIQUE) %>% #On filtre les points inutiles (VAL150, etc, ...)
                  dplyr::mutate(MINUTE = 60 * 24 * MINUTE/NB_PTS_CHRONIQUE) %>% #On convertit la valeur de VAL en minute
                  dplyr::mutate(HORODATE = lubridate::force_tz(as_datetime(x = DATE), tzone = 'CET') + lubridate::dminutes(MINUTE)) %>% #On crée une colonne horodate
                  dplyr::transmute(CODE_ENTITE, HORODATE,  HORODATE_UTC = lubridate::with_tz(HORODATE,tzone = 'UTC'), PUISSANCE) %>% #On convertit la puissance en kWh, on crée une colonne horodate_UTC en conservant les autres colonnes nécessaires
                  {chron2prog(tbl_ts = ., char_group = 'CODE_ENTITE', char_pow = 'PUISSANCE', char_datetime = 'HORODATE_UTC', int_step = 1800)} %>% #On transpose les chroniques en programme
                  dplyr::rename(CODE_ENTITE = group, DEBUT = begin, FIN = end, SIGNE = sign) %>%
                  tibble::add_column(MECANISME = y,.before = 1) %>%
                  tibble::add_column(DMO = NA) %>%
                  dplyr::mutate(DEBUT = lubridate::with_tz(DEBUT,tzone = 'CET'), FIN = lubridate::with_tz(FIN,tzone = 'CET'))
              }
            }
          }
        )
      } %>%
      mutate(DMO = lubridate::dminutes(DMO)) # DMO est exprimé en minutes
  }
}

#' Chargement des fichiers de courbes de charges aux formats prévues dans les règles SI décrivant les flux en provenance des GRD à destination de RTE
#'
#' @param dossiers le(s) nom(s) de répertoire contenant les fichiers passés en paramètre (facultatif)
#' @param fichiers un vecteur contenant les noms des fichiers de courbes de charges
#'
#' @return un dataframe comprenant 6 colonnes : MECANISME, CODE_ENTITE, CODE_SITE, HORODATE, HORODATE_UTC, PUISSANCE
#' @export
#' @import tidyverse
#' @import lubridate
#' @examples
LoadCdC<-function(fichiers, dossiers = NULL){

  if(is.null(dossiers))
  {
    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  #Si aucun fichier n'est conforme à la nomenclature alors pas de traitement
  if(!any(stringr::str_detect(string = fichiers,pattern = "^CRMA_[0-9]{4}_[0-9]{8}_[0-9]{6}_[0-9]{8}.csv$|^NEBEF_CRS_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv$")))
  {
    stop('aucun fichier de CdC conforme à la nomenclature prévue dans les règles SI MA ou NEBEF')

  }else{

    stringr::str_match(string =  fichiers, pattern = "^CRMA_[0-9]{4}_([0-9]{8}_[0-9]{6})_([0-9]{8}).csv$|^NEBEF_CRS_GRD_([0-9]{8})_[0-9A-Z]{16}_([0-9]{14}).csv$") %>%
      dplyr::as_tibble() %>%
      dplyr::transmute(
        dossier = dossiers
        , fichier = V1
        , mecanisme = dplyr::case_when(str_detect(string = V1,pattern = 'CRMA') ~ 'MA',str_detect(string = V1,pattern = 'NEBEF') ~ 'NEBEF', TRUE ~ NA_character_)
        , horodate_creation = coalesce(ymd_hms(V2,tz = 'CET'),ymd_hms(V5,tz = 'CET'))
        , date_validite = coalesce(as_date(V3),as_date(V4))
      ) %>%
      dplyr::filter(str_detect(string = fichier, pattern = "^CRMA_[0-9]{4}_[0-9]{8}_[0-9]{6}_[0-9]{8}.csv$|NEBEF_CRS_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv$")) %>% # On ne conserve que les fichiers de CdC
      dplyr::arrange(mecanisme, date_validite, desc(horodate_creation)) %>% # On trie les fichiers par mécanisme, période de validité et horodate de création
      dplyr::distinct(mecanisme, date_validite,.keep_all = TRUE) %>% # On ne conserve que le dernier fichier reçu pour une période donnée
      {
        purrr::pmap_dfr(
          .l = list(
            x = str_c(.$dossier,.$fichier,sep='')
            , y = .$mecanisme
            , z = .$horodate_creation
          )
          , .f = function(x,y,z){

            if(stringr::str_detect(string = x, pattern = "CRMA_[0-9]{4}_[0-9]{8}_[0-9]{6}_[0-9]{8}.csv")){ # Traitement des fichiers MA

              readr::read_delim(
                file = x
                , delim = ';'
                , locale = locale(date_format = '%Y%m%d', decimal_mark = ',', tz = 'CET')
                , comment = '<EOF>'
                , col_types =
                  list(
                    CODE_EDA = 'c'
                    , CODE_SITE = 'c'
                    , DATE_CRB = 'D'
                    , NB_PTS_CHRONIQUE = 'i'
                    , .default = 'd'
                  )
                , skip = 0
              ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                dplyr::rename(CODE_ENTITE = CODE_EDA, DATE = DATE_CRB) %>% #On renomme la table avec des noms communs aux différents mécanismes
                tidyr::gather(- CODE_ENTITE, - CODE_SITE, - DATE, - NB_PTS_CHRONIQUE, key = 'MINUTE', value = 'PUISSANCE') %>% #On transpose la table en ligne
                dplyr::mutate(MINUTE = parse_integer(str_extract(string = MINUTE, pattern = '[0-9]+')) - 1) %>% # On interprète le nom de la colonne VAL en numérique en retranchant 1
                dplyr::filter(MINUTE < NB_PTS_CHRONIQUE) %>% #On filtre les points inutiles (VAL150, etc, ...)
                dplyr::mutate(MINUTE = (60 * 24 * MINUTE)/NB_PTS_CHRONIQUE) %>% #On convertit la valeur de VAL en minute
                dplyr::mutate(HORODATE = lubridate::force_tz(as_datetime(x = DATE), tzone = 'CET') + lubridate::dminutes(MINUTE)) %>% #On crée une colonne horodate
                dplyr::transmute(CODE_ENTITE, CODE_SITE, HORODATE,  HORODATE_UTC = lubridate::with_tz(HORODATE,tzone = 'UTC'), PUISSANCE) %>% #On crée une colonne horodate_UTC en conservant les autres colonnes nécessaires
                tibble::add_column(HORODATE_CREATION = z, MECANISME = y,.before = 1)

            }else{

              if(stringr::str_detect(string = x, pattern = "NEBEF_CRS_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv")){ # Traitement des fichiers NEBEF

                readr::read_delim(
                  file = x
                  , delim = ';'
                  , locale = locale(date_format = '%Y%m%d', decimal_mark = ',', tz = 'CET')
                  , comment = '<EOF>'
                  , col_types =
                    list(
                      CODE_EDE = 'c'
                      , CODE_EXT_SITE = 'c'
                      , CODE_EIC_GRD = 'c'
                      , DATE = 'D'
                      , NB_PTS_CHRONIQUE = 'i'
                      , .default = 'd'
                    )
                  , skip = 2
                ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                  dplyr::select(- CODE_EIC_GRD) %>% # On supprime les colonnes inutiles
                  dplyr::rename(CODE_ENTITE = CODE_EDE, CODE_SITE = CODE_EXT_SITE) %>% #On renomme la table avec des noms communs aux différents mécanismes
                  tidyr::gather(- CODE_ENTITE, - CODE_SITE, - DATE, - NB_PTS_CHRONIQUE, key = 'MINUTE', value = 'PUISSANCE') %>% #On transpose la table en ligne
                  dplyr::mutate(MINUTE = parse_integer(str_extract(string = MINUTE, pattern = '[0-9]+')) - 1) %>% # On interprète le nom de la colonne VAL en numérique
                  dplyr::filter(MINUTE < NB_PTS_CHRONIQUE) %>% #On filtre les points inutiles (VAL150, etc, ...)
                  dplyr::mutate(MINUTE = 60 * 24 * MINUTE/NB_PTS_CHRONIQUE) %>% #On convertit la valeur de VAL en minute
                  dplyr::mutate(HORODATE = force_tz(as_datetime(x = DATE), tzone = 'CET') + lubridate::dminutes(MINUTE)) %>% #On crée une colonne horodate
                  dplyr::transmute(CODE_ENTITE, CODE_SITE, HORODATE,  HORODATE_UTC = with_tz(HORODATE,tzone = 'UTC'), PUISSANCE) %>% #On crée une colonne horodate_UTC en conservant les autres colonnes nécessaires
                  tibble::add_column(HORODATE_CREATION = z, MECANISME = y,.before = 1)
              }
            }
          }
        )
      } %>%
      arrange(CODE_ENTITE,CODE_SITE,HORODATE_UTC,desc(HORODATE_CREATION)) %>%
      distinct(CODE_ENTITE,CODE_SITE,HORODATE_UTC,.keep_all = TRUE) %>% #On dedoublonne les chroniques en conservant l'horodate de création la plus récente
      select(-HORODATE_CREATION)
  }
}

#' Chargement des fichiers de listing des entités aux formats prévues dans les règles SI décrivant les flux en provenance de RTE à destination des GRD
#'
#' @param dossier le nom du répertoire contenant les fichiers passés en paramètre
#' @param fichiers un vecteur contenant les noms des fichiers de listing des entités  (facultatif)
#'
#' @return un dataframe comprenant 5 colonnes : MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN
#' @export
#' @import tidyverse
#' @import lubridate
#' @examples
LoadListeEntt<-function(dossiers, fichiers = NULL){

  if(is.null(fichiers))
  {
    fichiers = list.files(full.names = TRUE, path = dossiers,pattern = "^LISTE_EDA_GRD_([0-9]{6})_(DECLARATIF|FINAL)_([0-9]{14}).csv$|^LISTE_EDE_GRD_([0-9]{14}).csv$")

    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  if(is.null(dossiers))
  {
    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  #Si aucun fichier n'est conforme à la nomenclature alors pas de traitement
  if(!any(stringr::str_detect(string = fichiers,pattern = "^LISTE_EDA_GRD_([0-9]{6})_(DECLARATIF|FINAL)_([0-9]{14}).csv$|^LISTE_EDE_GRD_([0-9]{14}).csv$")))
  {
    warning("Aucun fichier de listing des entités conforme à la nomenclature prévue dans les règles SI MA ou NEBEF. La méthode par défaut (RECTANGLE) sera appliquée à l'ensemble des entités.")

  }else{

    stringr::str_match(string =  fichiers, pattern = "^LISTE_EDA_GRD_([0-9]{6})_(DECLARATIF|FINAL)_([0-9]{14}).csv$|^LISTE_EDE_GRD_([0-9]{14}).csv$") %>%
      dplyr::as_tibble() %>%
      dplyr::transmute(
        dossier = dossiers
        , fichier = V1
        , mecanisme = dplyr::case_when(str_detect(string = V1,pattern = 'EDA') ~ 'MA',str_detect(string = V1,pattern = 'EDE') ~ 'NEBEF', TRUE ~ NA_character_)
        , version = coalesce(V3,'FINAL')
        , horodate_creation = coalesce(ymd_hms(V4,tz = 'CET'),ymd_hms(V5,tz = 'CET'))
        , date_validite = as_date(str_c(V2,'01')) #Pour la liste des EDE, on considère que les fichiers sont toujours exhaustifs
      ) %>%
      dplyr::filter(str_detect(string = fichier, pattern = "^LISTE_EDA_GRD_([0-9]{6})_(DECLARATIF|FINAL)_([0-9]{14}).csv$|^LISTE_EDE_GRD_([0-9]{14}).csv$")) %>% # On ne conserve que les fichiers de listing des entités
      dplyr::arrange(mecanisme, date_validite, desc(version), desc(horodate_creation)) %>% # On trie les fichiers par mécanisme, période de validité version (declaratif/final) et horodate de création
      dplyr::distinct(mecanisme, date_validite,.keep_all = TRUE) %>% # On ne conserve que le dernier fichier reçu pour une période donnée (pour NEBEF on ne conserve que le dernier fichier créé)
      {
        purrr::pmap_dfr(
          .l = list(
            x = str_c(.$dossier,.$fichier,sep='')
            , y = .$mecanisme
            , z = .$date_validite
          )
          , .f = function(x,y,z){

            if(stringr::str_detect(string = x, pattern = "LISTE_EDA_GRD_([0-9]{6})_(DECLARATIF|FINAL)_([0-9]{14}).csv$")){ # Traitement des fichiers MA

              readr::read_delim(
                file = x
                , delim = ';'
                , locale = locale(decimal_mark = ',')
                , comment = '<EOF>'
                , col_types =
                  list(
                    CODE_EDA = 'c'
                    , TYPE_EDA = 'c'
                    , METHODE = 'c'
                  )
                , skip = 1
              ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                dplyr::transmute(
                  CODE_ENTITE = CODE_EDA
                  , METHODE
                  , DEBUT = as_date(z)
                  , FIN = rollback(as_date(z),roll_to_first = T) + lubridate::days_in_month(as_date(z)) # date de fin exclue en sortie
                ) %>% #On renomme la table avec des noms communs aux différents mécanismes
                tibble::add_column(MECANISME = y,.before = 1) #On ajoute la colonne mécanisme

            }else{

              if(stringr::str_detect(string = x, pattern = "LISTE_EDE_GRD_([0-9]{14}).csv$"))
              { # Traitement des fichiers NEBEF

                readr::read_delim(
                  file = x
                  , delim = ';'
                  , locale = locale(date_format = '%Y%m%d', decimal_mark = ',', tz = 'CET')
                  , comment = '<EOF>'
                  , col_types =
                    list(
                      CODE_EDE = 'c'
                      , METHODE_CONTROLE_REALISE = 'c'
                      , DATE_DEBUT_VALIDITE = 'D'
                      , DATE_FIN_VALIDITE = 'D'
                      , .default = 'c'
                    )
                  , skip = 0
                ) %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
                  dplyr::transmute(
                    CODE_ENTITE = CODE_EDE
                    , METHODE = METHODE_CONTROLE_REALISE
                    , DEBUT = DATE_DEBUT_VALIDITE
                    , FIN = DATE_FIN_VALIDITE + lubridate::days(1) # date de fin exclue en sortie
                  ) %>% # On supprime les colonnes inutiles en renommant la table avec des noms communs aux différents mécanismes
                  tibble::add_column(MECANISME = y,.before = 1)
              }
            }
          }
        )
      }
  }
}

#' Chargement des fichiers de listing des sites homologués aux méthodes historique et prévision aux formats prévues dans les règles SI décrivant les flux en provenance de RTE à destination des GRD
#'
#' @param dossier le nom du répertoire contenant les fichiers passés en paramètre
#' @param fichiers un vecteur contenant les noms des fichiers de listing des sites homologués (facultatif)
#'
#' @return un dataframe comprenant 6 colonnes : MECANISME, CODE_SITE, METHODE, DEBUT, FIN, VARIANTE
#' @export
#' @import tidyverse
#' @import lubridate
#' @examples
LoadSitesHomol<-function(dossiers,fichiers = NULL){

  if(is.null(fichiers))
  {
    fichiers = list.files(full.names = TRUE, path = dossiers,pattern = "^(MA|NEBEF)_SITES_HOMOL_GRD_[0-9A-Z]{16}_([0-9]{14}).csv$")

    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  if(is.null(dossiers))
  {
    dossiers = stringr::str_extract(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
    fichiers = stringr::str_remove(string = fichiers,pattern = '([/]?[^/]+[/]{1})+')
  }

  #Si aucun fichier n'est conforme à la nomenclature alors pas de traitement
  if(!any(stringr::str_detect(string = fichiers,pattern = "^(MA|NEBEF)_SITES_HOMOL_GRD_[0-9A-Z]{16}_([0-9]{14}).csv$")))
  {
    warning("Aucun fichier d'homologation des sites conforme à la nomenclature prévue dans les règles SI MA ou NEBEF. La méthode par défaut (RECTANGLE) sera appliquée à l'ensemble des entités.")
    return(tibble(MECANISME=character(),CODE_SITE=character(),METHODE=character(),DEBUT=as_date(integer()),FIN=as_date(integer()),VARIANTE=character()))

  }else{

    stringr::str_match(string =  fichiers, pattern = "^(MA|NEBEF)_SITES_HOMOL_GRD_[0-9A-Z]{16}_([0-9]{14}).csv$") %>%
      dplyr::as_tibble() %>%
      dplyr::transmute(
        dossier = dossiers
        , fichier = V1
        , mecanisme = V2
        , horodate_creation = lubridate::ymd_hms(V3,tz = 'CET')
      ) %>%
      dplyr::filter(str_detect(string = fichier, pattern = "^(MA|NEBEF)_SITES_HOMOL_GRD_[0-9A-Z]{16}_([0-9]{14}).csv$")) %>% # On ne conserve que les fichiers de listing des entités
      dplyr::arrange(mecanisme, desc(horodate_creation)) %>% # On trie les fichiers par mécanisme, période de validité version (declaratif/final) et horodate de création
      dplyr::distinct(mecanisme, date_validite,.keep_all = TRUE) %>% # On ne conserve que le dernier fichier reçu pour une période donnée (pour NEBEF on ne conserve que le dernier fichier créé)
      {
        purrr::pmap_dfr(
          .l = list(
            x = str_c(.$dossier,.$fichier,sep='')
            , y = .$mecanisme
          )
          , .f = function(x,y,z){
            suppressWarnings(
              readr::read_delim(
                file = x
                , delim = ';'
                , locale = locale(decimal_mark = ',',date_format = '%Y%m%d')
                , comment = '<EOF>'
                , col_types =
                  list(
                    CODE_EXT_SITE = 'c'
                    , DATE_DEBUT_HOMOL_PREV = 'D'
                    , DATE_FIN_HOMOL_PREV = 'D'
                    , DATE_DEBUT_HOMOL_HIST = 'D'
                    , DATE_FIN_HOMOL_HIST = 'D'
                    , VARIANTE_HIST = 'c'
                    , .default = 'i'
                  )
                , skip = 2
              ))
          test2 %>% #On importe le fichier en précisant le séparateur de colonnes, le format des valeurs décimales, le format des colonnes et les lignes à passer en commentaires
              {
                bind_rows(
                  dplyr::transmute(.data = .
                    , CODE_SITE = CODE_EXT_SITE
                    , DEBUT = DATE_DEBUT_HOMOL_PREV
                    , FIN = coalesce(DATE_FIN_HOMOL_PREV,as_date('2099-12-31')) # date de fin exclue en sortie
                  ) %>%
                    dplyr::filter(!is.na(DEBUT)) %>%
                    add_column(METHODE = 'PREVISION',.before = 2) %>%
                    add_column(VARIANTE = NA)
                  , dplyr::transmute(.data = .
                    , CODE_SITE = CODE_EXT_SITE
                    , DEBUT = DATE_DEBUT_HOMOL_HIST
                    , FIN = coalesce(DATE_FIN_HOMOL_HIST,as_date('2099-12-31')) # date de fin exclue en sortie
                    , VARIANTE = VARIANTE_HIST
                  ) %>%
                    dplyr::filter(!is.na(DEBUT)) %>%
                    add_column(METHODE = 'HISTORIQUE',.before = 2)
                )
              } %>% #On renomme la table avec des noms communs aux différents mécanismes
              tibble::add_column(MECANISME = y,.before = 1) #On ajoute la colonne mécanisme
          }
        )
      }
  }
}

LoadIndHist<-function(dirs=""){#on charge le dernier fichier Indispo connu
  lf<-list.files(dossier,pattern="NEBEF_INDHIST_GRD_")
  lf<-lf[regexpr(".csv",lf)>0]
  if(length(lf)>0){
    IndHist1<-list()
    for(f in lf){
      IndHist1[[which(lf==f)]]<-read.csv2(paste(dossier,f,sep="/"),skip=2, stringsAsFactors = FALSE)[,c("CODE_EXT_SITE","DATE_INDISPO")]
    }
    IndHist<-do.call("rbind",IndHist1)
  }else{
    logprint("Pas de fichier 'Indispos'")
    IndHist<-data.frame(CODE_EXT_SITE=NA,DATE_INDISPO=NA)[0,]
  }
  names(IndHist)[names(IndHist)=="CODE_EXT_SITE"]<-"CODE_SITE"
  IndHist$DATE_INDISPO<-format(as.Date(as.character(IndHist$DATE_INDISPO),format="%Y%m%d"),format="%Y-%m-%d")
  return(unique(IndHist))
}

LoadPEIF<-function(dossier=""){}

#' Title
#'
#' @param dossier
#' @param pas
#'
#' @return
#' @export
#' @import tidyverse
#' @examples
LoadPrev<-function(dossier, pas = 600){

  if(dir.exists(dossier))
  {
    #Fichiers de programme d'Effacement Retenus
    lfprev <- list.files(path = dossier, pattern = "_PREV_GRD_[0-9A-Z]{16}_[0-9]{8}_[0-9]{14}.csv", full.names = TRUE)
    lfprev <- file.info(lfprev,extra_cols = TRUE)
    lfprev$Lien = rownames(lfprev)
    rownames(lfprev) <- NULL

    #Horodate de création figurant dans le nom du fichier
    lfprev$HorodateCreation = as.POSIXct(x = gsub(x = lfprev$Lien, pattern = paste(dossier,"_PREV_GRD_[0-9A-Z]{16}_[0-9]{8}_([0-9]{14}).csv", sep="/"), replacement = "\\1",perl = TRUE), format = "%Y%m%d%H%M%S")

    #Journee d'effacement correspondante
    lfprev$JourPrevision = as.Date(x = gsub(x = lfprev$Lien, pattern = paste(dossier,"_PREV_GRD_[0-9A-Z]{16}_([0-9]{8})_[0-9]{14}.csv", sep="/"), replacement = "\\1",perl = TRUE), format = "%Y%m%d")

    #Mécanisme
    lfprev$Mec = substr(gsub(dossier,"",lfprev$Lien),2,4)

    #Tri des fichiers par Jour et horodate de création
    lfprev <- lfprev[order(lfprev$JourPrevision,lfprev$HorodateCreation, decreasing = TRUE),]

    #Dédoublonnage des fichiers de CdC en selectionnant les plus récents par Jour d'effacement
    lfprev <- lfprev[!duplicated(lfprev[,c('JourPrevision','Mec')]),]

    #Fichiers non vides (superieur a 390 octets)
    lfprev <- lfprev[which(lfprev$size > 390),]

    prevData <- data.frame(CODE_ENTITE = NA, CODE_SITE = NA, horodate = NA, horodateutc = NA, puissance = NA)[0,]

    if( nrow(lfprev) > 0 ){

      for(i in 1:nrow(lfprev)){

        prevFile <- lfprev[i,]

        prevDataTemp <- suppressWarnings(read.table(file = prevFile$Lien, skip = 2, fill = TRUE, comment.char = "<", header = TRUE, sep = ';',dec = ',', stringsAsFactors = FALSE))

        for(j in 1:nrow(prevDataTemp))
        {
          #Jour de prévision
          jour <- as.Date.character(x = prevDataTemp$DATE[j], format = "%Y%m%d")

          #Début de la chronique
          deb <- as.POSIXct(x = paste(jour, '00:00:00'))

          #Entité correspondante
          EDE <- as.character(prevDataTemp$CODE_EDE[j])

          Site <- as.character(prevDataTemp$CODE_EXT_SITE[j])

          #Nombre de points de CdC pour ce "jour"
          NbPts <- prevDataTemp$NB_PTS_CHRONIQUE[j]

          #Définition du pas de temps
          pdt <-  switch(EXPR = as.character(NbPts)
                         , '48' = 1800
                         , '50' = 1800
                         , '36' = 1800
                         , '136' = 600
                         , '144' = 600
                         , '150' = 600)

          #Fin de la chronique
          fin <- as.POSIXct(x = paste(jour + 1, '00:00:00')) - pdt

          #création de la chronique
          chron <- seq.POSIXt(from = deb, by = pdt, to = fin)

          chronUTC <- chron
          attr(chronUTC, "tzone") <- "UTC"

          #Valeurs de puissance exprimée en kW (Attention aux chiffres significatifs)
          puissance <- as.numeric(unlist(prevDataTemp[j,paste('VAL',1:NbPts,sep='')], use.names = FALSE))

          tempdata = cbind.data.frame(CODE_ENTITE = EDE, CODE_SITE = Site, horodate = chron, horodateutc = chronUTC, puissance = puissance, stringsAsFactors = FALSE)


          if(pdt > pas)
          {
            tempdata = as.data.frame(apply(tempdata, MARGIN = 2, FUN = function(x){rep(x, each = pdt/pas)}))
            tempdata$horodate = seq.POSIXt(from = deb, by = pas, to = as.POSIXct(x = paste(jour + 1, '00:00:00')) - pas)
            tempdata$horodateutc = tempdata$horodate
            attr(tempdata$horodateutc, "tzone") <- "UTC"
          }

          #Compilation des effacements
          prevData <- rbind.data.frame(prevData, tempdata)

        }

      }
      prevData$puissance<-as.numeric(prevData$puissance)
    }else{

      logprint(paste("Pas de fichier prev dans",dossier))
    }

    return(prevData)

  }else{

    logprint(paste("Dossier",dossier,"introuvable !"))

  }
}

logprint<-function(logText, logFileName = NULL){

  if(is.null(logFileName))
    logFileName = paste0(getwd(), '/Exec-', format(Sys.time(), '%Y%m%d%H%M%S'),'.log')

  write.table(logText, logFileName, append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
}
