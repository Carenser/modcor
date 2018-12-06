LoadPerimetre<-function(dossier){
  #1b P?rim?tre----
  lf<-list.files(dossier,pattern=".csv",full.names=T)
  lfperimma<-lf[regexpr("MA_REFST_TLRLV_GRD",lf)>0]
  if(length(lfperimma)==0){
    logprint(paste("Pas de p?rim?tre MA dans",dossier,"\n"))
    perimma=data.frame(CODE_ENTITE=NA,CAPA_MAX_H_SITE=NA,TYPE_SITE=NA,TYPE_CONTRAT=NA,ID_SITE=NA,CATEGORIE=NA,mecanisme=NA)[0,]
  }else{
    perimma<-suppressMessages(read_csv2(file = lfperimma[length(lfperimma)], skip = 2, col_names = TRUE, comment = "<EOF>"))
    names(perimma)[names(perimma)=="CODE_EDA"]<-"CODE_ENTITE"
    perimma$mecanisme="MA"
  }
  lfperimnebef<-lf[regexpr("NEBEF_REFST_TLRLV_GRD",lf)>0 | regexpr("NEBEF_REFST_TLRV_GRD",lf)>0]
  if(length(lfperimnebef)==0){
    logprint(paste("Pas de p?rim?tre Nebef dans",dossier,"\n"))
    perimnebef=data.frame(CODE_ENTITE=NA,CAPA_MAX_H_SITE=NA,TYPE_SITE=NA,TYPE_CONTRAT=NA,ID_SITE=NA,CATEGORIE=NA,mecanisme=NA)[0,]
  }else{
    perimnebef<-suppressMessages(read_csv2(file = lfperimnebef[length(lfperimnebef)], skip = 2, col_names = TRUE, comment = "<EOF>"))
    names(perimnebef)[names(perimnebef)=="CODE_EDE"]<-"CODE_ENTITE"
    perimnebef$mecanisme="NEBEF"
  }
  names<-c("CODE_ENTITE","TYPE_SITE","ID_SITE","CAPA_MAX_H_SITE","TYPE_CONTRAT","CATEGORIE","mecanisme")
  perims<-rbind(perimnebef[,names],perimma[,names])
  perims<-perims[perims$CODE_ENTITE!="",]
  perims$CAPA_MAX_H_SITE<-as.numeric(perims$CAPA_MAX_H_SITE)
  perims$CODE_SITE<-paste(perims$TYPE_SITE,perims$ID_SITE,sep="")
  logprint(paste(nrow(perims),"sites inclus dans le perimetre"))
  return(perims[,names(perims)[!names(perims) %in% c("TYPE_SITE","ID_SITE")]])
}

#' Chargement des fichiers de programme d'effacements retenus et d'ordres d'activations consolidés en J+3 aux formats prévues dans les règles SI décrivant les flux en provenance de RTE à destination des GRD
#'
#' @param dossier le nom du répertoire contenant les fichiers passés en paramètre
#'
#' @return un dataframe comprenant 5 colonnes : CODE_ENTITE, DEBUT, FIN, SIGNE, DMO
#' @export
#' @import reshape2
#' @import tidyverse
#' @examples

LoadEffacements <- function(fichiers = NULL, dossiers)
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
                filter(SIGNE !=0) #On supprime les OA désactivés par contrepassation
              
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
#' @param dossier le nom du répertoire contenant les fichiers passés en paramètre (facultatif)
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

LoadListeEntt<-function(dossier=""){
  for(type in c("A","E")){
    lfle<-list.files(dossier,pattern=paste0("LISTE_ED",type,"_GRD_"))
    lfle<-lfle[regexpr(".csv",lfle)>0]
    if(length(lfle)>0){
      suppressWarnings(ListeEntt1<-read.csv2(paste(dossier,lfle[length(lfle)],sep="/"), stringsAsFactors = FALSE))
      names(ListeEntt1)[names(ListeEntt1)==paste0("CODE_ED",type)]<-"CODE_ENTITE"
      ListeEntt1<-ListeEntt1[,c("CODE_ENTITE","METHODE_CONTROLE_REALISE")]
      if(exists("ListeEntt"))ListeEntt<-rbind(ListeEntt,ListeEntt1)else ListeEntt<-ListeEntt1
    }else{
      logprint(paste0("Fichier des ED",type," actives non trouve. La methode du rectangle sera appliquee a toutes les ED",type))
    }
  }
  if(!exists("ListeEntt"))ListeEntt<-data.frame(CODE_ENTITE=NA,METHODE_CONTROLE_REALISE=NA)[0,]
  return(ListeEntt)
}

LoadSitesHomol<-function(dossier=""){
  lfsh<-list.files(dossier,pattern="_SITES_HOMOL_GRD_")
  lfsh<-lfsh[regexpr(".csv",lfsh)>0]
  lfsh<-lfsh[file.info(paste(dossier,lfsh,sep="/"))$size>170]
  sh<-list()
  for(f in lfsh){
    sh[[length(sh)+1]]<-read.csv2(paste(dossier,f,sep="/"),skip=2, stringsAsFactors = FALSE)
  }
  if(length(sh)>0){
    shh<-do.call("rbind",sh)
    shh<-shh[shh$CODE_EXT_SITE!="<EOF>",]
    shh<-shh[!duplicated(shh[,c("CODE_EXT_SITE","VARIANTE_HIST")]),]
  }else{
    logprint("Pas de fichier contenant les variantes retenues pour la methode par historique")
    SitesHomol<-data.frame(CODE_EXT_SITE=NA,VARIANTE_HIST=NA)[0,]
  }
  names(SitesHomol)[names(SitesHomol)=="CODE_EXT_SITE"]<-"CODE_SITE"
  return(SitesHomol[,c("CODE_SITE","VARIANTE_HIST")])
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
    
    #M?canisme
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
