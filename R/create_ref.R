#' Fonction de calcul des chroniques de référence
#'
#' @param tbl_cdc un tibble dataframe contenant les chroniques réalisées par site et entité
#' @param tbl_sites un tibble dataframe contenant les périmètres MA/NEBEF
#' @param tbl_eff un tibble dataframe contenant les effacements réalisés pour MA/NEBEF
#' @param tbl_entt un tibble dataframe contenant la liste des entités et leurs méthodes respectives
#' @param tbl_homol un tibble dataframe contenant la liste des sites homologués aux méthodes HISTORIQUE et PREVISION
#' @param tbl_indhist un tibble dataframe contenant les jours d'indisponibilité à la méthode HISTORIQUE par site et entité
#' @param tbl_prev un tibble dataframe contenant les chroniques de prévision par site et entité pour la méthode PREVISION
#' @param lgl_parallel un booleen définissant le mode de calcul ; séquentiel par défaut
#'
#' @import tidyverse
#' @import fuzzyjoin
#' @import furrr
#' @return un nested tibble dataframe contenant les chroniques d'effacement associées à chaque programme d'effacement
#' @export
#'
#' @examples
create_ref <- function(tbl_cdc, tbl_sites, tbl_eff, tbl_entt, tbl_homol, tbl_indhist, tbl_prev, lgl_parallel = FALSE){

  options(future.globals.maxSize = 2500*1024^2)

  if(lgl_parallel)
  {
    if(Sys.info()["sysname"] == "Linux")
    {
      plan(multiprocess)
    }

    if(Sys.info()["sysname"] == "Windows")
    {
      plan(multisession)
    }
  }else{

    plan(sequential)
  }

  if(nrow(tbl_eff)==0){

    warning('Aucune entité activée au cours de la période de calcul')
    return(
      tibble(
        MECANISME = character()
        , CODE_ENTITE = character()
        , CODE_SITE = character()
        , DATE = as_date(integer())
        , HEURE = as_datetime(integer())
        , CHRONIQUE = double()
        , PAS = integer()
      )
    )

  }else{

    #récupération de la méthode de certification associée à un effacement
    tbl_refSite =
      fuzzy_left_join(
        x = mutate(tbl_eff, DATE = as_date(DEBUT, tz='CET'))
        , y = tbl_entt
        , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
        , match_fun = list(`==`,`==`,`>=`,`<`)
      ) %>%
      #application de la méthode du rectangle par défaut
      replace_na(list(METHODE = 'RECTANGLE')) %>%
      transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO) %>%
      #ajout des sites rattachés aux entités
      fuzzy_left_join(
        y = tbl_sites
        , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
        , match_fun = list(`==`,`==`,`>=`,`<`)
      ) %>%
      transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO, CODE_SITE, CODE_EIC_GRD, CAPA_MAX_H_SITE, TYPE_CONTRAT) %>%
      # Un site ne peut être rattaché à une entité certifiée par des méthodes autres que RECTANGLE que s'il est homologué à ces méthodes
      fuzzy_left_join(
        y = tbl_homol
        , by = c('MECANISME','CODE_SITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
        , match_fun = list(`==`,`==`,`>=`,`<`)
      ) %>%
      dplyr::filter(!(METHODE.x != 'RECTANGLE' & METHODE.y != METHODE.x)) %>%
      transmute(
        MECANISME = MECANISME.x
        , CODE_ENTITE
        , CODE_SITE = CODE_SITE.x
        , CODE_EIC_GRD
        , CAPA_MAX_H_SITE
        , TYPE_CONTRAT
        , METHODE = METHODE.x
        #si la méthode est HISTORIQUE, la variante par défaut est MOY10J (la plus répandue ; utile ?)
        , VARIANTE = if_else(condition = METHODE.x == 'HISTORIQUE' & is.na(VARIANTE), true = 'MOY10J', false =  VARIANTE)
        , DEBUT = DEBUT.x
        , FIN = FIN.x
        , SIGNE
        , DMO
      ) %>%
      distinct() %>% #sites homologués à d'autres méthodes que le rectangle (utile ?)
      #prise en compte des périodes d'indisponibilité pour la méthode HISTORIQUE
      fuzzyjoin::fuzzy_left_join(
        y = tbl_indhist
        , by = c('MECANISME','CODE_ENTITE','CODE_SITE','DEBUT' = 'DATE')
        , match_fun = list(`==`,`==`,`==`,`>=`)
      ) %>%
      transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x,CODE_SITE = CODE_SITE.x, CODE_EIC_GRD, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO, DATE_INDHIST = DATE) %>%
      group_by(MECANISME, CODE_ENTITE, CODE_SITE, CODE_EIC_GRD, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO) %>%
      nest() %>%
      #ajout des journées d'effacement ou report passées du site dans l'historique d'indisponibilité pour la méthode HISTORIQUE
      {
        mutate(.data = .
               , data = pmap(
                 .l = list(x = data, y = CODE_SITE, z = METHODE, t = DEBUT)
                 , .f = function(x, y , z, t, u = transmute(., CODE_SITE, DATE_INDHIST = as_date(DEBUT, tz = 'CET')))
                 {
                   if(z == 'HISTORIQUE')
                   {
                     x %>%
                       bind_rows(subset(u, CODE_SITE == y & DATE_INDHIST < as_date(t), DATE_INDHIST)) %>%
                       distinct()
                   }else{tibble(DATE_INDHIST = as_date(NA))}
                 }
               )
        )
      } %>%
      #identification des journées utilisées comme historique pour la méthode HISTORIQUE et suppression des périodes d'indisponibilité pour les autres méthodes (gain en espace)
      mutate(
        data = furrr::future_pmap(
          .l = list(x = data, y = DEBUT, z = METHODE, t = VARIANTE)
          , .f = function(x,y,z,t)
          {
            tibble(
              DATE_HIST = as_date(
                unlist(
                  case_when(
                    z == 'HISTORIQUE' & t %in% c('MOY4S','MED4S') ~ list(as_date(setdiff(x = seq.Date(from = as_date(as_datetime(y)) - weeks(1), length.out = 52, by = '-1 week'), y = as_date(x$DATE_INDHIST))[1:4]))
                    , z == 'HISTORIQUE' & t %in% c('MOY10J','MED10J') ~ list(as_date(setdiff(x = seq.Date(from = as_date(as_datetime(y)) - days(1), length.out = 365, by = '-1 day'), y = as_date(x$DATE_INDHIST))[1:10]))
                    , TRUE ~ list(as_date(NA))
                  )
                )
              )
            )
          }
        )
      ) %>%
      #identification des périodes de référence selon les méthodes
      mutate(
        ref = furrr::future_pmap(
          .l = list(x = data, y = DEBUT, z = FIN, t = MECANISME, u = METHODE, v = DMO)
          , .f = function(x,y,z,t,u,v)
          {
            tibble(
              PERIODE_REFERENCE = do.call(
                args = case_when(
                  #les points sur la demi-heure ronde précédant le délai de mobilisation de l'offre
                  t == 'MA' & u == 'RECTANGLE' ~ list(c(lubridate::floor_date(as_datetime(y) - dseconds(v) - dminutes(30), unit = '30 minutes')%--%lubridate::floor_date(as_datetime(y) - dseconds(v), unit = '30 minutes'))) #arrondir au pas 30 minute précédent
                  #les points précédant et suivant l'effacement sur une période égale au minimum entre la durée de l'effacement et 2 heures (cas des entités télérelevées)
                  , t == 'NEBEF' & u == 'RECTANGLE' ~ list(c((as_datetime(y) - min(as.duration(as_datetime(z) - as_datetime(y)), dhours(2)))%--%as_datetime(y),as_datetime(z)%--%(as_datetime(z) + min(as.duration(as_datetime(z) - as_datetime(y)), dhours(2))))) #arrondir au pas 30 minute précédent
                  #les points de la chronique de prévision portant sur la période d'effacement ou de report
                  , u == 'PREVISION' ~ list(c(as_datetime(y)%--%as_datetime(z)))
                  #les points passés selon la variante, 4 même jours de semaine éligibles ou 10 derniers jours éligibles, aux mêmes heures que celles de la période d'effacement ou report
                  , u == 'HISTORIQUE' ~ list(c((as_date(x$DATE_HIST) + dhours(hour(as_datetime(y))) + dminutes(minute(as_datetime(y))))%--%(as_date(x$DATE_HIST) + dhours(hour(as_datetime(z))) + dminutes(minute(as_datetime(z))))))

                  , TRUE ~ list(c(as.interval(NA)))
                )
                , what = 'c'
              )
            )
          }
        )
      ) %>%
      #Ajout des chroniques de réalisé des sites
      left_join(
        y = tbl_cdc
        , by = c('MECANISME','CODE_ENTITE','CODE_SITE')
      ) %>%
      group_by(MECANISME, CODE_ENTITE, CODE_SITE, CODE_EIC_GRD, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO) %>%
      nest(HORODATE, HORODATE_UTC, PUISSANCE, PAS, ref) %>%
      mutate(
        ref = map(.x = .$data, ~ do.call(c,head(.$ref,n=1)))
        , data = pmap(
          .l = list(ts = data, deb=DEBUT, fin=FIN)
          , .f = function(ts,deb,fin){
            ts %>%
              dplyr::filter(HORODATE %within% as.list(c(as_datetime(deb)%--%(as_datetime(fin) - dseconds(unique(PAS))), int_start(ref[[1]]$PERIODE_REFERENCE)%--%(int_end(ref[[1]]$PERIODE_REFERENCE) - dseconds(unique(PAS)))))) %>%
              select(-ref)
          }
        )
      ) %>%
      #récupération des chroniques réalisées et calcul des chroniques de référence
      mutate(
        data = furrr::future_pmap(
          .progress = TRUE
          , .l = list(ref = ref, ts = data, deb = DEBUT, fin = FIN, meca = MECANISME, meth = METHODE, entt = CODE_ENTITE, site = CODE_SITE, variante = VARIANTE, signe = SIGNE)
          , .f = function(ref, ts, deb, fin, meca, meth, entt, site, variante, signe, prev = tbl_prev, pas = unique(tbl_cdc$PAS))
          {
            #affichage des caractéristiques de l'effacement ou report
            message(paste0('\nPERIODE\t\t: ', as_datetime(deb)%--%as_datetime(fin)))
            message(paste0('\nMECANISME\t: ', meca))
            message(paste0('\nMETHODE\t\t: ',meth))
            message(paste0('\nENTITE\t\t: ',entt))
            message(paste0('\nSITE\t\t: ',site))
            if(meth == 'HISTORIQUE'){message(paste0(str_c('\nVARIANTE\t: ',variante)))}
            message(paste0('\nSENS\t\t: ', if_else(signe < 0, 'REPORT', 'EFFACEMENT')))
            message(paste0('\nREFERENCE\t: ', paste(ref$PERIODE_REFERENCE, collapse = ' '),'\n'))

            if(nrow(ts) == 0)
            {
              warning(immediate. = TRUE, "courbe de charge manquante -> impossible de calculer la chronique d'effacement ou report")
              tibble(HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), REALISE = double(), REFERENCE = double(), PAS = integer())

            }else{

              as.tibble(
                do.call(
                  args = case_when(
                    #puissance moyenne sur la demi-heure ronde précédant le DMO
                    meth == 'RECTANGLE' & meca == 'MA' ~

                      list(
                        ts %>%
                          dplyr::filter(HORODATE_UTC %within% as.list(as_datetime(deb)%--%(as_datetime(fin) - dseconds(pas)))) %>%
                          add_column(REFERENCE =
                                       mean(subset(ts, HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE[1])%--%(int_end(ref$PERIODE_REFERENCE[1]) - dseconds(pas))), PUISSANCE)[[1]])
                                     , .before = 'PAS') %>%
                          transmute(HORODATE, HORODATE_UTC, REALISE = PUISSANCE, REFERENCE, PAS)
                      )
                    #puissance moyenne minimum, resp. maximum, entre la période précédant et la période suivant l'effacement, resp. le report
                    , meth == 'RECTANGLE' & meca == 'NEBEF' ~

                      list(
                        ts %>%
                          dplyr::filter(HORODATE_UTC %within% as.list(as_datetime(deb)%--%(as_datetime(fin) - dseconds(pas)))) %>%
                          add_column(REFERENCE =
                                       signe * min(
                                         signe * mean(subset(ts, HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE[1])%--%(int_end(ref$PERIODE_REFERENCE[1]) - dseconds(pas))), PUISSANCE)[[1]])
                                         , signe * mean(subset(ts, HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE[2])%--%(int_end(ref$PERIODE_REFERENCE[2]) - dseconds(pas))), PUISSANCE)[[1]])
                                       )
                                     , .before = 'PAS'
                          ) %>%
                          transmute(HORODATE, HORODATE_UTC, REALISE = PUISSANCE, REFERENCE, PAS)
                      )
                    #chronique de prévision transmise par l'opérateur d'effacement
                    , meth == 'PREVISION' & meca %in% c('MA','NEBEF') ~

                      list(
                        prev %>%
                          dplyr::filter(
                            MECANISME == meca
                            , CODE_ENTITE == entt
                            , CODE_SITE == site
                            , HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE)%--%(int_end(ref$PERIODE_REFERENCE) - dseconds(pas)))
                          ) %>%
                          dplyr::full_join(
                            y = mutate(
                              ts %>%
                                dplyr::filter(HORODATE_UTC %within% as.list(as_datetime(deb)%--%(as_datetime(fin) - dseconds(pas))))
                              , HORODATE_UTC_FUSION =
                                if_else(
                                  condition = unique(prev$PAS) > PAS
                                  , true = as_datetime(unique(prev$PAS) * as.numeric(HORODATE_UTC)%/%unique(prev$PAS), tz = 'UTC', origin = origin)
                                  , false = HORODATE_UTC
                                )
                            )
                            , by = c('HORODATE_UTC'='HORODATE_UTC_FUSION')
                          ) %>%
                          transmute(HORODATE = HORODATE.y, HORODATE_UTC = HORODATE_UTC.y, REALISE = PUISSANCE.y, REFERENCE = PUISSANCE.x, PAS = PAS.y)
                      )
                    #puissance moyenne ou médiane, des 4 mêmes jours de semaine éligibles passées ou des 10 derniers jours éligibles passés
                    , meth == 'HISTORIQUE' & meca %in% c('MA','NEBEF') ~

                      list(
                        ts %>%
                          dplyr::filter(HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE)%--%(int_end(ref$PERIODE_REFERENCE) - dseconds(pas)))
                          ) %>%
                          mutate(HORODATE_UTC = floor_date(as_datetime(deb), 'day') + dhours(hour(HORODATE_UTC)) + dminutes(minute(HORODATE_UTC))) %>%
                          dplyr::group_by(HORODATE_UTC) %>%
                          summarise(
                            REFERENCE = case_when(
                              variante %in% c('MOY10J','MOY4S') ~ mean(PUISSANCE)
                              , variante %in% c('MED10J','MED4S') ~ median(PUISSANCE)
                              , TRUE ~ NA_real_
                            )
                          ) %>%
                          dplyr::ungroup() %>%
                          dplyr::full_join(
                            y = ts %>%
                              dplyr::filter(HORODATE_UTC %within% as.list(as_datetime(deb)%--%(as_datetime(fin) - dseconds(pas))))
                            , by = c('HORODATE_UTC')
                          ) %>%
                          transmute(HORODATE, HORODATE_UTC, REALISE = PUISSANCE, REFERENCE, PAS)
                      )

                    , TRUE ~ list(tibble(HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), REALISE = double(), REFERENCE = double(), PAS = integer()))
                  )
                  , what = 'cbind'
                )
              )
            }
          }
        )
      )

    # warning(
    #   paste(
    #     capture.output(
    #       {
    #         cat("Courbe de réalisé ou de référence incomplète -> chronique d'effacement ou report partiellement calculée")
    #         cat(
    #           tbl_refSite %>%
    #             mutate(
    #               refIncomp = pmap(
    #                 .l = list(ref = data, deb = DEBUT, fin = FIN)
    #                 , function(ref,deb,fin)
    #                 {
    #                   if(length(ref) > 0)
    #                   {
    #                     tibble(
    #                       horodate_UTC = setdiff(x = seq.POSIXt(from = as_datetime(deb), to = (as_datetime(fin) - dseconds(unique(ref$PAS))), by =  dseconds(unique(ref$PAS)))
    #                                              , y = subset(ref, !is.na(REFERENCE), HORODATE)[[1]])
    #                     ) %>%
    #                       add_column(groupe = NA_character_, puissance = 0, dmo = NA_integer_) %>%
    #                       chron2prog(int_step = unique(ref$PAS)) %>%
    #                       transmute(PERIODE_MANQUANTE = begin%--%end)
    #
    #                   }else{
    #                     tibble(PERIODE_MANQUANTE = as_datetime(deb)%--%as_datetime(fin))
    #                   }
    #                 }
    #               )
    #
    #               , realIncomp = pmap(
    #                 .l = list(ref = data, deb = DEBUT, fin = FIN)
    #                 , function(ref,deb,fin)
    #                 {
    #                   if(length(ref) > 0)
    #                   {
    #                     tibble(
    #                       horodate_UTC = setdiff(x = seq.POSIXt(from = as_datetime(deb), to = (as_datetime(fin) - dseconds(unique(ref$PAS))), by =  dseconds(unique(ref$PAS)))
    #                                              , y = subset(ref, !is.na(REALISE), HORODATE)[[1]])
    #                     ) %>%
    #                       add_column(groupe = NA_character_, puissance = 0, dmo = NA_integer_) %>%
    #                       chron2prog(int_step = unique(ref$PAS)) %>%
    #                       transmute(PERIODE_MANQUANTE = begin%--%end)
    #
    #                   }else{
    #                     tibble(PERIODE_MANQUANTE = as_datetime(deb)%--%as_datetime(fin))
    #                   }
    #                 }
    #               )
    #             ) %>%
    #             transmute(
    #               message = pmap_chr(
    #                 .l = list(
    #                   x = MECANISME
    #                   , y = CODE_ENTITE
    #                   , z = CODE_SITE
    #                   , t = realIncomp
    #                   , u = refIncomp
    #                 )
    #                 , .f = function(x,y,z,t,u)
    #                 {
    #                   if_else(
    #                     condition = (nrow(t)+nrow(u)) > 0
    #                     , true = paste0('\nMECANISME\t: ',x,'\nENTITE\t\t: ',y,'\nSITE\t\t: ',z,'\nPERIODE DE REALISE MANQUANTE\t: ',t$PERIODE_MANQUANTE,'\nPERIODE DE REFERENCE MANQUANTE\t: ',u$PERIODE_MANQUANTE,'\n')
    #                     , false = ''
    #                   )
    #                 }
    #               )
    #             ) %>%
    #             dplyr::filter(nchar(message)>0) %>%
    #             unlist()
    #         )
    #       }
    #     )
    #     , collapse = '\n')
    #   , immediate. = TRUE
    # )

    #controle du réalisé par entité
    tbl_RefEntt = tbl_refSite %>%
      unnest(data, .preserve = ref) %>%
      group_by(MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, HORODATE, HORODATE_UTC, PAS) %>%
      summarise(
        CAPA_MAX_H_ENTITE = sum(CAPA_MAX_H_SITE)
        , ref = if_else(unique(METHODE) == 'RECTANGLE', head(ref, n = 1), list(NULL))
        , REALISE = sum(REALISE, na.rm = TRUE)
        , REFERENCE = sum(REFERENCE, na.rm = TRUE)
      )  %>%
      mutate(
        REFERENCE = furrr::future_pmap_dbl(
          .progress = TRUE
          , .l = list(meca = MECANISME, entt = CODE_ENTITE, meth = METHODE, signe = SIGNE, ref = ref)
          , .f = function(meca, entt, meth, signe, ref, ts = dplyr::filter(tbl_cdc, CODE_ENTITE %in% unique(tbl_refSite$CODE_ENTITE) & CODE_SITE %in% unique(tbl_refSite$CODE_SITE)))
          {
            #affichage des caractéristiques de l'effacement ou report
            message(paste0('\nMECANISME\t: ', meca))
            message(paste0('\nMETHODE\t\t: ',meth))
            message(paste0('\nENTITE\t\t: ',entt))
            message(paste0('\nSENS\t\t: ', if_else(signe < 0, 'REPORT', 'EFFACEMENT')))
            if(meth == 'RECTANGLE'){message(paste0('\nREFERENCE\t: ', paste(ref$PERIODE_REFERENCE, collapse = ' '),'\n'))}

            case_when(

              #puissance moyenne sur la demi-heure ronde précédant le DMO
              meth == 'RECTANGLE' & meca == 'MA' ~

                dplyr::filter(ts, MECANISME == meca & CODE_ENTITE == entt & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[1])) %>%
                dplyr::group_by(MECANISME, CODE_ENTITE, HORODATE, HORODATE_UTC, PAS) %>%
                dplyr::summarise(PUISSANCE = sum(PUISSANCE, na.rm = TRUE)) %>%
                ungroup() %>%
                {mean(select(.,PUISSANCE)[[1]])}

              #puissance moyenne minimum, resp. maximum, entre la période précédant et la période suivant l'effacement, resp. le report
              , meth == 'RECTANGLE' & meca == 'NEBEF' ~

                signe * min(
                  dplyr::filter(ts, MECANISME == meca & CODE_ENTITE == entt & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[1])) %>%
                    dplyr::group_by(MECANISME, CODE_ENTITE, HORODATE, HORODATE_UTC, PAS) %>%
                    dplyr::summarise(PUISSANCE = sum(PUISSANCE, na.rm = TRUE)) %>%
                    ungroup() %>%
                    {signe * mean(select(.,PUISSANCE)[[1]])}
                  , dplyr::filter(ts, MECANISME == meca & CODE_ENTITE == entt & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[2])) %>%
                    dplyr::group_by(MECANISME, CODE_ENTITE, HORODATE, HORODATE_UTC, PAS) %>%
                    dplyr::summarise(PUISSANCE = sum(PUISSANCE, na.rm = TRUE)) %>%
                    ungroup() %>%
                    {signe * mean(select(.,PUISSANCE)[[1]])}
                )
              # pour les autres méthodes agrégation des chroniques de réalisé et de référence des sites composant l'entité
              , TRUE ~ REFERENCE
            )
          }
        )
      )

    tbl_refSite %>%
      unnest(.preserve = ref) %>%
      inner_join(
        y = select(tbl_RefEntt, MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, HORODATE, HORODATE_UTC, REFERENCE, REALISE, CAPA_MAX_H_ENTITE)
        , by = c('MECANISME', 'CODE_ENTITE', 'METHODE', 'DEBUT', 'FIN', 'SIGNE', 'HORODATE', 'HORODATE_UTC')
        , suffix = c("","_ENTITE")
      ) %>%
      mutate(
        VOLUME = if_else(
          condition = SIGNE < 0
          , true =  pmin(0,(REFERENCE - REALISE))
          , false =  pmax(0,(REFERENCE - REALISE))
        )
        #recalage du volume d'effacement sur la capacité maximale de l'entité
        , DATE = as_date(HORODATE)
        , HEURE = floor_date(HORODATE, unit = '30 minutes')

      ) %>%
      group_by(MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, DATE, HEURE) %>%
      mutate(
        VOLUME_ENTITE = if_else(
          condition = SIGNE < 0
          , true =  pmax(pmin(0,sum(REFERENCE_ENTITE - REALISE_ENTITE))
                         , if_else(condition = MECANISME == 'NEBEF' , true = - as.double(CAPA_MAX_H_ENTITE), false = -Inf))
          , false =  pmin(pmax(0,sum(REFERENCE_ENTITE - REALISE_ENTITE)), if_else(condition = MECANISME == 'NEBEF', true = as.double(CAPA_MAX_H_ENTITE), false = Inf))
        )
        , RATIO = (VOLUME_ENTITE/n_distinct(CODE_SITE))/sum(VOLUME)
      ) %>%
      group_by(MECANISME, CODE_ENTITE, CODE_SITE, CODE_EIC_GRD, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DATE, HEURE) %>%
      mutate(CHRONIQUE = mean(VOLUME) * RATIO) %>%
      # group_by(MECANISME, CODE_ENTITE, CAPA_MAX_H_ENTITE, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DMO, SIGNE, DEBUT, FIN) %>%
      # nest(HORODATE, HORODATE_UTC, REALISE, REFERENCE, REALISE_ENTITE, REFERENCE_ENTITE, RATIO, DATE, HEURE, CHRONIQUE)
      ungroup() %>%
      dplyr::filter(TYPE_CONTRAT == 'CARD') %>%
      distinct(MECANISME, CODE_ENTITE, CODE_SITE, CODE_EIC_GRD, DATE, HEURE, CHRONIQUE, PAS = 1800, .keep_all = FALSE)
  }
}
