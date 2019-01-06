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
#' @param lgl_details un boolen definissant le niveau de détails en sortie ; synthètique par défaut
#'
#' @import tidyverse
#' @import fuzzyjoin
#' @import furrr
#' @return un nested tibble dataframe contenant les chroniques d'effacement associées à chaque programme d'effacement
#' @export
#'
#' @examples
create_ref <- function(tbl_cdc, tbl_sites, tbl_eff, tbl_entt, tbl_homol, tbl_indhist, tbl_prev, lgl_parallel = FALSE, lgl_details = FALSE){

  options(future.globals.maxSize = 4000*1024^2)

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
    return(tibble(MECANISME = character(), CODE_ENTITE = character(), CODE_SITE = character(), CODE_EIC_GRD = character(), TYPE_CONTRAT = character(), DATE = as_date(integer()), HEURE = as_datetime(integer()), CHRONIQUE = double(), PAS = double(), ref = list()))

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
      fuzzy_inner_join(
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
               , data = purrr::pmap(
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
        data = purrr::pmap(
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
        ref = purrr::pmap(
          .l = list(x = data, y = DEBUT, z = FIN, t = MECANISME, u = METHODE, v = DMO)
          , .f = function(x,y,z,t,u,v)
          {
            tibble(
              PERIODE_REFERENCE = do.call(
                args = case_when(
                  #les points sur la demi-heure ronde précédant le délai de mobilisation de l'offre
                  t == 'MA' & u == 'RECTANGLE' ~ list(c(lubridate::floor_date(as_datetime(y) - dseconds(v) - dminutes(30), unit = period(num = 30,units = 'minutes'))%--%lubridate::floor_date(as_datetime(y) - dseconds(v), unit = period(num = 30,units = 'minutes')))) #arrondir au pas 30 minute précédent
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
              dplyr::filter(HORODATE %within% as.list(c(floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes')) - dseconds(1)), int_start(ref[[1]]$PERIODE_REFERENCE)%--%(int_end(ref[[1]]$PERIODE_REFERENCE) - dseconds(1))))) %>%
              select(-ref)
          }
        )
      ) %>%
      #récupération des chroniques réalisées et calcul des chroniques de référence
      mutate(
        data = furrr::future_pmap(
          .progress = TRUE
          , .l = list(ref = ref, ts = data, deb = DEBUT, fin = FIN, meca = MECANISME, meth = METHODE, entt = CODE_ENTITE, site = CODE_SITE, variante = VARIANTE, signe = SIGNE)
          , .f = function(ref, ts, deb, fin, meca, meth, entt, site, variante, signe, prev = tbl_prev)
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
                          dplyr::filter(HORODATE_UTC %within% as.list(floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes')) - dseconds(1)))) %>%
                          add_column(REFERENCE =
                                       mean(subset(ts, HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE[1])%--%(int_end(ref$PERIODE_REFERENCE[1]) - dseconds(1))), PUISSANCE)[[1]])
                                     , .before = 'PAS') %>%
                          transmute(HORODATE, HORODATE_UTC, REALISE = PUISSANCE, REFERENCE, PAS)
                      )
                    #puissance moyenne minimum, resp. maximum, entre la période précédant et la période suivant l'effacement, resp. le report
                    , meth == 'RECTANGLE' & meca == 'NEBEF' ~

                      list(
                        ts %>%
                          dplyr::filter(HORODATE_UTC %within% as.list(floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes')) - dseconds(1)))) %>%
                          add_column(REFERENCE =
                                       signe * min(
                                         signe * mean(subset(ts, HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE[1])%--%(int_end(ref$PERIODE_REFERENCE[1]) - dseconds(1))), PUISSANCE)[[1]])
                                         , signe * mean(subset(ts, HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE[2])%--%(int_end(ref$PERIODE_REFERENCE[2]) - dseconds(1))), PUISSANCE)[[1]])
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
                            , HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE)%--%(int_end(ref$PERIODE_REFERENCE) - dseconds(1)))
                          ) %>%
                          mutate(HORODATE_FUSION = floor_date(HORODATE_UTC, unit = as.period(duration(num = max(unique(PAS), unique(ts$PAS)), units = 'seconds')))) %>%
                          dplyr::full_join(
                            y = ts %>%
                              dplyr::filter(HORODATE_UTC %within% as.list(floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit =  period(num = 30, units = 'minutes')) - dseconds(1)))) %>%
                              mutate(HORODATE_FUSION = floor_date(HORODATE_UTC, unit = as.period(duration(num = max(unique(PAS), unique(prev$PAS)), units = 'seconds'))))
                            , by = c('HORODATE_FUSION')
                          ) %>%
                          transmute(HORODATE = HORODATE.y, HORODATE_UTC = HORODATE_UTC.y, REALISE = PUISSANCE.y, REFERENCE = PUISSANCE.x, PAS = PAS.y)
                      )
                    #puissance moyenne ou médiane, des 4 mêmes jours de semaine éligibles passées ou des 10 derniers jours éligibles passés
                    , meth == 'HISTORIQUE' & meca %in% c('MA','NEBEF') ~

                      list(
                        ts %>%
                          dplyr::filter(HORODATE_UTC %within% as.list(int_start(ref$PERIODE_REFERENCE)%--%(int_end(ref$PERIODE_REFERENCE) - dseconds(1)))
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
                              dplyr::filter(HORODATE_UTC %within% as.list(floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes')) - dseconds(1))))
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

    #"\nCourbe de réalisé ou de référence incomplète -> chronique d'effacement ou report partiellement calculée\n\n"
    cat(
      tbl_refSite %>%
        mutate(
          refIncomp = pmap(
            .l = list(ref = data, deb = as_datetime(DEBUT), fin = as_datetime(FIN))
            , function(ref,deb,fin)
            {
              if(nrow(ref) > 0)
              {
                tibble(
                  horodate_UTC = as_datetime(setdiff(x = seq.POSIXt(from = floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes')), to = (ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes')) - dseconds(unique(ref$PAS))), by =  dseconds(unique(ref$PAS)))
                                                     , y = subset(ref, !is.na(REFERENCE), HORODATE_UTC)[[1]]))
                ) %>%
                  add_column(groupe = "", puissance = 1, dmo = NA_integer_) %>%
                  chron2prog(int_step = unique(ref$PAS)) %>%
                  transmute(PERIODE_MANQUANTE = begin%--%end)

              }else{

                tibble(PERIODE_MANQUANTE = floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes'))))
              }
            }
          )

          , realIncomp = pmap(
            .l = list(ref = data, deb = as_datetime(DEBUT), fin = as_datetime(FIN))
            , function(ref,deb,fin)
            {
              if(nrow(ref) > 0)
              {
                tibble(
                  horodate_UTC = as_datetime(setdiff(x = seq.POSIXt(from = floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes')), to = (ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes')) - dseconds(unique(ref$PAS))), by =  dseconds(unique(ref$PAS)))
                                                     , y = subset(ref, !is.na(REALISE), HORODATE_UTC)[[1]]))
                ) %>%
                  add_column(groupe = "", puissance = 1, dmo = NA_integer_) %>%
                  chron2prog(int_step = unique(ref$PAS)) %>%
                  transmute(PERIODE_MANQUANTE = begin%--%end)

              }else{

                tibble(PERIODE_MANQUANTE = floor_date(as_datetime(deb),unit = period(num = 30, units = 'minutes'))%--%(ceiling_date(as_datetime(fin), unit = period(num = 30, units = 'minutes'))))
              }
            }
          )
        ) %>%
        transmute(
          miss = pmap_chr(
            .l = list(
              x = MECANISME
              , y = CODE_ENTITE
              , z = CODE_SITE
              , r = METHODE
              , s = VARIANTE
              , t = realIncomp
              , u = refIncomp
              , v = DEBUT
              , w = FIN
            )
            , .f = function(x,y,z,r,s,t,u,v,w)
            {
              if_else(
                condition = (nrow(t)+nrow(u)) > 0
                , true = paste0('\nMECANISME\t: ',x,'\nENTITE\t\t: ',y,'\nSITE\t\t: ',z,'\nMETHODE\t\t: ',r, if_else(r == 'HISTORIQUE', paste0('\nVARIANTE\t: ',s),''),'\nPERIODE ATTENDUE\t\t: ',as_datetime(v)%--%as_datetime(w),'\nPERIODE DE REALISE MANQUANTE\t: ',t$PERIODE_MANQUANTE,'\nPERIODE DE REFERENCE MANQUANTE\t: ',u$PERIODE_MANQUANTE,'\n')
                , false = NA_character_
              )
            }
          )
        ) %>%
        {subset(., !is.na(miss), 'miss')[[1]]}
    )

    if(any(map_int(.x = tbl_refSite$data,.f = nrow) > 0))
    {
      #controle du réalisé par entité
      tbl_RefEntt = tbl_refSite %>%
        unnest(data, .preserve = ref) %>%
        mutate(HORODATE = with_tz(HORODATE, 'CET')) %>%
        group_by(MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, HORODATE, HORODATE_UTC, PAS) %>%
        summarise(
          CAPA_MAX_H_ENTITE = sum(CAPA_MAX_H_SITE)
          , ref = if_else(unique(METHODE) == 'RECTANGLE', head(ref, n = 1), list(NULL))
          , REALISE = sum(REALISE, na.rm = TRUE)
          , REFERENCE = sum(REFERENCE, na.rm = TRUE)
        )  %>%
        group_by(MECANISME, CODE_ENTITE, CAPA_MAX_H_ENTITE, METHODE, DEBUT, FIN, SIGNE) %>%
        nest() %>%
        mutate(
          data = purrr::pmap(
            .l = list(meca = MECANISME, entt = CODE_ENTITE, meth = METHODE, signe = SIGNE, ref = data)
            , .f = function(meca, entt, meth, signe, ref, ts = dplyr::filter(tbl_cdc, CODE_ENTITE %in% unique(tbl_refSite$CODE_ENTITE) & CODE_SITE %in% unique(tbl_refSite$CODE_SITE)))
            {

              #affichage des caractéristiques de l'effacement ou report
              message(paste0('\nMECANISME\t: ', meca))
              message(paste0('\nMETHODE\t\t: ',meth))
              message(paste0('\nENTITE\t\t: ',entt))
              #message(paste0('\nPERIODE\t\t: ',as_datetime(deb)%--%as_datetime(fin)))
              message(paste0('\nSENS\t\t: ', if_else(signe < 0, 'REPORT', 'EFFACEMENT')))
              if(meth == 'RECTANGLE'){message(paste0('\nREFERENCE\t: ', paste(ref$ref[[1]]$PERIODE_REFERENCE, collapse = ' '),'\n'))}

              if(nrow(ts) == 0)
              {
                warning(immediate. = TRUE, "courbe de charge manquante -> impossible de calculer la chronique d'effacement ou report")
                tibble(HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), PAS = integer(), ref = list(), REALISE = double(), REFERENCE = double())

              }else{
                mutate(.data = ref,
                       REFERENCE = case_when(

                         #puissance moyenne sur la demi-heure ronde précédant le DMO
                         meth == 'RECTANGLE' & meca == 'MA' ~

                           dplyr::filter(ts, MECANISME == meca & CODE_ENTITE == entt & HORODATE_UTC %within% as.list(ref[[1]]$PERIODE_REFERENCE[1])) %>%
                           dplyr::group_by(MECANISME, CODE_ENTITE, HORODATE, HORODATE_UTC, PAS) %>%
                           dplyr::summarise(PUISSANCE = sum(PUISSANCE, na.rm = TRUE)) %>%
                           ungroup() %>%
                           {mean(select(.,PUISSANCE)[[1]])}

                         #puissance moyenne minimum, resp. maximum, entre la période précédant et la période suivant l'effacement, resp. le report
                         , meth == 'RECTANGLE' & meca == 'NEBEF' ~

                           signe * min(
                             dplyr::filter(ts, MECANISME == meca & CODE_ENTITE == entt & HORODATE_UTC %within% as.list(ref[[1]]$PERIODE_REFERENCE[1])) %>%
                               dplyr::group_by(MECANISME, CODE_ENTITE, HORODATE, HORODATE_UTC, PAS) %>%
                               dplyr::summarise(PUISSANCE = sum(PUISSANCE, na.rm = TRUE)) %>%
                               ungroup() %>%
                               {signe * mean(select(.,PUISSANCE)[[1]])}
                             , dplyr::filter(ts, MECANISME == meca & CODE_ENTITE == entt & HORODATE_UTC %within% as.list(ref[[1]]$PERIODE_REFERENCE[2])) %>%
                               dplyr::group_by(MECANISME, CODE_ENTITE, HORODATE, HORODATE_UTC, PAS) %>%
                               dplyr::summarise(PUISSANCE = sum(PUISSANCE, na.rm = TRUE)) %>%
                               ungroup() %>%
                               {signe * mean(select(.,PUISSANCE)[[1]])}
                           )
                         # pour les autres méthodes agrégation des chroniques de réalisé et de référence des sites composant l'entité
                         , TRUE ~ REFERENCE
                       )
                )
              }
            }
          )
        )

      #recalage des volumes par sites et calcul des chroniques d'effacement au pas 30 minutes
      tbl_refSite =  tbl_refSite %>%
        unnest(.preserve = ref) %>%
        inner_join(
          y = select(unnest(data = tbl_RefEntt), MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, HORODATE, HORODATE_UTC, REFERENCE, REALISE, CAPA_MAX_H_ENTITE)
          , by = c('MECANISME', 'CODE_ENTITE', 'METHODE', 'DEBUT', 'FIN', 'SIGNE', 'HORODATE', 'HORODATE_UTC')
          , suffix = c("","_ENTITE")
        ) %>%
        mutate(
          VOLUME = REFERENCE - REALISE
          , DATE = as_date(HORODATE)
          , HEURE = floor_date(HORODATE, unit = '30 minutes')
        ) %>%
        group_by(MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, HORODATE_UTC) %>%
        mutate(
          VOLUME_ENTITE = REFERENCE_ENTITE - REALISE_ENTITE
          , SOMME_VOLUME = sum(VOLUME)
        ) %>%
        group_by(MECANISME, CODE_ENTITE, METHODE, DEBUT, FIN, SIGNE, DATE, HEURE) %>%
        mutate(
          SOMME_CHRONIQUE = mean(SOMME_VOLUME)
          , CHRONIQUE_ENTITE = if_else(
            condition = SIGNE < 0
            , true =  pmax(pmin(0,mean(VOLUME_ENTITE))
                           , if_else(condition = MECANISME == 'NEBEF' , true = - as.double(CAPA_MAX_H_ENTITE), false = -Inf))
            , false =  pmin(pmax(0,mean(VOLUME_ENTITE)), if_else(condition = MECANISME == 'NEBEF', true = as.double(CAPA_MAX_H_ENTITE), false = Inf))
          )
        ) %>%
        group_by(MECANISME, CODE_ENTITE, CODE_SITE, CODE_EIC_GRD, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DATE, HEURE) %>%
        mutate(

          RATIO = CHRONIQUE_ENTITE/SOMME_CHRONIQUE
          , CHRONIQUE = if_else(
            condition = SIGNE < 0
            , true =  pmin(0,mean(VOLUME))
            , false =  pmax(0,mean(VOLUME))
          ) * if_else(is.nan(RATIO),1,RATIO,1)
        )

      if(lgl_details){

        tbl_refSite %>%
          group_by(MECANISME, CODE_ENTITE, CAPA_MAX_H_ENTITE, CODE_EIC_GRD, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DMO, SIGNE, DEBUT, FIN) %>%
          nest(HORODATE, HORODATE_UTC, REALISE, REFERENCE, VOLUME, REALISE_ENTITE, REFERENCE_ENTITE, VOLUME_ENTITE, PAS, RATIO, DATE, HEURE, CHRONIQUE, ref) %>%
          mutate(
            ref = map(.x = data,.f = ~ tibble(PERIODE_REFERENCE = do.call(args = unique(.$ref), what = 'c')))
            , data = map(.x = data,.f = ~ select(.,- ref) %>% rename(PAS_CHRON = PAS))
            , PAS = 1800
          )

      }else{
        suppressWarnings(
          tbl_refSite %>%
            ungroup() %>%
            distinct(MECANISME, CODE_ENTITE, CODE_SITE, CODE_EIC_GRD, TYPE_CONTRAT, DATE, HEURE, CHRONIQUE, PAS = 1800, ref, .keep_all = FALSE)
        )
      }
    }else{
      warning('Aucune chronique de consommation disponible -> aucune chronique calculée', immediate. = TRUE)
      return(tibble(MECANISME = character(), CODE_ENTITE = character(), CODE_SITE = character(), CODE_EIC_GRD = character(), TYPE_CONTRAT = character(), DATE = as_date(integer()), HEURE = as_datetime(integer()), CHRONIQUE = double(), PAS = double(), ref = list()))
    }
  }
}
