#' Title
#'
#' @param tbl_cdc
#' @param tbl_sites
#' @param tbl_eff
#' @param tbl_entt
#' @param tbl_homol
#' @param tbl_indhist
#' @param tbl_prev
#'
#' @import tidyverse
#' @import fuzzyjoin
#' @return
#' @export
#'
#' @examples
create_ref <- function(tbl_cdc, tbl_sites, tbl_eff, tbl_entt, tbl_homol, tbl_indhist, tbl_prev){

  if(nrow(tbl_eff)==0){

    warning('Aucune entité activée au cours de la période de calcul')
    return(
      tibble(
        MECANISME = character()
        , CODE_ENTITE = character()
        , CODE_SITE = character()
        , HORODATE_UTC = as_datetime(integer())
        , HORODATE = as_datetime(integer())
        , PUISSANCE = double()
        , REFERENCE = double()
        , SIGNE = integer()
      )
    )

  }else{

    #récupération de la méthode de certification associée à un effacement
    # tbl_cdcRef =
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
      transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT) %>%
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
      transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x,CODE_SITE = CODE_SITE.x, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO, DATE_INDHIST = DATE) %>%
      group_by(MECANISME, CODE_ENTITE, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO) %>%
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
        data = pmap(
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
        data = purrr::pmap(
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
      #récupération des chroniques réalisées et calcul des chroniques de référence
      mutate(
        data = purrr::pmap(
          .l = list(ref = data, deb = DEBUT, fin = FIN, meca = MECANISME, meth = METHODE, entt = CODE_ENTITE, site = CODE_SITE, variante = VARIANTE, signe = SIGNE)
          , .f = function(ref, deb, fin, meca, meth, entt, site, variante, signe, ts = tbl_cdc, prev = tbl_prev)
          {

            #affichage des caractéristiques de l'effacement ou report

            cat(paste0('\nPERIODE\t: ', as_datetime(deb)%--%as_datetime(fin)))
            cat(paste0('\nMECANISME\t: ', meca))
            cat(paste0('\nMETHODE\t: ',meth))
            cat(paste0('\nENTITE\t: ',entt))
            cat(paste0('\nSITE\t: ',site))
            cat(paste0(if_else(meth == 'HISTORIQUE', str_c('\nVARIANTE\t: ',variante),'')))
            cat(paste0('\nSENS\t: ', if_else(signe < 0, 'REPORT', 'EFFACEMENT')))
            print(paste0('\nPERIODE DE REFERENCE\t:', paste(ref, collapse = '\t')))

            if(
              nrow(ts %>%
                   dplyr::filter(
                     MECANISME == meca
                     , CODE_ENTITE == entt
                     , CODE_SITE == site
                     , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
                   )) == 0)
            {
              warning("courbe de charge manquante : impossible de calculer la chronique d'effacement ou report")
              # warning(
              #           paste(
              #             capture.output(
              #               {
              #                 cat("Courbe(s) de charge manquante(s)\n impossible de calculer les chroniques d'effacement ou report suivants :\n")
              #                 # print(., len = nrow(.))
              #
              #               }
              #             )
              #             , collapse = '\n')
              #         )
              tibble(MECANISME = character(), CODE_ENTITE = character(), CODE_SITE = character(), HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), REALISE = double(), REFERENCE = double(), PAS = integer())

            }else{

              as.tibble(
                do.call(
                  args = case_when(
                    #puissance moyenne sur la demi-heure ronde précédant le DMO
                    meth == 'RECTANGLE' & meca == 'MA' ~

                      list(
                        ts %>%
                          dplyr::filter(
                            MECANISME == meca
                            , CODE_ENTITE == entt
                            , CODE_SITE == site
                            , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
                          ) %>%
                          add_column(REFERENCE =
                                       mean(subset(ts, MECANISME == meca & CODE_ENTITE == entt & CODE_SITE == site & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[1]), PUISSANCE)[[1]])
                                     , .before = 'PAS') %>%
                          rename(REALISE = PUISSANCE)
                      )
                    #puissance moyenne minimum, resp. maximum, entre la période précédant et la période suivant l'effacement, resp. le report
                    , meth == 'RECTANGLE' & meca == 'NEBEF' ~

                      list(
                        ts %>%
                          dplyr::filter(
                            MECANISME == meca
                            , CODE_ENTITE == entt
                            , CODE_SITE == site
                            , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
                          ) %>%
                          add_column(REFERENCE =
                                       signe * min(
                                         signe * mean(subset(ts, MECANISME == meca & CODE_ENTITE == entt & CODE_SITE == site & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[1]), PUISSANCE)[[1]])
                                         , signe * mean(subset(ts, MECANISME == meca & CODE_ENTITE == entt & CODE_SITE == site & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[2]), PUISSANCE)[[1]])
                                       )
                                     , .before = 'PAS'
                          ) %>%
                          rename(REALISE = PUISSANCE)
                      )
                    #chronique de prévision transmise par l'opérateur d'effacement
                    , meth == 'PREVISION' & meca %in% c('MA','NEBEF') ~

                      list(
                        prev %>%
                          dplyr::filter(
                            MECANISME == meca
                            , CODE_ENTITE == entt
                            , CODE_SITE == site
                            , HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE)
                          ) %>%
                          dplyr::full_join(
                            y = mutate(
                              ts %>%
                                dplyr::filter(
                                  MECANISME == meca
                                  , CODE_ENTITE == entt
                                  , CODE_SITE == site
                                  , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
                                )
                              , HORODATE_UTC_FUSION =
                                if_else(
                                  condition = unique(prev$PAS) > PAS
                                  , true = as_datetime(unique(prev$PAS) * as.numeric(HORODATE_UTC)%/%unique(prev$PAS), tz = 'UTC', origin = origin)
                                  , false = HORODATE_UTC
                                )
                            )
                            , by = c('MECANISME','CODE_ENTITE','CODE_SITE','HORODATE_UTC'='HORODATE_UTC_FUSION')
                          ) %>%
                          transmute(MECANISME, CODE_ENTITE,CODE_SITE, HORODATE = HORODATE.y, HORODATE_UTC = HORODATE_UTC.y, REALISE = PUISSANCE.y, REFERENCE = PUISSANCE.x, PAS = PAS.y)
                      )
                    #puissance moyenne ou médiane, des 4 mêmes jours de semaine éligibles passées ou des 10 derniers jours éligibles passés
                    , meth == 'HISTORIQUE' & meca %in% c('MA','NEBEF') ~

                      list(
                        ts %>%
                          dplyr::filter(
                            MECANISME == meca
                            , CODE_ENTITE == entt
                            , CODE_SITE == site
                            , HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE)
                          ) %>%
                          mutate(HORODATE_UTC = floor_date(as_datetime(deb), 'day') + dhours(hour(HORODATE_UTC)) + dminutes(minute(HORODATE_UTC))) %>%
                          dplyr::group_by(MECANISME,CODE_ENTITE,CODE_SITE,HORODATE_UTC) %>%
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
                              dplyr::filter(
                                MECANISME == meca
                                , CODE_ENTITE == entt
                                , CODE_SITE == site
                                , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
                              )
                            , by = c('MECANISME','CODE_ENTITE','CODE_SITE','HORODATE_UTC')
                          ) %>%
                          transmute(MECANISME, CODE_ENTITE, CODE_SITE, HORODATE, HORODATE_UTC, REALISE = PUISSANCE, REFERENCE, PAS)
                      )

                    , TRUE ~ list(tibble(MECANISME = character(), CODE_ENTITE = character(), CODE_SITE = character(), HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), REALISE = double(), REFERENCE = double(), PAS = integer()))
                  )
                  , what = 'cbind'
                )
              )
            }
          }
        )
      )
  }

  # tbl_cdcRef %>%
  #   mutate(
  #     `CHRONIQUE DE REFERENCE INCOMPLETE` = map_dbl(data, ~ sum(!is.na(.$REFERENCE))) < as.duration(FIN - DEBUT)%/%dseconds(unique(data$PAS))
  #     , `CHRONIQUE DE REALISE INCOMPLETE` = map_dbl(data, ~ sum(!is.na(.$REALISE))) < as.duration(FIN - DEBUT)%/%dseconds(unique(data$PAS))
  #   )
  #
  # if(any(with(tbl_cdcRef,`CHRONIQUE DE REFERENCE INCOMPLETE`|`CHRONIQUE DE REALISE INCOMPLETE`)))
  # {
  #   dplyr::filter(tbl_cdcRef, `CHRONIQUE DE REFERENCE INCOMPLETE`|`CHRONIQUE DE REALISE INCOMPLETE`) %>%
  #     dplyr::select(CODE_ENTITE,CODE_SITE,DEBUT,FIN) %>%
  #     {
  #       warning(
  #         paste(
  #           capture.output(
  #             {
  #               cat("Courbe(s) de charge réalisée ou de référence incomplète(s) sur la période d'effacement ou report")
  #               print(subset(., `CHRONIQUE DE REFERENCE INCOMPLETE`|`CHRONIQUE DE REALISE INCOMPLETE`, data)[[1]][which(is.na(REALISE)|is.na(REFERENCE)), 'HORODATE_UTC'])
  #               #print(., len = nrow(.))
  #             }
  #           )
  #           , collapse = '\n')
  #       )
  #     }
  # }

  #aggrégation des tbl_cdc par entité
  # tbl_cdcRefAgr = dplyr::filter(tbl_cdcRef, METHODE == 'RECTANGLE') %>%
  #   group_by(MECANISME, METHODE, CODE_ENTITE, HORODATE, HORODATE_UTC) %>%
  #   summarise(
  #     PUISSANCE = sum(PUISSANCE)
  #     , CAPA_MAX_H_ENTITE = sum(CAPA_MAX_H_SITE)
  #   ) %>%
  #   mutate(
  #     data = case_when(
  #       #puissance moyenne sur la demi-heure ronde précédant le DMO
  #       meth == 'RECTANGLE' & meca == 'MA' ~
  #
  #         list(
  #           ts %>%
  #             dplyr::filter(
  #               MECANISME == meca
  #               , CODE_ENTITE == entt
  #               , CODE_SITE == site
  #               , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
  #             ) %>%
  #             add_column(REFERENCE =
  #                          mean(subset(ts, MECANISME == meca & CODE_ENTITE == entt & CODE_SITE == site & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[1]), PUISSANCE)[[1]])
  #                        , .before = 'PAS') %>%
  #             rename(REALISE = PUISSANCE)
  #         )
  #       #puissance moyenne minimum, resp. maximum, entre la période précédant et la période suivant l'effacement, resp. le report
  #       , meth == 'RECTANGLE' & meca == 'NEBEF' ~
  #
  #         list(
  #           ts %>%
  #             dplyr::filter(
  #               MECANISME == meca
  #               , CODE_ENTITE == entt
  #               , CODE_SITE == site
  #               , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
  #             ) %>%
  #             add_column(REFERENCE =
  #                          signe * min(
  #                            signe * mean(subset(ts, MECANISME == meca & CODE_ENTITE == entt & CODE_SITE == site & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[1]), PUISSANCE)[[1]])
  #                            , signe * mean(subset(ts, MECANISME == meca & CODE_ENTITE == entt & CODE_SITE == site & HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE[2]), PUISSANCE)[[1]])
  #                          )
  #                        , .before = 'PAS'
  #             ) %>%
  #             rename(REALISE = PUISSANCE)
  #         )
  #
  #     )
  #   )%>%
  #   ungroup()

  #recalage du volume d'effacement sur la capacité totale de l'entité

  # return(tbl_cdcRef)

}
#   tbl_cdccrmc1<-list()
#
#   if(length(entites)==0){
#
#     print("Aucune entite activee au cours de la semaine")
#
#   }else{
#
#     #2 Boucle sur les entites----
#     for(i in 1:length(entites)){
#
#       entite<-entites[i]
#       tbl_cdcagr30e<-tbl_cdcagr[tbl_cdcagr$CODE_ENTITE==entite,]
#       eff<-tbl_eff[tbl_eff$CODE_ENTITE==entite,]
#
#       if(nrow(eff)>0 & nrow(tbl_cdcagr30e)>0){
#
#         sites <- tbl_sites$CODE_SITE[tbl_sites$CODE_ENTITE==entite]
#
#         #2a Application des methodes a la maille entite
#         # if(substr(entite,1,3)!="EDE"){
#         #
#         #   tbl_cdcref <- CR_RectangleSimple(tbl_cdc=tbl_cdcagr30e,eff=eff)
#         #   METHODE = "RECTANGLE_MA"
#         #
#         # }else{
#
#         METHODE <- tbl_entt$METHODE[tbl_entt$CODE_ENTITE == entite][1]
#
#         #Si la methode est "PREVISION" et qu'il n'y a aucune prevision alors "RECTANGLE"
#         #Si la methode est "HISTORIQUE" et qu'il n'y a aucune variante alors "RECTANGLE"
#         if(length(METHODE)==0 | is.na(METHODE))METHODE <- "RECTANGLE"
#         if((METHODE == "HISTORIQUE" & length(which(tbl_homol$CODE_SITE %in% sites)) == 0) | (METHODE == "PREVISION" & length(which(tbl_prev$CODE_SITE %in% sites)) == 0))
#         {            METHODE <- "RECTANGLE"          }
#         if(METHODE=="RECTANGLE"){
#           if(substr(entite,1,3)!="EDE"){
#             tbl_cdcref <- CR_RectangleSimple(tbl_cdc=tbl_cdcagr30e,eff=eff)
#             tbl_cdcagr30e$PUISSANCE_effacee <- tbl_cdcref$PUISSANCE - tbl_cdcagr30e$PUISSANCE
#             tbl_cdcagr30e$PUISSANCE_effacee[tbl_cdcagr30e$PUISSANCE_effacee<0]<-0
#           }else{
#             tbl_cdcref<-CR_RectangleDouble(tbl_cdc=tbl_cdcagr30e,eff=eff)
#             tbl_cdcagr30e$PUISSANCE_effacee <- tbl_cdcref$PUISSANCE - tbl_cdcagr30e$PUISSANCE
#             tbl_cdcagr30e$SIGNE = tbl_cdcref$SIGNE
#
#             tbl_cdcagr30e$PUISSANCE_effacee[tbl_cdcagr30e$PUISSANCE_effacee < 0 & tbl_cdcagr30e$SIGNE > 0] <- 0
#             tbl_cdcagr30e$PUISSANCE_effacee[tbl_cdcagr30e$PUISSANCE_effacee > 0 & tbl_cdcagr30e$SIGNE < 0] <- 0
#           }
#
#         }
#
#         tbl_indhistEff<-unique(substr(tbl_effhisto$DEBUT[tbl_effhisto$CODE_ENTITE==entite],1,10))#on invalide la journee de debut d'effacement
#
#         tbl_cdcent<-tbl_cdc[tbl_cdc$CODE_ENTITE==entite,]
#         tbl_cdcsites1<-list()
#
#         logprint(paste(entite,METHODE,"\n"))
#
#         for(j in 1:length(sites)){
#
#           if(length(sites)<=10)logprint(paste(sites[j],METHODE,"\n"))
#
#           tbl_cdcsit<-tbl_cdcent[tbl_cdcent$CODE_SITE==sites[j],]
#
#           if(nrow(tbl_cdcsit)==0){
#
#             print(paste("pas de courbes pour", sites[j]))
#
#           }else{
#
#             #2b Application des methodes a la maille site
#
#             if(METHODE=="SITE_A_SITE"){
#               tbl_cdcref<-CR_RectangleDouble(tbl_cdc=tbl_cdcsit,eff=eff)
#             }
#
#             if(METHODE=="PREVISION")
#             {
#               tbl_prevsit<-tbl_prev[tbl_prev$CODE_SITE == sites[j],]
#               tbl_cdcref <- CR_PREVISION(tbl_prev = tbl_prevsit,eff = eff,tbl_cdc = tbl_cdcsit)
#               if(length(tbl_cdcref)==0)logprint(paste("Pas de tbl_cdc de prevision pour le site", sites[j]," : application du rectangle \n"))
#             }
#
#             if(METHODE=="HISTORIQUE")
#             {
#               VARIANTE_HIST <- tbl_homol$VARIANTE_HIST[tbl_homol$CODE_SITE==sites[j]]
#               if(length(VARIANTE_HIST) == 0) VARIANTE_HIST <- "MOY10J"
#
#               tbl_cdcref <- CR_HISTORIQUE(tbl_cdc=tbl_cdcsit,eff=eff,VARIANTE_HIST=VARIANTE_HIST,DATE_INDISPO=c(tbl_indhistEff,tbl_indhist$DATE_INDISPO[tbl_indhist$CODE_SITE==sites[j]]))
#
#             }
#
#             if(METHODE=="RECTANGLE" | length(tbl_cdcref)==0){
#               if(substr(entite,1,3)!="EDE"){
#                 tbl_cdcref <- CR_RectangleSimple(tbl_cdc=tbl_cdcsit,eff=eff)
#               }else{
#                 tbl_cdcref<-CR_RectangleDouble(tbl_cdc=tbl_cdcsit,eff=eff)
#               }
#             }
#
#             tbl_cdcsit$PUISSANCE_effacee<-tbl_cdcref$PUISSANCE-tbl_cdcsit$PUISSANCE
#
#             tbl_cdcsit$SIGNE = tbl_cdcref$SIGNE
#
#             tbl_cdcsit$PUISSANCE_effacee[tbl_cdcsit$PUISSANCE_effacee < 0 & tbl_cdcsit$SIGNE > 0] <- 0
#             tbl_cdcsit$PUISSANCE_effacee[tbl_cdcsit$PUISSANCE_effacee > 0 & tbl_cdcsit$SIGNE < 0] <- 0
#
#             tbl_cdcsites1[[j]]<-tbl_cdcsit
#
#           }#/if tbl_cdc
#         }#/for site
#         tbl_cdcsites<- do.call("rbind",tbl_cdcsites1)
#
#         #2c Filtrage sur les journees avec effacement pour la semaine consideree (les tbl_eff de l'historique n'interviennent pas ici)
#         tbl_cdcsites <- tbl_cdcsites[substr(tbl_cdcsites$HORODATE,1,10) %in% substr(c(eff$DEBUT,eff$FIN-1),1,10),]
#
#         #2d1 Agregation des tbl_eff-sites a la maille entite
#
#         #tbl_cdcenteffagr<-aggregate(PUISSANCE_effacee~HORODATE_UTC+HORODATE,tbl_cdcsites,sum)
#         tbl_cdcenteffagr<-aggregate(PUISSANCE_effacee~CODE_ENTITE+HORODATE_UTC+HORODATE,tbl_cdcsites,sum)#Plante si tbl_cdcsites vide suite au filtre
#
#         if(METHODE %in% c("HISTORIQUE","PREVISION")) tbl_cdcagr30e<-tbl_cdcenteffagr
#
#         #2d2 Recalage des volumes avec la capacite de l'EDE (exprimee en kW)
#         if(substr(entite,1,3)=="EDE"){
#           capa<-sum(tbl_sites$CAPA_MAX_H_SITE[tbl_sites$CODE_ENTITE==entite])
#           tbl_cdcagr30e$PUISSANCE_effacee[abs(tbl_cdcagr30e$PUISSANCE_effacee)>capa]<- capa * sign(tbl_cdcagr30e$PUISSANCE_effacee)
#         }
#
#         #tbl_cdcenteffagr2<-merge(tbl_cdcenteffagr,tbl_cdcagr30e,by=c("HORODATE_UTC","HORODATE"))
#         tbl_cdcenteffagr2<-merge(tbl_cdcenteffagr,tbl_cdcagr30e,by=c("CODE_ENTITE","HORODATE_UTC","HORODATE"))
#
#         tbl_cdcenteffagr2$ratio<-tbl_cdcenteffagr2$PUISSANCE_effacee.y/tbl_cdcenteffagr2$PUISSANCE_effacee.x#on divise l'effacement a la maille entite par la somme des tbl_eff a la maille site
#         tbl_cdcenteffagr2$ratio[tbl_cdcenteffagr2$PUISSANCE_effacee.x==0]<-1
#         tbl_cdcsit2<-merge(tbl_cdcsites,tbl_cdcenteffagr2[,c("CODE_ENTITE","HORODATE_UTC","HORODATE","ratio")],by=c("CODE_ENTITE","HORODATE_UTC","HORODATE"))
#         tbl_cdcsit2$PUISSANCE_effacee<-tbl_cdcsit2$PUISSANCE_effacee*tbl_cdcsit2$ratio
#         tbl_cdccrmc1[[i]]<-tbl_cdcsit2[,c("CODE_ENTITE","CODE_SITE","HORODATE_UTC","HORODATE","PUISSANCE_effacee")]
#       }#/if eff
#     }#/for entite
#   }
#   tbl_cdccrmc<- do.call("rbind",tbl_cdccrmc1)
#   return(tbl_cdccrmc)
# }
