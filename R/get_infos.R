
#' Title
#'
#' @param tbl_eff 
#' @param tbl_entt 
#' @param tbl_sites 
#' @param tbl_homol 
#' @param tbl_indhist 
#'
#' @return
#' @export
#' @import tidyverse
#' @import fuzzyjoin
#' @import lubridate
#' @examples
get_infos = function(tbl_eff, tbl_entt, tbl_sites, tbl_homol, tbl_indhist)
{
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
    )%>%
    # identification des semaines BGC associées
    mutate(
      sem = purrr::map2(
        .x = ref
        , .y = as_date(DEBUT, tz = 'CET')
        , .f =  function(x,y)
        {
          unique(floor_date(c(as_date(y), as_date(int_start(x$PERIODE_REFERENCE))), unit = 'week', week_start = 6))
        }
      )
    )
}
