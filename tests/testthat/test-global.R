library(tidyverse)
library(lubridate)
library(fuzzyjoin)

options(warning.length = 5000L, tibble.print_max = Inf)

map(.x = list.files(path = paste0(getwd(),'/R'), pattern = '[^Main.R]',full.names = TRUE),.f = function(x){source(file = x, prompt.echo = FALSE, verbose = FALSE, print.eval = FALSE, echo = FALSE)})

# Chargement des données  -------------------------------------------------

tbl_cdc = LoadCdC(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '^NEBEF.*_(20180127|20180203|20180210|20180217|20180224).*.csv', full.names = TRUE), dossiers = NULL)

tbl_eff = LoadEffacements(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '(OA_GRD|PEC_GRD)', full.names = TRUE), dossiers = NULL)

tbl_sites = LoadPerimetre(dossiers = paste0(getwd(),'/data-raw/201802'))

tbl_entt = LoadListeEntt(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_homol = LoadSitesHomol(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_indhist = LoadIndHist(dossiers  =  paste0(getwd(),'/data-raw/', c('201712','201801','201802')))

tbl_prev = LoadPrev(dossiers =  paste0(getwd(),'/data-raw/201802'))

test = fuzzy_left_join(
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
    #si la méthode est HISTORIQUE, la variante par défaut est MOY10J (la plus répandue; utile ?)
    , VARIANTE = if_else(condition = METHODE.x == 'HISTORIQUE' & is.na(VARIANTE), true = 'MOY10J', false =  VARIANTE)
    , DEBUT = DEBUT.x
    , FIN = FIN.x
    , SIGNE
    , DMO
  ) %>%
  distinct() %>% #sites homologués à d'autres méthodes que le rectangle
  #fusion avec les tables d'indisponibilité
  fuzzyjoin::fuzzy_left_join(
    y = tbl_indhist
    , by = c('MECANISME','CODE_ENTITE','CODE_SITE','DEBUT' = 'DATE')
    , match_fun = list(`==`,`==`,`==`,`>=`)
  ) %>%
  transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x,CODE_SITE = CODE_SITE.x, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO, DATE_INDHIST = DATE) %>%
  group_by(MECANISME, CODE_ENTITE, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT, METHODE, VARIANTE, DEBUT, FIN, SIGNE, DMO) %>%
  nest() %>%
  #ajout des journées d'effacement et reports passées du site dans l'historique d'indisponibilité
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
  #identification des journées utilisées comme historique pour la méthode HISTORIQUE
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
  #identification des périodes de référence en fonction des méthodes
  mutate(
    data = purrr::pmap(
      .l = list(x = data, y = DEBUT, z = FIN, t = MECANISME, u = METHODE, v = DMO)
      , .f = function(x,y,z,t,u,v)
      {
        tibble(
          PERIODE_REFERENCE = do.call(
            args = case_when(
              t == 'MA' & u == 'RECTANGLE' ~ list(c(lubridate::floor_date(as_datetime(y) - dseconds(v) - dminutes(30), unit = '30 minutes')%--%lubridate::floor_date(as_datetime(y) - dseconds(v), unit = '30 minutes'))) #arrondir au pas 30 minute précédent
              , t == 'NEBEF' & u == 'RECTANGLE' ~ list(c((as_datetime(y) - min(as.duration(as_datetime(z) - as_datetime(y)), dhours(2)))%--%as_datetime(y),as_datetime(z)%--%(as_datetime(z) + min(as.duration(as_datetime(z) - as_datetime(y)), dhours(2))))) #arrondir au pas 30 minute précédent
              , u == 'PREVISION' ~ list(c(as_datetime(y)%--%as_datetime(z)))
              , u == 'HISTORIQUE' ~ list(c((as_date(x$DATE_HIST) + dhours(hour(as_datetime(y))) + dminutes(minute(as_datetime(y))))%--%(as_date(x$DATE_HIST) + dhours(hour(as_datetime(z))) + dminutes(minute(as_datetime(z))))))
              , TRUE ~ list(c(as.interval(NA)))
            )
            , what = 'c'
          )
        )
      }
    )
  )


ref = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), data)[[1]][[1]]
deb = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), DEBUT)[[1]][[1]]
fin = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), FIN)[[1]][[1]]
meca = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), MECANISME)[[1]][[1]]
meth = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), METHODE)[[1]][[1]]
entt = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), CODE_ENTITE)[[1]][[1]]
site = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), CODE_SITE)[[1]][[1]]
variante = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), VARIANTE)[[1]][[1]]
signe = subset(test,MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), SIGNE)[[1]][[1]]
ts = tbl_cdc
prev = tbl_prev


ref = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), data)[[1]][[1]]
deb = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), DEBUT)[[1]][[1]]
fin = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), FIN)[[1]][[1]]
meca = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), MECANISME)[[1]][[1]]
meth = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), METHODE)[[1]][[1]]
entt = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), CODE_ENTITE)[[1]][[1]]
site = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), CODE_SITE)[[1]][[1]]
variante = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), VARIANTE)[[1]][[1]]
signe = subset(test,MECANISME == 'NEBEF' & METHODE == 'PREVISION' & !is.na(CODE_SITE), SIGNE)[[1]][[1]]
ts = tbl_cdc
prev = tbl_prev


ref = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), data)[[1]][[1]]
deb = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), DEBUT)[[1]][[1]]
fin = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), FIN)[[1]][[1]]
meca = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), MECANISME)[[1]][[1]]
meth = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), METHODE)[[1]][[1]]
entt = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), CODE_ENTITE)[[1]][[1]]
site = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), CODE_SITE)[[1]][[1]]
variante = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), VARIANTE)[[1]][[1]]
signe = subset(test,MECANISME == 'NEBEF' & METHODE == 'HISTORIQUE' & !is.na(CODE_SITE), SIGNE)[[1]][[1]]
ts = tbl_cdc
prev = tbl_prev


ref = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), data)[[1]][[5]]
deb = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), DEBUT)[[1]][[5]]
fin = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), FIN)[[1]][[5]]
meca = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), MECANISME)[[1]][[5]]
meth = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), METHODE)[[1]][[5]]
entt = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), CODE_ENTITE)[[1]][[5]]
site = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), CODE_SITE)[[1]][[5]]
variante = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), VARIANTE)[[1]][[5]]
signe = subset(test,MECANISME == 'MA' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE), SIGNE)[[1]][[5]]
ts = tbl_cdc
prev = tbl_prev

test2 = dplyr::filter(test, MECANISME == 'NEBEF' & METHODE == 'RECTANGLE' & !is.na(CODE_SITE)) %>%
  mutate(
    data = purrr::pmap(
      .l = list(ref = data, deb = DEBUT, fin = FIN, meca = MECANISME, meth = METHODE, entt = CODE_ENTITE, site = CODE_SITE, variante = VARIANTE, signe = SIGNE)
      , .f = function(ref, deb, fin, meca, meth, entt, site, variante, signe, ts = tbl_cdc, prev = tbl_prev)
      {

        print(ref)
        print(as_datetime(deb))
        print(as_datetime(fin))
        print(meca)
        print(meth)
        print(entt)
        print(site)
        print(variante)
        print(signe)

        case_when(

          meth == 'PREVISION' ~
            prev %>%
            dplyr::filter(
              MECANISME == meca
              , CODE_ENTITE == entt
              , CODE_SITE == site
              , HORODATE_UTC %within% as.list(ref$PERIODE_REFERENCE)
            ) %>%
            inner_join(
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

          , meth == 'HISTORIQUE' ~
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
            ungroup() %>%
            inner_join(
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

          , meth == 'RECTANGLE' & meca == 'MA' ~
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

          , meth == 'RECTANGLE' & meca == 'NEBEF' ~
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
          , TRUE ~ tibble(MECANISME = character(), CODE_ENTITE = character(), CODE_SITE = character(), HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), REALISE = double(), REFERENCE = double(), PAS = integer())
        )
      }
    )
  )

