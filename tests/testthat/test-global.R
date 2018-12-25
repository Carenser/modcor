library(tidyverse)
library(lubridate)
library(fuzzyjoin)

options(warning.length = 5000L, tibble.print_max = Inf)

map(.x = list.files(path = paste0(getwd(),'/R'), pattern = '[^Main.R]',full.names = TRUE),.f = function(x){source(file = x, prompt.echo = FALSE, verbose = FALSE, print.eval = FALSE, echo = FALSE)})

# Chargement des données  -------------------------------------------------

tbl_cdc = read_cdc(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '^NEBEF.*_(20180127|20180203|20180210|20180217|20180224).*.csv', full.names = TRUE), dossiers = NULL)

tbl_eff = read_eff(fichiers = list.files(recursive = TRUE, path = paste0(getwd(),'/data-raw/'), pattern = '(OA_GRD|PEC_GRD)', full.names = TRUE), dossiers = NULL)

tbl_sites = read_perim(dossiers = paste0(getwd(),'/data-raw/201802'))

tbl_entt = read_entt(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_homol = read_homol(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_indhist = read_indispo(dossiers  =  paste0(getwd(),'/data-raw/', c('201712','201801','201802')))

tbl_prev = read_prev(dossiers =  paste0(getwd(),'/data-raw/201802'))

tbl_cdcRef = create_ref(tbl_cdc = tbl_cdc, tbl_sites = tbl_sites, tbl_eff = dplyr::filter(tbl_eff,MECANISME == 'NEBEF'), tbl_entt = tbl_entt, tbl_homol = tbl_homol, tbl_indhist = tbl_indhist, tbl_prev = tbl_prev)


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

test2 = dplyr::filter(test,MECANISME == 'NEBEF' & !is.na(CODE_SITE)) %>%
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
        print(if_else(signe < 0, 'report', 'effacement'))

        if(
          nrow(ts %>%
               dplyr::filter(
                 MECANISME == meca
                 , CODE_ENTITE == entt
                 , CODE_SITE == site
                 , HORODATE_UTC %within% as.list(as_datetime(deb)%--%as_datetime(fin))
               )) == 0)
        {
          print('courbe de charge manquante : impossible de calculer la courbe de référence')
          tibble(MECANISME = character(), CODE_ENTITE = character(), CODE_SITE = character(), HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), REALISE = double(), REFERENCE = double(), PAS = integer())

        }else{
          as.tibble(
            do.call(
              args = case_when(

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

                , meth == 'HISTORIQUE' & meca %in% c('MA','NEBEF') ~

                  list(ts %>%
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
                # , TRUE ~ tibble(MECANISME = NA_character_, CODE_ENTITE = NA_character_, CODE_SITE = NA_character_, HORODATE = as_datetime(NA_integer_), HORODATE_UTC = as_datetime(NA_integer_), REALISE = NA_real_, REFERENCE = NA_real_, PAS = NA_integer_)
              )
              , what = 'cbind'
            )
          )
        }
      }
    )
  )

library(plotly)

p = do.call(args = test2$data,what = rbind) %>%
  gather(key = 'TYPE', value = 'PUISSANCE', - MECANISME, -CODE_ENTITE, -CODE_SITE, - HORODATE, -HORODATE_UTC, - PAS) %>%
  ggplot(mapping = aes(x = HORODATE_UTC, y = PUISSANCE, col = CODE_SITE, linetype = TYPE)) +
  geom_line()
ggplotly(p, tooltip = c('PUISSANCE','HORODATE_UTC'))
