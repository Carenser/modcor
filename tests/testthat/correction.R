#recalage du volume d'effacement sur la capacité maximale de l'entité
test = 
  tbl_refSite %>%
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
    
    , RATIO = CHRONIQUE_ENTITE/SOMME_CHRONIQUE
    , CHRONIQUE = if_else(
      condition = SIGNE < 0
      , true =  pmin(0,mean(VOLUME))  
      , false =  pmax(0,mean(VOLUME))
    ) * if_else(is.nan(RATIO),1,RATIO,1)
  )


View(test)
