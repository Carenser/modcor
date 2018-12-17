#' Title
#'
#' @param cdc
#' @param eff
#'
#' @return
#' @export
#' @import tidyverse
#' @import fuzzyjoin
#' @examples
CR_RectangleSimple<-function(cdc,eff){
  for(i in 1:nrow(eff)){
    if(i==1)successive<-0 else successive<-(eff$DEBUT[i]<eff$FIN[i-1]+eff$DMO[i-1]+30*60)#regle sur les activations successives d'une meme offre
    heureFINPref<-as.POSIXct(floor(as.numeric(eff$DEBUT[i]-eff$DMO[i]*60)/1800)*1800,origin="1970-01-01")#on arrondit au pas demi-horaire pr?s
    if(successive==0)w<-which(cdc$HORODATE<heureFINPref & cdc$HORODATE>=heureFINPref-30*60)else print(paste("successive",paste(eff[i,],collapse="_")))
    if(length(w)>0)cdc$PUISSANCE[cdc$HORODATE>eff$DEBUT[i]-10*60 & cdc$HORODATE<eff$FIN[i]]<-mean(cdc$PUISSANCE[w])else print(paste("Pas de reference pour ",paste(eff[i,],collapse="_")))
    
    cdc$SIGNE[cdc$HORODATE>eff$DEBUT[i]-10*60 & cdc$HORODATE<eff$FIN[i]] = eff$SIGNE[i]
  }
  
  return(cdc)
}

CR_RectangleDouble<-function(cdc,eff){
  for(i in 1:nrow(eff)){
    avant<-which(cdc$HORODATE<eff$DEBUT[i] & cdc$HORODATE>=as.POSIXct(as.numeric(eff$DEBUT[i])*2-as.numeric(eff$FIN[i]),origin="1970-01-01"))
    apres<-which(cdc$HORODATE>=eff$FIN[i] & cdc$HORODATE<as.POSIXct(as.numeric(eff$FIN[i])*2-as.numeric(eff$DEBUT[i]),origin="1970-01-01"))
    if(length(avant)>0)Pavant<-mean(cdc$PUISSANCE[avant])else Pavant<-Inf
    if(length(apres)>0)Papres<-mean(cdc$PUISSANCE[apres])else Papres<-Inf
    cdc$PUISSANCE[cdc$HORODATE>=eff$DEBUT[i] & cdc$HORODATE<eff$FIN[i]]<-ifelse(eff$SIGNE[i] < 0, max(Pavant,Papres), min(Pavant,Papres))
    
    cdc$SIGNE[cdc$HORODATE>=eff$DEBUT[i] & cdc$HORODATE<eff$FIN[i]] = eff$SIGNE[i]
    
  }
  return(cdc)
}

CR_HISTORIQUE<-function(cdc,eff,VARIANTE_HIST,DATE_INDISPO){#renvoie une courbe de r?f?rence
  cdc$heure<-substr(cdc$HORODATE,12,15)
  cdc$jour<-as.Date(substr(cdc$HORODATE,1,10))
  Jrs<-unique(cdc$jour)
  Jrs<-Jrs[!Jrs %in% as.Date(DATE_INDISPO)]
  Jrs<-Jrs[order(Jrs)]
  for(i in 1:nrow(eff)){
    J<-as.Date(substr(eff$DEBUT[i],1,10))
    Jrs2<-Jrs[Jrs<J]
    if(VARIANTE_HIST %in% c("MED4S","MOY4S")){
      Jrs2<-Jrs2[format(Jrs2,"%w")==format(J,"%w")]
      Jrs2<-Jrs2[max(1,length(Jrs2)-3):length(Jrs2)]#on utilise les 4 derni?res semaines
    }else{
      Jrs2<-Jrs2[max(1,length(Jrs2)-9):length(Jrs2)]#on utilise les 10 derniers jours
    }
    w<-which(cdc$HORODATE>eff$DEBUT[i]-10*60 & cdc$HORODATE<eff$FIN[i])
    if(VARIANTE_HIST %in% c("MOY4S","MOY10J")){#en cas de non connaissance de la variante, on utilise MED10J
      for(w1 in w)cdc$PUISSANCE[w1]<-mean(cdc$PUISSANCE[cdc$jour %in% Jrs2 & cdc$heure %in% cdc$heure[w1]])
    }else{
      for(w1 in w)cdc$PUISSANCE[w1]<-median(cdc$PUISSANCE[cdc$jour %in% Jrs2 & cdc$heure %in% cdc$heure[w1]])
    }
    
    cdc$SIGNE[w] = eff$SIGNE[i]
  }
  return(cdc[,names(cdc)[!names(cdc) %in% c("jour","heure")]])
}

CR_PREVISION <- function(tbl_eff, tbl_cdc, tbl_prev, int_step){
  
  #discretisation de la prevision par pas de temps int_step seconds
  
  tbl_cdcref =
    dplyr::left_join(
      x = tbl_cdc
      , y =  dplyr::rename(tbl_prev,REFERENCE = PUISSANCE)
      , by = c('MECANISME','CODE_ENTITE','CODE_SITE','HORODATE','HORODATE_UTC')
    ) %>%
    fuzzyjoin::fuzzy_right_join(
      y = tbl_eff
      , by = c('MECANISME', 'CODE_ENTITE','HORODATE_UTC' = 'DEBUT', 'HORODATE_UTC' = 'FIN')
      , match_fun = list(`==`, `==`, `>=`, `<`)
    ) %>%
    dplyr::transmute(
      MECANISME = MECANISME.y
      , CODE_ENTITE = CODE_ENTITE.y
      , CODE_SITE = CODE_SITE.y
      , HORODATE_UTC
      , PUISSANCE
      , REFERENCE
      , FLEXIBILITE = REFERENCE - PUISSANCE
      , SIGNE
      , DEBUT
      , FIN
    ) %>%
    group_by(CODE_ENTITE, CODE_SITE, DEBUT, FIN) %>%
    nest() %>%
    mutate(
      `PREVISION INCOMPLETE` = map_dbl(data, ~ sum(!is.na(.$REFERENCE))) < as.duration(FIN - DEBUT)%/%dseconds(int_step)
      , `COURBE DE CHARGE INCOMPLETE` = map_dbl(data, ~ sum(!is.na(.$PUISSANCE))) < as.duration(FIN - DEBUT)%/%dseconds(int_step)
    )
  
  if(any(with(tbl_cdcref,`PREVISION INCOMPLETE`|`COURBE DE CHARGE INCOMPLETE`)))
  {
    dplyr::filter(tbl_cdcref, `PREVISION INCOMPLETE`|`COURBE DE CHARGE INCOMPLETE`) %>%
      dplyr::select(CODE_ENTITE,CODE_SITE,DEBUT,FIN) %>%
      {
        warning(
          paste(
            capture.output(
              {
                cat('Courbe(s) de charge ou de prévision manquante(s)\n Contrôle du réalisé impossible pour les activations suivantes :\n')
                print(., len = nrow(.))
              }
            )
            , collapse = '\n')
        )
      }
  }
  
  dplyr::filter(tbl_cdcref, !`PREVISION INCOMPLETE` & !`COURBE DE CHARGE INCOMPLETE`) %>%
    unnest() %>%
    select(MECANISME, CODE_ENTITE, CODE_SITE, HORODATE_UTC, PUISSANCE, REFERENCE, FLEXIBILITE)
}

#' Title
#'
#' @param cdc
#' @param eff
#'
#' @return
#' @export
#' @import tidyverse
#' @import fuzzyjoin
#' @examples
CR_RECTANGLE<-function(cdc,eff){
  
  if(nrow(cdc)==0)
  {
    warning(paste('courbe de charge manquante pour le site',unique(cdc$CODE_SITE), 'sur la période du', range(eff$DEBUT)[1], 'au', range(eff$FIN)[2]))
    return(tibble(MECANISME = character(), CODE_ENTITE=character(), CODE_SITE = character(), HORODATE = as_datetime(integer()), HORODATE_UTC = as_datetime(integer()), PUISSANCE = double(), REFERENCE = double(), SIGNE = integer()))
  }
  
  for(i in seq_len(nrow(eff)))
  {
    if(eff$MECANISME[i] == 'MA')
    {
      #if(i==1)successive<-0 else successive<-(eff$DEBUT[i]<eff$FIN[i-1]+eff$DMO[i-1]+30*60)#regle sur les activations successives d'une meme offre
      heureFINPref <- lubridate::round_date(eff$DEBUT[i]-eff$DMO[i], unit = 'minutes') #on arrondit au pas demi-horaire près
      #if(successive==0) w<-which(cdc$HORODATE<heureFINPref & cdc$HORODATE>=heureFINPref-30*60)else print(paste("successive",paste(eff[i,],collapse="_")))
      #if(length(w)>0)cdc$REFERENCE[cdc$HORODATE>eff$DEBUT[i]-10*60 & cdc$HORODATE<eff$FIN[i]]<-mean(cdc$PUISSANCE[w])else print(paste("Pas de reference pour ",paste(eff[i,],collapse="_")))
      
      cdc$REFERENCE[cdc$HORODATE>eff$DEBUT[i]-10*60 & cdc$HORODATE<eff$FIN[i]]<-mean(cdc$PUISSANCE[which(cdc$HORODATE<heureFINPref & cdc$HORODATE>=heureFINPref-30*60)])
      
      cdc$SIGNE[cdc$HORODATE>eff$DEBUT[i]-10*60 & cdc$HORODATE<eff$FIN[i]] = eff$SIGNE[i]
      
    }
    
    if(eff$MECANISME[i] == 'NEBEF')
    {
      avant<-which(cdc$HORODATE<eff$DEBUT[i] & cdc$HORODATE>=as.POSIXct(as.numeric(eff$DEBUT[i])*2-as.numeric(eff$FIN[i]),origin="1970-01-01"))
      apres<-which(cdc$HORODATE>=eff$FIN[i] & cdc$HORODATE<as.POSIXct(as.numeric(eff$FIN[i])*2-as.numeric(eff$DEBUT[i]),origin="1970-01-01"))
      
      if(length(avant)>0)Pavant<-mean(cdc$PUISSANCE[avant])else Pavant<-Inf
      if(length(apres)>0)Papres<-mean(cdc$PUISSANCE[apres])else Papres<-Inf
      
      cdc$REFERENCE[cdc$HORODATE>=eff$DEBUT[i] & cdc$HORODATE<eff$FIN[i]] <- ifelse(eff$SIGNE[i] < 0, max(Pavant,Papres), min(Pavant,Papres))
      
      cdc$SIGNE[cdc$HORODATE>=eff$DEBUT[i] & cdc$HORODATE<eff$FIN[i]] = eff$SIGNE[i]
    }
  }
  
  return(cdc)
}
