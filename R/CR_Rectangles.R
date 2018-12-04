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
    if(i==1)successive<-0 else successive<-(eff$debut[i]<eff$fin[i-1]+eff$DMO[i-1]+30*60)#regle sur les activations successives d'une meme offre
    heurefinPref<-as.POSIXct(floor(as.numeric(eff$debut[i]-eff$DMO[i]*60)/1800)*1800,origin="1970-01-01")#on arrondit au pas demi-horaire pr?s
    if(successive==0)w<-which(cdc$HORODATE<heurefinPref & cdc$HORODATE>=heurefinPref-30*60)else print(paste("successive",paste(eff[i,],collapse="_")))
    if(length(w)>0)cdc$PUISSANCE[cdc$HORODATE>eff$debut[i]-10*60 & cdc$HORODATE<eff$fin[i]]<-mean(cdc$PUISSANCE[w])else print(paste("Pas de reference pour ",paste(eff[i,],collapse="_")))
    
    cdc$SIGNE[cdc$HORODATE>eff$debut[i]-10*60 & cdc$HORODATE<eff$fin[i]] = eff$SIGNE[i]
  }
  
  return(cdc)
}

CR_RectangleDouble<-function(cdc,eff){
  for(i in 1:nrow(eff)){
    avant<-which(cdc$HORODATE<eff$debut[i] & cdc$HORODATE>=as.POSIXct(as.numeric(eff$debut[i])*2-as.numeric(eff$fin[i]),origin="1970-01-01"))
    apres<-which(cdc$HORODATE>=eff$fin[i] & cdc$HORODATE<as.POSIXct(as.numeric(eff$fin[i])*2-as.numeric(eff$debut[i]),origin="1970-01-01"))
    if(length(avant)>0)Pavant<-mean(cdc$PUISSANCE[avant])else Pavant<-Inf
    if(length(apres)>0)Papres<-mean(cdc$PUISSANCE[apres])else Papres<-Inf
    cdc$PUISSANCE[cdc$HORODATE>=eff$debut[i] & cdc$HORODATE<eff$fin[i]]<-ifelse(eff$SIGNE[i] < 0, max(Pavant,Papres), min(Pavant,Papres))

    cdc$SIGNE[cdc$HORODATE>=eff$debut[i] & cdc$HORODATE<eff$fin[i]] = eff$SIGNE[i]

  }
  return(cdc)
}

CR_Histo<-function(cdc,eff,VARIANTE_HIST,DATE_INDISPO){#renvoie une courbe de r?f?rence
  cdc$heure<-substr(cdc$HORODATE,12,15)
  cdc$jour<-as.Date(substr(cdc$HORODATE,1,10))
  Jrs<-unique(cdc$jour)
  Jrs<-Jrs[!Jrs %in% as.Date(DATE_INDISPO)]
  Jrs<-Jrs[order(Jrs)]
  for(i in 1:nrow(eff)){
    J<-as.Date(substr(eff$debut[i],1,10))
    Jrs2<-Jrs[Jrs<J]
    if(VARIANTE_HIST %in% c("MED4S","MOY4S")){
      Jrs2<-Jrs2[format(Jrs2,"%w")==format(J,"%w")]
      Jrs2<-Jrs2[max(1,length(Jrs2)-3):length(Jrs2)]#on utilise les 4 derni?res semaines
    }else{
      Jrs2<-Jrs2[max(1,length(Jrs2)-9):length(Jrs2)]#on utilise les 10 derniers jours
    }
    w<-which(cdc$HORODATE>eff$debut[i]-10*60 & cdc$HORODATE<eff$fin[i])
    if(VARIANTE_HIST %in% c("MOY4S","MOY10J")){#en cas de non connaissance de la variante, on utilise MED10J
      for(w1 in w)cdc$PUISSANCE[w1]<-mean(cdc$PUISSANCE[cdc$jour %in% Jrs2 & cdc$heure %in% cdc$heure[w1]])
    }else{
      for(w1 in w)cdc$PUISSANCE[w1]<-median(cdc$PUISSANCE[cdc$jour %in% Jrs2 & cdc$heure %in% cdc$heure[w1]])
    }
    
    cdc$SIGNE[w] = eff$SIGNE[i]
  }
  return(cdc[,names(cdc)[!names(cdc) %in% c("jour","heure")]])
}

CR_Prev <- function(cdcPrev,eff,cdc){
  names(cdcPrev)[names(cdcPrev)=="PUISSANCE"]<-"prev"
  cdcref<-merge(cdc,cdcPrev,by=c("HORODATE","HORODATE_UTC"),all.x=T)
  w<-which(is.na(cdcref$prev) & gsub("-","",cdcref$DATE) %in% gsub("-","",eff$DATE))
  if(length(w)>0){
    cdcref<-numeric(0)
  }else{
    for(i in 1:nrow(eff)){
      w<-which(cdcref$HORODATE > eff$debut[i]-10*60 & cdcref$HORODATE<eff$fin[i])
      cdcref$PUISSANCE[w]<-cdcref$prev[w]
      
      cdcref$SIGNE[w] = eff$SIGNE[i]
    }
  }
  return(cdcref)
}

