#' Fonction de chargement des fichiers OA & PEC
#'
#' @param dir a character string giving PEC & OA files folder
#'
#' @return a dataframe with 4 columns CODE_ENTITE debut fin DMO
#' @export
#' @import reshape2
#' @examples
LoadEffacements<-function(dir){
  
  logprint(paste("Imports des effacements/OA dans le dossier", dir , "\n"))
  
  lf<-list.files(dir,full.names = T,recursive = T)
  #Fichiers des ordres d'ajustements
  lfoa <- list.files(path = dir,pattern = "OA_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv",full.names = TRUE)
  lfoa <- file.info(lfoa,extra_cols = TRUE)
  lfoa$Lien <- rownames(lfoa)
  rownames(lfoa) <- NULL
  
  #Horodate de cr?ation figurant dans le nom du fichier
  lfoa$HorodateCreation = as.POSIXct(x = gsub(x = lfoa$Lien, pattern = paste(dir,"OA_GRD_[0-9]{8}_[0-9A-Z]{16}_([0-9]{14}).csv", sep="/"), replacement = "\\1",perl = TRUE), format = "%Y%m%d%H%M%S")
  
  #Journ?e d'effacement correspondante
  lfoa$JourOA = as.Date(x = gsub(x = lfoa$Lien, pattern = paste(dir,"OA_GRD_([0-9]{8})_[0-9A-Z]{16}_[0-9]{14}.csv", sep="/"), replacement = "\\1",perl = TRUE), format = "%Y%m%d")
  
  #Tri des fichiers par Jour et horodate de cr?ation
  lfoa <- lfoa[order(lfoa$JourOA,lfoa$HorodateCreation, decreasing = TRUE),]
  
  #D?doublonnage des fichiers de CdC en selectionnant les plus r?cents par Jour d'effacement
  lfoa <- lfoa[!duplicated(lfoa[,c('JourOA')]),]
  
  if(length(lfoa$Lien)>0){
    for(oa0 in lfoa$Lien){
      suppressWarnings(oa1<-read.csv2(oa0,skip=2))
      if(oa0==lfoa$Lien[1])oa<-oa1 else oa<-rbind(oa,oa1)
    }
    oa<-oa[oa$ID_AJUSTEMENT!="<EOF>" & !is.na(oa$ACTIVATION_DEBUT),]
    oa$debut<-as.POSIXct(as.character(oa$ACTIVATION_DEBUT),format="%Y%m%d%H%M")
    oa$fin<-as.POSIXct(as.character(oa$ACTIVATION_FIN),format="%Y%m%d%H%M")
    oa$PUISSANCE[oa$SENS_AJUSTEMENT=="BAISSE"]<-(-oa$PUISSANCE[oa$SENS_AJUSTEMENT=="BAISSE"])
    oa<-oa[order(oa$CODE_EDA,oa$debut),]
    oa0<-list()
    for(i in 1:nrow(oa)){#Traitement des ordres/contrordres
      seq<-seq.POSIXt(from=oa$debut[i],to=oa$fin[i],by="1 min")
      oa0[[i]]<-data.frame(CODE_EDA=oa$CODE_EDA[i],HORODATE=seq,PUISSANCE=oa$PUISSANCE[i],DMO=oa$DMO[i])
      oa0[[i]][nrow(oa0[[i]]),"PUISSANCE"]<-0#on termine par une puissance nulle
    }
    oa2<-do.call("rbind",oa0)
    dmo<-oa2[!duplicated(oa2[,c("CODE_EDA","HORODATE")]),c("CODE_EDA","HORODATE","DMO")]#on conserve le DMO du premier ordre
    oa3<-aggregate(PUISSANCE~CODE_EDA+HORODATE,oa2,sum)
    oa4<-merge(oa3,dmo,by=c("CODE_EDA","HORODATE"))
    names(oa4)[names(oa4)=="CODE_EDA"]<-"CODE_ENTITE"
    oa4<-oa4[order(oa4$CODE_ENTITE,oa4$HORODATE,oa4$PUISSANCE),]
    avant<-rbind(c(NA,NA),oa4[-nrow(oa4),c("CODE_ENTITE","PUISSANCE")])
    apres<-rbind(oa4[-1,c("CODE_ENTITE","PUISSANCE")],c(NA,NA))
    oa5<-cbind(oa4[,c("HORODATE","CODE_ENTITE","PUISSANCE","DMO")],avant,apres)
    names(oa5)[5:8]<-c("CODE_ENTITE_avant","PUISSANCE_avant","CODE_ENTITE_apres","PUISSANCE_apres")
    debut<-oa5[oa5$PUISSANCE>0 & (oa5$PUISSANCE_avant==0 | oa5$CODE_ENTITE!=oa5$CODE_ENTITE_avant | 1:nrow(oa5)==1),]
    fin<-oa5[oa5$PUISSANCE>0 & (oa5$PUISSANCE_apres==0 | oa5$CODE_ENTITE!=oa5$CODE_ENTITE_apres | 1:nrow(oa5)==nrow(oa5)),]
    oa<-data.frame(CODE_ENTITE=debut$CODE_ENTITE,debut=debut$HORODATE,fin=fin$HORODATE+60,DMO=debut$DMO)
    
  }else{
    logprint(paste("Pas de fichier OA dans",dir,"\n"))
    oa<-data.frame(CODE_ENTITE=NA,debut=NA,fin=NA,DMO=NA)[0,]
  }
  #Fichiers de programme d'Effacement Retenus
  lfpec <- list.files(path = dir,pattern = "PEC_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv",full.names = TRUE)
  lfpec <- file.info(lfpec,extra_cols = TRUE)
  lfpec$Lien <- rownames(lfpec)
  rownames(lfpec) <- NULL
  
  #Horodate de cr?ation figurant dans le nom du fichier
  lfpec$HorodateCreation = as.POSIXct(x = gsub(x = lfpec$Lien, pattern = paste(dir,"PEC_GRD_[0-9]{8}_[0-9A-Z]{16}_([0-9]{14}).csv", sep="/"), replacement = "\\1",perl = TRUE), format = "%Y%m%d%H%M%S")
  
  #Journ?e d'effacement correspondante
  lfpec$JourEffacement = as.Date(x = gsub(x = lfpec$Lien, pattern = paste(dir,"PEC_GRD_([0-9]{8})_[0-9A-Z]{16}_[0-9]{14}.csv", sep="/"), replacement = "\\1",perl = TRUE), format = "%Y%m%d")
  
  #Tri des fichiers par Jour et horodate de cr?ation
  lfpec <- lfpec[order(lfpec$JourEffacement,lfpec$HorodateCreation, decreasing = TRUE),]
  
  #D?doublonnage des fichiers de CdC en selectionnant les plus r?cents par Jour d'effacement
  lfpec <- lfpec[!duplicated(lfpec[,c('JourEffacement')]),]
  
  #Fichiers non vides (sup?rieur ? 367 octets)
  lfpec <- lfpec[which(lfpec$size > 367),]
  
  pecs4<-data.frame(debut=NA,fin=NA,CODE_ENTITE=NA,DMO=NA)[0,]
  if(nrow(lfpec)>0){
    pecs<-pecs4
    names<-c("CODE_EDE","NB_PTS_CHRONIQUE",paste("VAL",1:50,sep=""))
    for(pec in lfpec$Lien){
      suppressWarnings(pec1<-read.csv2(pec,skip=2,fill=T))
      pec1[,names[!names %in% names(pec1)]]<-NA
      pec1<-pec1[,names]
      pec1$DATE<-substr(pec,regexpr("PEC_GRD",pec)+8,regexpr("PEC_GRD",pec)+15)
      if(pec==lfpec$Lien[1])pecs<-pec1 else pecs<-rbind(pecs,pec1)
    }
    pecs<-pecs[pecs$CODE_EDE!="<EOF>",]
    if(nrow(pecs)>0){
      pecs<-pecs[!duplicated(pecs[,c("CODE_EDE","DATE")]),]
      pecs2<-melt(pecs,measure.vars=names(pecs)[substr(names(pecs),1,3)=="VAL"])
      pecs2$variable2<-as.numeric(substr(as.character(pecs2$variable),4,nchar(as.character(pecs2$variable))))
      
      pecs2<-pecs2[pecs2$variable2<=pecs$NB_PTS_CHRONIQUE,]#Filtre sur les demi heures au del? de 48
      pecs2<-pecs2[order(pecs2$CODE_EDE,pecs2$DATE,pecs2$variable2),]
      
      pecs2$horodate<-as.POSIXct(as.character(pecs2$DATE),format="%Y%m%d")
      pecs2$horodate<-pecs2$horodate+(pecs2$variable2-1)*30*60
      pecs2$horodate30utc<-as.POSIXct(floor(as.numeric(as.POSIXct(format(pecs2$horodate,tz="UTC"),tz="UTC"))/1800)*1800,origin="1970-01-01",tz="UTC")
      
      avant<-rbind(c(NA,NA,NA),pecs2[-nrow(pecs2),c("CODE_EDE","value","horodate30utc")])
      apres<-rbind(pecs2[-1,c("CODE_EDE","value","horodate30utc")],c(NA,NA,NA))
      pecs3<-cbind(pecs2[,c("horodate30utc","horodate","CODE_EDE","value")],avant,apres)
      names(pecs3)[5:10]<-c("CODE_EDE_avant","value_avant","horodate30utc_avant","CODE_EDE_apres","value_apres","horodate30utc_apres")
      debut<-pecs3[pecs3$value>0 & (pecs3$value_avant==0 | pecs3$CODE_EDE!=pecs3$CODE_EDE_avant | 1:nrow(pecs3)==1 | as.POSIXct(pecs3$horodate30utc)!=as.POSIXct(pecs3$horodate30utc_avant)+30*60),]
      fin<-pecs3[pecs3$value>0 & (pecs3$value_apres==0 | pecs3$CODE_EDE!=pecs3$CODE_EDE_apres | 1:nrow(pecs3)==nrow(pecs3) | as.POSIXct(pecs3$horodate30utc)!=as.POSIXct(pecs3$horodate30utc_apres)-30*60),]
      
      #      avant<-rbind(c(NA,NA),pecs2[-nrow(pecs2),c("CODE_EDE","value")])
      #      apres<-rbind(pecs2[-1,c("CODE_EDE","value")],c(NA,NA))
      #      pecs3<-cbind(pecs2[,c("horodate","CODE_EDE","value")],avant,apres)
      #      names(pecs3)[4:7]<-c("CODE_EDE_avant","value_avant","CODE_EDE_apres","value_apres")
      #      debut<-pecs3[pecs3$value>0 & (pecs3$value_avant==0 | pecs3$CODE_EDE!=pecs3$CODE_EDE_avant | 1:nrow(pecs3)==1),]
      #      fin<-pecs3[pecs3$value>0 & (pecs3$value_apres==0 | pecs3$CODE_EDE!=pecs3$CODE_EDE_apres | 1:nrow(pecs3)==nrow(pecs3)),]
      pecs4<-data.frame(CODE_ENTITE=debut$CODE_EDE,debut=debut$horodate,fin=fin$horodate+30*60,DMO=rep(NA,nrow(debut)))
    }else{
      logprint(paste("Tous les fichiers PEC sont vides dans ",dir,"\n"))
    }
  }else{
    logprint(paste("Pas de fichier PEC dans",dir,"\n"))
  }
  effacements<-rbind(oa[,names(pecs4)],pecs4)
  logprint(paste(nrow(effacements),"effacements dans le dossier",dir))
  
  return(effacements)
}