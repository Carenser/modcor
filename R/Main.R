# ### Modele Corrige - Enedis La F<brique Numerique - v2 - Janvier 2018
# #A PARAMETRER :
# #Mettre ? TRUE si vous souhaitez que toutes les courbes de charge CARD soient calcul?es (m?me ? nul, lorsqu'il n'y a pas d'effacement)
# #Mettre ? FALSE si vous souhaitez que seules les lignes ID*DATE concernees par un effacement ne soient calcul?es
# cdcnul<-FALSE
#
# ## Outils de calcul des chroniques d'effacement des sites soumis au modele corrige
#
# # DIR_DATA <- "c:/8-OutilsPRE/10-MA_NEBEF/7-ModeleCorrige/Outils GRD HP v0.x/MODELE_CORRIGE/data"
# # DIR_SCRIPT <- "c:/8-OutilsPRE/10-MA_NEBEF/7-ModeleCorrige/Outils GRD HP v0.x/MODELE_CORRIGE/script"
# # DIR_DATA <- "//2kze0.ze0.erd.edf.fr/ep/NEDI-PRONUM-FAB-PRE/COMMUN/8-OutilsPRE/10-MA_NEBEF/7-ModeleCorrige/Outils GRD v1/MODELE_CORRIGE/data"
# # DIR_SCRIPT <- "//2kze0.ze0.erd.edf.fr/ep/NEDI-PRONUM-FAB-PRE/COMMUN/8-OutilsPRE/10-MA_NEBEF/7-ModeleCorrige/Outils GRD v1/MODELE_CORRIGE/script"
#
# # ETAPE 0 - PARAMETRAGE & CHARGEMENT DES PACKAGES ET FONCTIONS ----------------------
#
# options(scipen = 14, stringsAsFactors = FALSE)
#
# #Stocke les arguments passes en parametres de la ligne de commande BATCH
# args <- commandArgs(trailingOnly = TRUE)
#
# #Recupere le parametre "Dossier" du lanceur BATCH
# DIR_DATA <- gsub(x = args[1], pattern = "\\\\", replacement = "/")
# DIR_SCRIPT <- gsub(x = args[2], pattern = "\\\\", replacement = "/")
#
# DIR_DATA <- paste0(getwd(),"/data-raw")
# DIR_SCRIPT <- paste0(getwd(),"/R")
#
# if(!dir.exists(paste(DIR_SCRIPT,"/log",sep="")))dir.create(paste(DIR_SCRIPT,"/log",sep=""))
# logfilename<-paste(DIR_SCRIPT,"/log/log",format(Sys.time(),"_%Y%m%d_%H%M%S.txt"),sep="")
# logprint<-function(string){
#   cat(string)
#   print(string)
#   cat("\n")
#   write.table(string,file = logfilename,append=T,row.names=FALSE,quote=FALSE,col.names = FALSE)
# }
#
# logprint(paste("TRAITEMENT DU MODELE CORRIGE",format(Sys.time(),"%Y-%m-%d %H:%M:%S")))
# logprint(paste("Traitement du dossier",DIR_DATA))
# logprint(paste("Scripts stockés dans ",DIR_SCRIPT))
#
# #Chargement des fonctions et librairies necessaires au traitement
# source(paste(DIR_SCRIPT,"LoadData.R",sep="/"))
# source(paste(DIR_SCRIPT,"CR_Rectangles.R",sep="/"))
# source(paste(DIR_SCRIPT,"CRModeCorrige.R",sep="/"))
# source(paste(DIR_SCRIPT,"chron2prog.R",sep="/"))
#
# library(reshape2)
# library(tidyverse)
# library(lubridate)
#
# #Fonction de scan d'une chaine de caractere
# #Entree : Une chaine de caractere, un caractere separateur, une position
# #Sortie : La sous-chaine de caractere correspondant a la position passe en parametre
# strscan <- function(x, split=" ", pos)
# {
#   return(sapply(X = strsplit(x = x,split = split), FUN = function(x){n = length(x);if(abs(pos)>n|pos == 0) stop("position incorrecte") ; if(pos<0) pos = pos%%n + 1 ; return(x[pos])}))
# }
#
# #Code EIC du GRD
# EIC <- gsub(x = grep(list.files(DIR_DATA,recursive = T),pattern = "[0-9]{6}/(MA_REFST_TLRLV_GRD|NEBEF_REFST_TLRLV_GRD)_[0-9]{6}_([0-9A-Z]{16})_[0-9]{14}.csv$",value = TRUE),pattern = "[0-9]{6}/(MA_REFST_TLRLV_GRD|NEBEF_REFST_TLRLV_GRD)_[0-9]{6}_([0-9A-Z]{16})_[0-9]{14}.csv$",replacement = "\\2")[1]
# if(is.na(EIC))EIC<-"17X9999999999999"
#
# # ETAPE 1 - SYNTHESE DES FICHIERS a TRAITER----
#
# #Liste des fichiers de CdC deposes dans les dossiers mensuels
# Fichiers <- data.frame(Lien = list.files(full.names = TRUE, include.dirs = TRUE, pattern = "(MA|NEBEF)_CRMODECORRIGE_[0-9]{6,8}_[0-9A-Z]{16}_[0-9]{14}.csv|CRMA_[0-9]{4}_[0-9]{8}_[0-9]{6}_[0-9]{8}.csv|NEBEF_CRS_GRD_[0-9]{8}_[0-9A-Z]{16}_[0-9]{14}.csv", path = DIR_DATA, recursive = TRUE))
# Fichiers$Nom <- strscan(x = as.character(Fichiers$Lien), split = '/', pos = -1)
# Fichiers$Dossier <- strscan(x = as.character(Fichiers$Lien), split = '/', pos = -2)
# Fichiers$Mecanisme <- gsub(x = strscan(x = Fichiers$Nom,split =  '_', pos =  1) , pattern = "CRMA", replacement = "MA")
# Fichiers$Type <- ifelse(test = strscan(Fichiers$Nom,split = '_',pos = 2) == "CRMODECORRIGE", yes = "CRMC", no = "CDC")
# Fichiers$Periode <- gsub(x = Fichiers$Nom, pattern = "(MA|NEBEF)_CRMODECORRIGE_([0-9]{6,8})_[0-9A-Z]{16}_[0-9]{14}.csv|CRMA_[0-9]{4}_[0-9]{8}_[0-9]{6}_([0-9]{8}).csv|NEBEF_CRS_GRD_([0-9]{8})_[0-9A-Z]{16}_[0-9]{14}.csv", replacement = "\\2\\3\\4")
# Fichiers$Source <- ifelse(test = nchar(Fichiers$Periode) == 6, yes = "RTE", no = "GRD")
# Fichiers$HorodateCreation <- as.POSIXct(x = gsub(x = gsub(x = Fichiers$Nom, pattern = "(MA|NEBEF)_CRMODECORRIGE_[0-9]{6,8}_[0-9A-Z]{16}_([0-9]{14}).csv|CRMA_[0-9]{4}_([0-9]{8}_[0-9]{6})_[0-9]{8}.csv|NEBEF_CRS_GRD_[0-9]{8}_[0-9A-Z]{16}_([0-9]{14}).csv", replacement = "\\2\\3\\4\\5"), pattern = "_", replacement = ""), format='%Y%m%d%H%M%S')
#
# #Filtre sur les dossiers correspondants a des mois (Cas eventuel de dossier 'archive')
# Fichiers <- Fichiers[grep(pattern = "20[0-9]{2}", x = Fichiers$Dossier),]
#
# #Tri des fichiers par type, mecanisme, Dossier (mois), periode et horodate de creation decroissant
# Fichiers <- Fichiers[order(Fichiers$Mecanisme, Fichiers$Dossier, Fichiers$Type, Fichiers$Periode, Fichiers$HorodateCreation, decreasing = TRUE),]
#
# #Dedoublonnage des fichiers de CdC en selectionnant les plus recents par mecanisme, Dossier, type, source et periode
# Fichiers <- Fichiers[!duplicated(Fichiers[,c('Mecanisme', 'Dossier', 'Type', 'Source' ,'Periode')]),]
#
# #Ajout d'un champ de contrôle de correspondance entre la periode du fichier et le mois du dossier
# Fichiers$PeriodeValide <- (Fichiers$Source =='GRD' & format(as.Date(Fichiers$Periode,format = "%Y%m%d")+6,"%Y%m") == Fichiers$Dossier) | (Fichiers$Source =='GRD' & format(as.Date(Fichiers$Periode,format = "%Y%m%d"),"%Y%m") == Fichiers$Dossier) |(Fichiers$Source =='RTE' & Fichiers$Periode == Fichiers$Dossier)
#
# #Filtre des fichiers avec periode correspondant au mois du dossier
# Fichiers <- Fichiers[which(Fichiers$PeriodeValide),]
#
# #Table de synthese des traitements a effectuer
# Synthese <- merge(y = Fichiers[which(with(data = Fichiers, Type == "CRMC" & Source == "GRD")),c('Mecanisme','Dossier','Periode','HorodateCreation')],
#                   x = Fichiers[which(Fichiers$Type == "CDC"),c('Lien','Mecanisme','Dossier','Periode','HorodateCreation')],by = c('Mecanisme','Dossier','Periode'), all.x = TRUE, suffixes = c("CDC",""))
#
# Synthese <- merge(y = Fichiers[which(with(data = Fichiers, Type == "CRMC" & Source == "RTE")),c('Mecanisme','Dossier','HorodateCreation')],
#                   x = Synthese ,by.x = c('Mecanisme','Dossier'),by.y = c('Mecanisme','Dossier'), all.x = TRUE, suffixes = c("CRMC_GRD","CRMC_RTE"))
# logprint("Fichiers stockés : ")
# logprint(Synthese)
#
#
# # ETAPE 2 - TRAITEMENT PAR MOIS ET PAR SEMAINE ----------------------------
#
# #Filtre de la table de synthese sur les periodes sans Chroniques d'effacement GRD ou RTE
# ATraiter <- Synthese[which(is.na(Synthese$HorodateCreationCRMC_GRD) & is.na(Synthese$HorodateCreationCRMC_RTE)),]
# logprint("Fichiers ? traiter : ")
# logprint(ATraiter)
# if(nrow(ATraiter)==0)logprint("Aucun fichier à traiter. Supprimez les fichiers 'MODECOR' et relancez le script")
#
# #Liste des mois existants
# Mois <- unique(ATraiter$Dossier)
# incDir=0
#
# #3-Boucle sur les semaines----
# for(mois in Mois){
#
#   incDir = incDir + 1
#   logprint(paste("Traitement du mois de ",unique(format(as.Date(paste(mois,'01'),"%Y%m %d"),"%B %Y ")),incDir,"/",length(Mois),"\n",sep=""))
#
#   #Lien complet vers le dossier du mois a traiter
#   dossier <- paste(DIR_DATA,"/", mois, sep='')
#
#   logprint(paste("TRAITEMENT DU DOSSIER",dossier))
#
#   #Liste des semaines du mois a traiter
#   EnCours <- ATraiter[ATraiter$Dossier == mois,]
#
#   #Chargement du perimetre du mois a traiter
#   Perimetre<-LoadPerimetre(dossiers = dossier)
#   Perimetre<-Perimetre[!duplicated(Perimetre[,c("CODE_ENTITE","CODE_SITE")]),]
#
#   #Chargement de la liste des EDE actives pour les methodes de CR associees
#   ListeEntt<-LoadListeEntt(dossiers = dossier)
#
#   #Chargement de la liste des sites homologues pour les variantes
#   SitesHomol <- LoadSitesHomol(dossiers = dossier)
#
#   #Chargement des jours d'indisponibilites
#   IndHist <- LoadIndHist(dossiers = dossier)
#
#   #Chargement des chroniques de previsions
#   cdcPrev <- LoadPrev(dossiers = dossier)
#
#   #S'il existe une entite avec la methode par historique dans le perimetre alors on charge les CdC du mois passe.
#   if(any(ListeEntt$CODE_ENTITE[which(ListeEntt$METHODE == 'HISTORIQUE')] %in% Perimetre$CODE_ENTITE))
#   {
#     #Chargement des effacements du mois en cours de traitement et du mois precedent
#     Effacements <- mapply(SIMPLIFY = TRUE, FUN = LoadEffacements,dossiers = paste(DIR_DATA,"/",format(seq.Date(from = as.Date(paste(mois,"01"),"%Y%m %d"),by = "-1 month",length.out = 2),"%Y%m"),sep=''))
#     #Pas d'historique du mois passes
#     if(typeof(Effacements[[2]])!="character"){
#
#       Effacements <- rbind.data.frame(Effacements[,2], Effacements[,1],deparse.level = 2,stringsAsFactors = FALSE)
#
#       }else
#     {
#       Effacements <- Effacements[[1]]
#     }
#
#     Effacements$DEBUT <- as.POSIXct(Effacements$DEBUT,origin="1970-01-01")
#     Effacements$FIN <- as.POSIXct(Effacements$FIN,origin="1970-01-01")
#
#   }else
#   {
#     #Chargement des effacements du mois en cours de traitement
#     Effacements <- LoadEffacements(dossiers = dossier)
#   }
#
#   #On ne garde que les effacements du perimetre du mois depose dans le dossier
#   Effacements <- Effacements[which(Effacements$CODE_ENTITE %in% unique(Perimetre$CODE_ENTITE)),]
#
#   #Liste des semaines a traiter parmi celles du mois en cours de traitement
#   Semaines <- unique(EnCours$Periode)
#
#   incSem = 0
#   for(semaine in Semaines){
#
#     incSem = incSem + 1
#     logprint(paste("Traitement de la semaine du ",format(as.Date(semaine,"%Y%m%d"),"%A %d %B %Y "),incSem,"/",length(Semaines),"\n",sep=""))
#
#     #Filtre sur les effacements de la semaine
#     effacements <- Effacements[as.Date(Effacements$FIN)>=as.Date(semaine,format="%Y%m%d") & as.Date(Effacements$DEBUT)<=as.Date(semaine,format="%Y%m%d")+6,]
#
#     #Filtre sur les previsions de la semaine
#     cdcPrevHebdo <- cdcPrev[as.Date(cdcPrev$HORODATE)>=as.Date(semaine,format="%Y%m%d") & as.Date(cdcPrev$HORODATE)<=as.Date(semaine,format="%Y%m%d")+6,]
#
#     if(nrow(effacements)>0){
#       historique<-Synthese[as.Date(Synthese$Periode,format="%Y%m%d")>=as.Date(semaine,format="%Y%m%d")-35
#                            & as.Date(Synthese$Periode,format="%Y%m%d")<=as.Date(semaine,format="%Y%m%d")
#                            & Synthese$Mecanisme %in% EnCours$Mecanisme[EnCours$Periode == semaine],c("Dossier","Lien")]##on charge a la fois les courbes de la semaine S en cours de traitement mais aussi jusqu'a S-5
#
#
#       cdcfilenames <- as.character(historique$Lien)
#       cdc <- LoadCdC(fichiers = cdcfilenames)
#
#       #VERIFICATION QUE TOUS LES SITES EFFACES ONT DES COURBES
#       cdc = mutate(.data = cdc, DATE = as_date(HORODATE, tz = 'CET'))
#
#       effacements = mutate(.data = effacements, DATE = as_date(DEBUT, tz = 'CET'))
#
#       perimeff<-merge(Perimetre[Perimetre$mecanisme %in% EnCours$Mecanisme,],effacements,by="CODE_ENTITE")
#       verifcdc<-merge(by=c("CODE_ENTITE","CODE_SITE","DATE"),all.y=T,cdc[!duplicated(cdc[,c("CODE_ENTITE","CODE_SITE","DATE")]),],perimeff)
#       cdcmqt<-verifcdc[is.na(verifcdc$PUISSANCE),c("CODE_ENTITE","CODE_SITE")]
#       if(nrow(cdcmqt)>0){
#         logprint(paste("Courbes introuvables ",paste(cdcmqt,collapse=", ")," - Entite supprimee",sep=""))
#         effacements<-effacements[!effacements$CODE_ENTITE %in% cdcmqt$CODE_ENTITE,]
#       }
#
#       #CALCULS DES CHRONIQUES D'EFFACEMENTS
#       cdccrmc1<-CRModeCorrige(cdc = cdc,perimetre = Perimetre,effacements = effacements,effacementshisto=Effacements,ListeEntt = ListeEntt,SitesHomol = SitesHomol,IndHist = IndHist, cdcPrev = cdcPrevHebdo)
#
#       if(length(cdccrmc1)<=1){
#         cdccrmc<-data.frame(CODE_ENTITE=NA,CODE_SITE=NA,DATE=NA,HORODATE_UTC=NA,HORODATE30=NA)
#       }else{
#         #Agregation des volumes au pas 30
#         cdccrmc1$HORODATE30utc<-as.POSIXct(floor(as.numeric(as.POSIXct(format(cdccrmc1$HORODATE,tz="UTC"),tz="UTC"))/1800)*1800,origin="1970-01-01",tz="UTC")
#         cdccrmc<-aggregate(PUISSANCE_effacee~CODE_ENTITE+HORODATE30utc+CODE_SITE,cdccrmc1,mean)
#         cdccrmc$HORODATE30<-as.POSIXct(format(cdccrmc$HORODATE30utc,tz="CET"))
#         cdccrmc$DATE<-format(cdccrmc$HORODATE30,"%Y%m%d")
#         cdccrmc<-cdccrmc[order(cdccrmc$CODE_ENTITE,cdccrmc$CODE_SITE,cdccrmc$HORODATE30utc,cdccrmc$HORODATE30),]
#         cdccrmc$PUISSANCE_effacee<-gsub(".",",",as.character(round(cdccrmc$PUISSANCE_effacee*1000)/1000),fixed=TRUE)
#         cdccrmc<-cdccrmc[cdccrmc$CODE_SITE %in% Perimetre$CODE_SITE[Perimetre$TYPE_CONTRAT=="CARD" & Perimetre$CATEGORIE=="SUP_36"],]#Filtre sur les sites CARD
#       }
#     }else{
#
#       cdccrmc<-data.frame(CODE_ENTITE=NA,CODE_SITE=NA,DATE=NA)
#
#     }
#     if(cdcnul==TRUE){
#       cdccrmct1<-aggregate(PUISSANCE~CODE_ENTITE+CODE_SITE+DATE,cdc,length)
#       cdccrmct1<-cdccrmct1[cdccrmct1$CODE_SITE %in% Perimetre$CODE_SITE[Perimetre$TYPE_CONTRAT=="CARD" & Perimetre$CATEGORIE=="SUP_36"],]
#       names(cdccrmct1)[names(cdccrmct1)=="PUISSANCE"]<-"NB_POINT"
#       cdccrmct1$NB_POINT<-round(cdccrmct1$NB_POINT/3)#passage cdc 10' à 30'
#       cdccrmct1$DATE<-gsub("-","",cdccrmct1$DATE)
#       logprint("Generation de toutes les courbes des sites CARD meme en l'absence d'effacement")
#     }else{
#       cdccrmct1<-unique(cdccrmc[,c("CODE_ENTITE","CODE_SITE","DATE")])
#       cdccrmc<-cdccrmc[!is.na(cdccrmc$CODE_ENTITE),]
#       cdccrmct1$NB_POINT<-NA
#       logprint("Generation uniquement des courbes des sites CARD en presence d'effacement")
#     }
#     cdccrmct1[,paste("VAL",1:48,sep="")]<-0
#     cdccrmct1[,paste("VAL",49:51,sep="")]<-""
#     cdccrmct1<-cdccrmct1[!is.na(cdccrmct1$CODE_ENTITE),]
#
#     #Exportation des chroniques d'effacements
#     for(meca in EnCours$Mecanisme[EnCours$Periode==semaine]){
#
#       filename<-paste(dossier,"/",meca,"_CRMODECORRIGE_",semaine,"_",EIC,"_",format(Sys.time(),"%Y%m%d%H%M%S"),".csv",sep="")
#       if(meca=="NEBEF"){
#         typeent<-"CODE_EDE"
#         cdccrmct<-cdccrmct1[substr(cdccrmct1$CODE_ENTITE,1,3)=="EDE",]
#         names=c(typeent,"CODE_SITE","DATE_APP","NB_POINT")
#       }else{
#         typeent<-"CODE_EDA"
#         cdccrmct<-cdccrmct1[substr(cdccrmct1$CODE_ENTITE,1,3)!="EDE",]
#         names=c(typeent,"CODE_SITE","DATE_APP","NB_PTS_CHRONIQUE")
#       }
#       write.table(paste(c(names,paste("VAL",1:50,sep=""),""),collapse=";"),file=filename,col.names=F,quote=F,row.names = F)
#
#       if(nrow(cdccrmc)>0){
#         entjrssit<-unique(cdccrmc[,c("CODE_ENTITE","CODE_SITE","DATE")])
#         if(meca=="NEBEF") entjrssit<-entjrssit[substr(entjrssit$CODE_ENTITE,1,3)=="EDE",]else entjrssit<-entjrssit[substr(entjrssit$CODE_ENTITE,1,3)!="EDE",]
#         names(cdccrmct)[4:54]<-c("NB_POINT",paste("VAL",1:50,sep=""))
#         incentjrssit=1
#         while(incentjrssit <= nrow(entjrssit)){
#           wy<-which(cdccrmct$CODE_ENTITE==entjrssit$CODE_ENTITE[incentjrssit] & cdccrmct$CODE_SITE==entjrssit$CODE_SITE[incentjrssit] & cdccrmct$DATE==entjrssit$DATE[incentjrssit])
#           w<-which(cdccrmc$DATE==entjrssit$DATE[incentjrssit] & cdccrmc$CODE_ENTITE==entjrssit$CODE_ENTITE[incentjrssit] & cdccrmc$CODE_SITE==entjrssit$CODE_SITE[incentjrssit])
#           cdccrmct[wy,47:50]<-""
#           cdccrmct[wy,5:(4+length(w))]<-cdccrmc$PUISSANCE_effacee[w]
#           cdccrmct$NB_POINT[wy]<-length(w)
#           incentjrssit=incentjrssit +1
#         }
#
#         write.table(x=cdccrmct,sep=";",quote=F,file=filename,append=TRUE,dec=",",col.names=F,row.names=F,na="")
#
#         }#/if cdccrmc
#       logprint(paste("creation du fichier :",filename))
#       cat(paste("creation du fichier :",filename,"\n"))
#     }#/for mecanisme
#   }#/for semaine
# }#/for dossier/mois
# logprint(paste("Fin du traitement",format(Sys.time(),"%Y-%m-%d %H:%M:%S")))
