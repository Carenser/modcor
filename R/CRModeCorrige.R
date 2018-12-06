CRModeCorrige <- function(cdc,perimetre,effacements,effacementshisto,ListeEntt,SitesHomol,IndHist,cdcPrev){

  cdcagr<-aggregate(PUISSANCE~CODE_ENTITE+HORODATE_UTC+HORODATE,cdc,sum)
  cdccrmc1<-list()
  entites<-unique(perimetre$CODE_ENTITE[perimetre$CODE_ENTITE %in% effacements$CODE_ENTITE])

  if(length(entites)==0){

    print("Aucune entite activee au cours de la semaine")

  }else{

    #2 Boucle sur les entites----
    for(i in 1:length(entites)){

      entite<-entites[i]
      cdcagr30e<-cdcagr[cdcagr$CODE_ENTITE==entite,]
      eff<-effacements[effacements$CODE_ENTITE==entite,]

      if(nrow(eff)>0 & nrow(cdcagr30e)>0){

        sites <- perimetre$CODE_SITE[perimetre$CODE_ENTITE==entite]

        #2a Application des methodes a la maille entite
        # if(substr(entite,1,3)!="EDE"){
        #
        #   cdcref <- CR_RectangleSimple(cdc=cdcagr30e,eff=eff)
        #   METHODE = "RECTANGLE_MA"
        #
        # }else{

        METHODE <- ListeEntt$METHODE[ListeEntt$CODE_ENTITE == entite][1]

        #Si la methode est "PREVISION" et qu'il n'y a aucune prevision alors "RECTANGLE"
        #Si la methode est "HISTORIQUE" et qu'il n'y a aucune variante alors "RECTANGLE"
        if(length(METHODE)==0 | is.na(METHODE))METHODE <- "RECTANGLE"
        if((METHODE == "HISTORIQUE" & length(which(SitesHomol$CODE_SITE %in% sites)) == 0) | (METHODE == "PREVISION" & length(which(cdcPrev$CODE_SITE %in% sites)) == 0))
        {            METHODE <- "RECTANGLE"          }
        if(METHODE=="RECTANGLE"){
          if(substr(entite,1,3)!="EDE"){
            cdcref <- CR_RectangleSimple(cdc=cdcagr30e,eff=eff)
            cdcagr30e$PUISSANCE_effacee <- cdcref$PUISSANCE - cdcagr30e$PUISSANCE
            cdcagr30e$PUISSANCE_effacee[cdcagr30e$PUISSANCE_effacee<0]<-0
          }else{
            cdcref<-CR_RectangleDouble(cdc=cdcagr30e,eff=eff)
            cdcagr30e$PUISSANCE_effacee <- cdcref$PUISSANCE - cdcagr30e$PUISSANCE
            cdcagr30e$SIGNE = cdcref$SIGNE

            cdcagr30e$PUISSANCE_effacee[cdcagr30e$PUISSANCE_effacee < 0 & cdcagr30e$SIGNE > 0] <- 0
            cdcagr30e$PUISSANCE_effacee[cdcagr30e$PUISSANCE_effacee > 0 & cdcagr30e$SIGNE < 0] <- 0
          }

        }

        IndHistEff<-unique(substr(effacementshisto$DEBUT[effacementshisto$CODE_ENTITE==entite],1,10))#on invalide la journee de debut d'effacement

        cdcent<-cdc[cdc$CODE_ENTITE==entite,]
        cdcsites1<-list()

        logprint(paste(entite,METHODE,"\n"))

        for(j in 1:length(sites)){

          if(length(sites)<=10)logprint(paste(sites[j],METHODE,"\n"))

          cdcsit<-cdcent[cdcent$CODE_SITE==sites[j],]

          if(nrow(cdcsit)==0){

            print(paste("pas de courbes pour", sites[j]))

          }else{

            #2b Application des methodes a la maille site

            if(METHODE=="SITE_A_SITE"){
              cdcref<-CR_RectangleDouble(cdc=cdcsit,eff=eff)
            }

            if(METHODE=="PREVISION")
            {
              cdcprevsit<-cdcPrev[cdcPrev$CODE_SITE == sites[j],]
              cdcref <- CR_Prev(cdcPrev = cdcprevsit,eff = eff,cdc = cdcsit)
              if(length(cdcref)==0)logprint(paste("Pas de CdC de prevision pour le site", sites[j]," : application du rectangle \n"))
            }

            if(METHODE=="HISTORIQUE")
            {
              VARIANTE_HIST <- SitesHomol$VARIANTE_HIST[SitesHomol$CODE_SITE==sites[j]]
              if(length(VARIANTE_HIST) == 0) VARIANTE_HIST <- "MOY10J"

              cdcref <- CR_Histo(cdc=cdcsit,eff=eff,VARIANTE_HIST=VARIANTE_HIST,DATE_INDISPO=c(IndHistEff,IndHist$DATE_INDISPO[IndHist$CODE_SITE==sites[j]]))

            }

            if(METHODE=="RECTANGLE" | length(cdcref)==0){
              if(substr(entite,1,3)!="EDE"){
                cdcref <- CR_RectangleSimple(cdc=cdcsit,eff=eff)
              }else{
                cdcref<-CR_RectangleDouble(cdc=cdcsit,eff=eff)
              }
            }

            cdcsit$PUISSANCE_effacee<-cdcref$PUISSANCE-cdcsit$PUISSANCE

            cdcsit$SIGNE = cdcref$SIGNE

            cdcsit$PUISSANCE_effacee[cdcsit$PUISSANCE_effacee < 0 & cdcsit$SIGNE > 0] <- 0
            cdcsit$PUISSANCE_effacee[cdcsit$PUISSANCE_effacee > 0 & cdcsit$SIGNE < 0] <- 0

            cdcsites1[[j]]<-cdcsit

          }#/if cdc
        }#/for site
        cdcsites<- do.call("rbind",cdcsites1)

        #2c Filtrage sur les journees avec effacement pour la semaine consideree (les effacements de l'historique n'interviennent pas ici)
        cdcsites <- cdcsites[substr(cdcsites$HORODATE,1,10) %in% substr(c(eff$DEBUT,eff$FIN-1),1,10),]

        #2d1 Agregation des effacements-sites a la maille entite

        #cdcenteffagr<-aggregate(PUISSANCE_effacee~HORODATE_UTC+HORODATE,cdcsites,sum)
        cdcenteffagr<-aggregate(PUISSANCE_effacee~CODE_ENTITE+HORODATE_UTC+HORODATE,cdcsites,sum)#Plante si cdcsites vide suite au filtre

        if(METHODE %in% c("HISTORIQUE","PREVISION")) cdcagr30e<-cdcenteffagr

        #2d2 Recalage des volumes avec la capacite de l'EDE (exprimee en kW)
        if(substr(entite,1,3)=="EDE"){
          capa<-sum(perimetre$CAPA_MAX_H_SITE[perimetre$CODE_ENTITE==entite])
          cdcagr30e$PUISSANCE_effacee[abs(cdcagr30e$PUISSANCE_effacee)>capa]<- capa * sign(cdcagr30e$PUISSANCE_effacee)
        }

        #cdcenteffagr2<-merge(cdcenteffagr,cdcagr30e,by=c("HORODATE_UTC","HORODATE"))
        cdcenteffagr2<-merge(cdcenteffagr,cdcagr30e,by=c("CODE_ENTITE","HORODATE_UTC","HORODATE"))

        cdcenteffagr2$ratio<-cdcenteffagr2$PUISSANCE_effacee.y/cdcenteffagr2$PUISSANCE_effacee.x#on divise l'effacement a la maille entite par la somme des effacements a la maille site
        cdcenteffagr2$ratio[cdcenteffagr2$PUISSANCE_effacee.x==0]<-1
        cdcsit2<-merge(cdcsites,cdcenteffagr2[,c("CODE_ENTITE","HORODATE_UTC","HORODATE","ratio")],by=c("CODE_ENTITE","HORODATE_UTC","HORODATE"))
        cdcsit2$PUISSANCE_effacee<-cdcsit2$PUISSANCE_effacee*cdcsit2$ratio
        cdccrmc1[[i]]<-cdcsit2[,c("CODE_ENTITE","CODE_SITE","HORODATE_UTC","HORODATE","PUISSANCE_effacee")]
      }#/if eff
    }#/for entite
  }
  cdccrmc<- do.call("rbind",cdccrmc1)
  return(cdccrmc)
}
