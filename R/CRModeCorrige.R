#' Title
#'
#' @param tbl_cdc
#' @param tbl_sites
#' @param tbl_eff
#' @param tbl_effhisto
#' @param tbl_entt
#' @param tbl_homol
#' @param tbl_indhist
#' @param tbl_prev
#' @import tidyverse
#' @import fuzzyjoin
#' @return
#' @export
#'
#' @examples
CRModeCorrige <- function(tbl_cdc, tbl_sites, tbl_eff, tbl_effhisto, tbl_entt, tbl_homol, tbl_indhist, tbl_prev){
  
  #récupération de la méthode de certification associée à un effacement
  test = fuzzy_left_join(
    x = mutate(tbl_eff, DATE = as_date(DEBUT,tz='CET'))
    , y = tbl_entt
    , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
    , match_fun = list(`==`,`==`,`>=`,`<`)
  ) %>%
    #(méthode du rectangle par défaut).
    replace_na(list(METHODE = 'RECTANGLE')) %>%
    transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO) %>%
    fuzzy_left_join(
      y = tbl_sites
      , by = c('MECANISME','CODE_ENTITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
      , match_fun = list(`==`,`==`,`>=`,`<`)
      #ajout des sites rattachés aux entités
    ) %>%
    transmute(MECANISME = MECANISME.x, CODE_ENTITE = CODE_ENTITE.x, METHODE, DATE, DEBUT = DEBUT.x, FIN = FIN.x, SIGNE, DMO, CODE_SITE, CAPA_MAX_H_SITE, TYPE_CONTRAT) %>%
    fuzzy_left_join(
      y = tbl_homol
      , by = c('MECANISME','CODE_SITE', 'DATE' = 'DEBUT', 'DATE' = 'FIN')
      , match_fun = list(`==`,`==`,`>=`,`<`)
    ) %>% # Un site ne peut être rattaché à une entité certifiée par des méthodes autres que RECTANGLE que s'il est homologué à ces méthodes
    dplyr::filter(!(METHODE.x != 'RECTANGLE' & METHODE.y != METHODE.x)) %>%
    transmute(
      MECANISME = MECANISME.x
      , CODE_ENTITE
      , CODE_SITE = CODE_SITE.x
      , CAPA_MAX_H_SITE
      , TYPE_CONTRAT
      , METHODE = METHODE.x
      #si la méthode est HISTORIQUE, la variante par défaut est MOY10J (utile ?)
      , VARIANTE = if_else(condition = METHODE.x == 'HISTORIQUE' & is.na(VARIANTE), true = 'MOY10J', false =  VARIANTE)
      , DEBUT = DEBUT.x
      , FIN = FIN.x
      , SIGNE
      , DMO
    ) %>%
    distinct() %>% #sites homologués à d'autres méthodes que le rectangle
    #extension des périodes d'effacement pour disposer des cdc nécessaires au calcul de la cdc de référence
    mutate(DEBUT_ETENDU = as_date(DEBUT,tz='CET') - if_else(METHODE == 'HISTORIQUE', days(60), days(1)), FIN_ETENDU = as_date(FIN,tz='CET') + days(2)) %>%
    dplyr::filter(METHODE =='RECTANGLE' & MECANISME == 'MA') %>%
    split(list(.$MECANISME, .$CODE_ENTITE, .$CODE_SITE, .$METHODE, .$DEBUT, .$FIN), drop = TRUE)
  
  
  results = 
    
    test[10:11] %>%
    {
      purrr::map_dfr(
        .x = .
        , list(y = tbl_cdc)
        , .f = function(x,y){
          
          mutate(y, DATE = as_date(HORODATE, tz = 'CET')) %>%
                                    dplyr::filter(
                                      MECANISME == x$MECANISME
                                      , CODE_ENTITE == x$CODE_ENTITE
                                      , CODE_SITE == x$CODE_SITE
                                      , DATE >= x$DEBUT_ETENDU
                                      , DATE < x$FIN_ETENDU
                                    )
          }
        #   , .f = function(tbl_prog, tbl_ts)
        #   {
        # 
        # tbl_ts = mutate(tbl_cdc, DATE = as_date(HORODATE, tz = 'CET')) %>%
        #   dplyr::filter(
        #     MECANISME == .$MECANISME
        #     , CODE_ENTITE == .$CODE_ENTITE
        #     , CODE_SITE == .$CODE_SITE
        #     , DATE >= .$DEBUT_ETENDU
        #     , DATE < .$FIN_ETENDU
        #   )
        # 
        # 
        #     if(nrow(tbl_ts)==0){
        #       
        #       warning(
        #         paste(
        #           capture.output(
        #             {
        #               cat('Courbe(s) de charge absente(s)\n Contrôle du réalisé impossible pour les activations suivantes :\n')
        #               print(tbl_prog, len = nrow(tbl_prog))
        #             }
        #           )
        #           , collapse = '\n')
        #       )
        #       
        #     }else{
        #       
        #       call(
        #         name = paste('CR', unique(tbl_prog[['METHODE']]), sep = '_') #application de la méthode de contrôle du réalisé de l'entité
        #         , eff = tbl_prog
        #         , cdc = tbl_ts
        #       ) %>%
        #         eval()
        #     }
        #   }
      )
    }
  
  #aggrégation des tbl_cdc par entité
  tbl_cdcagr = tbl_cdc %>%
    group_by(CODE_ENTITE,HORODATE,HORODATE_UTC) %>%
    summarise(PUISSANCE = sum(PUISSANCE)) %>%
    ungroup()
  
  if(nrow(entites_actives)==0)
  {
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
    pmap_dfr(
      .l = list(
        x = test_eff %>% split(.$CODE_ENTITE)
        , y = test_eff %>% split(.$CODE_ENTITE) %>% names()
      )
      , .f =
    )
    
  }
  
  tbl_cdccrmc1<-list()
  
  if(length(entites)==0){
    
    print("Aucune entite activee au cours de la semaine")
    
  }else{
    
    #2 Boucle sur les entites----
    for(i in 1:length(entites)){
      
      entite<-entites[i]
      tbl_cdcagr30e<-tbl_cdcagr[tbl_cdcagr$CODE_ENTITE==entite,]
      eff<-tbl_eff[tbl_eff$CODE_ENTITE==entite,]
      
      if(nrow(eff)>0 & nrow(tbl_cdcagr30e)>0){
        
        sites <- tbl_sites$CODE_SITE[tbl_sites$CODE_ENTITE==entite]
        
        #2a Application des methodes a la maille entite
        # if(substr(entite,1,3)!="EDE"){
        #
        #   tbl_cdcref <- CR_RectangleSimple(tbl_cdc=tbl_cdcagr30e,eff=eff)
        #   METHODE = "RECTANGLE_MA"
        #
        # }else{
        
        METHODE <- tbl_entt$METHODE[tbl_entt$CODE_ENTITE == entite][1]
        
        #Si la methode est "PREVISION" et qu'il n'y a aucune prevision alors "RECTANGLE"
        #Si la methode est "HISTORIQUE" et qu'il n'y a aucune variante alors "RECTANGLE"
        if(length(METHODE)==0 | is.na(METHODE))METHODE <- "RECTANGLE"
        if((METHODE == "HISTORIQUE" & length(which(tbl_homol$CODE_SITE %in% sites)) == 0) | (METHODE == "PREVISION" & length(which(tbl_prev$CODE_SITE %in% sites)) == 0))
        {            METHODE <- "RECTANGLE"          }
        if(METHODE=="RECTANGLE"){
          if(substr(entite,1,3)!="EDE"){
            tbl_cdcref <- CR_RectangleSimple(tbl_cdc=tbl_cdcagr30e,eff=eff)
            tbl_cdcagr30e$PUISSANCE_effacee <- tbl_cdcref$PUISSANCE - tbl_cdcagr30e$PUISSANCE
            tbl_cdcagr30e$PUISSANCE_effacee[tbl_cdcagr30e$PUISSANCE_effacee<0]<-0
          }else{
            tbl_cdcref<-CR_RectangleDouble(tbl_cdc=tbl_cdcagr30e,eff=eff)
            tbl_cdcagr30e$PUISSANCE_effacee <- tbl_cdcref$PUISSANCE - tbl_cdcagr30e$PUISSANCE
            tbl_cdcagr30e$SIGNE = tbl_cdcref$SIGNE
            
            tbl_cdcagr30e$PUISSANCE_effacee[tbl_cdcagr30e$PUISSANCE_effacee < 0 & tbl_cdcagr30e$SIGNE > 0] <- 0
            tbl_cdcagr30e$PUISSANCE_effacee[tbl_cdcagr30e$PUISSANCE_effacee > 0 & tbl_cdcagr30e$SIGNE < 0] <- 0
          }
          
        }
        
        tbl_indhistEff<-unique(substr(tbl_effhisto$DEBUT[tbl_effhisto$CODE_ENTITE==entite],1,10))#on invalide la journee de debut d'effacement
        
        tbl_cdcent<-tbl_cdc[tbl_cdc$CODE_ENTITE==entite,]
        tbl_cdcsites1<-list()
        
        logprint(paste(entite,METHODE,"\n"))
        
        for(j in 1:length(sites)){
          
          if(length(sites)<=10)logprint(paste(sites[j],METHODE,"\n"))
          
          tbl_cdcsit<-tbl_cdcent[tbl_cdcent$CODE_SITE==sites[j],]
          
          if(nrow(tbl_cdcsit)==0){
            
            print(paste("pas de courbes pour", sites[j]))
            
          }else{
            
            #2b Application des methodes a la maille site
            
            if(METHODE=="SITE_A_SITE"){
              tbl_cdcref<-CR_RectangleDouble(tbl_cdc=tbl_cdcsit,eff=eff)
            }
            
            if(METHODE=="PREVISION")
            {
              tbl_prevsit<-tbl_prev[tbl_prev$CODE_SITE == sites[j],]
              tbl_cdcref <- CR_PREVISION(tbl_prev = tbl_prevsit,eff = eff,tbl_cdc = tbl_cdcsit)
              if(length(tbl_cdcref)==0)logprint(paste("Pas de tbl_cdc de prevision pour le site", sites[j]," : application du rectangle \n"))
            }
            
            if(METHODE=="HISTORIQUE")
            {
              VARIANTE_HIST <- tbl_homol$VARIANTE_HIST[tbl_homol$CODE_SITE==sites[j]]
              if(length(VARIANTE_HIST) == 0) VARIANTE_HIST <- "MOY10J"
              
              tbl_cdcref <- CR_HISTORIQUE(tbl_cdc=tbl_cdcsit,eff=eff,VARIANTE_HIST=VARIANTE_HIST,DATE_INDISPO=c(tbl_indhistEff,tbl_indhist$DATE_INDISPO[tbl_indhist$CODE_SITE==sites[j]]))
              
            }
            
            if(METHODE=="RECTANGLE" | length(tbl_cdcref)==0){
              if(substr(entite,1,3)!="EDE"){
                tbl_cdcref <- CR_RectangleSimple(tbl_cdc=tbl_cdcsit,eff=eff)
              }else{
                tbl_cdcref<-CR_RectangleDouble(tbl_cdc=tbl_cdcsit,eff=eff)
              }
            }
            
            tbl_cdcsit$PUISSANCE_effacee<-tbl_cdcref$PUISSANCE-tbl_cdcsit$PUISSANCE
            
            tbl_cdcsit$SIGNE = tbl_cdcref$SIGNE
            
            tbl_cdcsit$PUISSANCE_effacee[tbl_cdcsit$PUISSANCE_effacee < 0 & tbl_cdcsit$SIGNE > 0] <- 0
            tbl_cdcsit$PUISSANCE_effacee[tbl_cdcsit$PUISSANCE_effacee > 0 & tbl_cdcsit$SIGNE < 0] <- 0
            
            tbl_cdcsites1[[j]]<-tbl_cdcsit
            
          }#/if tbl_cdc
        }#/for site
        tbl_cdcsites<- do.call("rbind",tbl_cdcsites1)
        
        #2c Filtrage sur les journees avec effacement pour la semaine consideree (les tbl_eff de l'historique n'interviennent pas ici)
        tbl_cdcsites <- tbl_cdcsites[substr(tbl_cdcsites$HORODATE,1,10) %in% substr(c(eff$DEBUT,eff$FIN-1),1,10),]
        
        #2d1 Agregation des tbl_eff-sites a la maille entite
        
        #tbl_cdcenteffagr<-aggregate(PUISSANCE_effacee~HORODATE_UTC+HORODATE,tbl_cdcsites,sum)
        tbl_cdcenteffagr<-aggregate(PUISSANCE_effacee~CODE_ENTITE+HORODATE_UTC+HORODATE,tbl_cdcsites,sum)#Plante si tbl_cdcsites vide suite au filtre
        
        if(METHODE %in% c("HISTORIQUE","PREVISION")) tbl_cdcagr30e<-tbl_cdcenteffagr
        
        #2d2 Recalage des volumes avec la capacite de l'EDE (exprimee en kW)
        if(substr(entite,1,3)=="EDE"){
          capa<-sum(tbl_sites$CAPA_MAX_H_SITE[tbl_sites$CODE_ENTITE==entite])
          tbl_cdcagr30e$PUISSANCE_effacee[abs(tbl_cdcagr30e$PUISSANCE_effacee)>capa]<- capa * sign(tbl_cdcagr30e$PUISSANCE_effacee)
        }
        
        #tbl_cdcenteffagr2<-merge(tbl_cdcenteffagr,tbl_cdcagr30e,by=c("HORODATE_UTC","HORODATE"))
        tbl_cdcenteffagr2<-merge(tbl_cdcenteffagr,tbl_cdcagr30e,by=c("CODE_ENTITE","HORODATE_UTC","HORODATE"))
        
        tbl_cdcenteffagr2$ratio<-tbl_cdcenteffagr2$PUISSANCE_effacee.y/tbl_cdcenteffagr2$PUISSANCE_effacee.x#on divise l'effacement a la maille entite par la somme des tbl_eff a la maille site
        tbl_cdcenteffagr2$ratio[tbl_cdcenteffagr2$PUISSANCE_effacee.x==0]<-1
        tbl_cdcsit2<-merge(tbl_cdcsites,tbl_cdcenteffagr2[,c("CODE_ENTITE","HORODATE_UTC","HORODATE","ratio")],by=c("CODE_ENTITE","HORODATE_UTC","HORODATE"))
        tbl_cdcsit2$PUISSANCE_effacee<-tbl_cdcsit2$PUISSANCE_effacee*tbl_cdcsit2$ratio
        tbl_cdccrmc1[[i]]<-tbl_cdcsit2[,c("CODE_ENTITE","CODE_SITE","HORODATE_UTC","HORODATE","PUISSANCE_effacee")]
      }#/if eff
    }#/for entite
  }
  tbl_cdccrmc<- do.call("rbind",tbl_cdccrmc1)
  return(tbl_cdccrmc)
}
