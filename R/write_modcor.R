#' Fonction d'écriture des chroniques d'effacement ou report au format csv prévu dans les règles MA/NEBEF SI
#'
#' @param tbl_chron un tible dataframe contenant les chroniques d'effacement par site
#' @param lgl_all un booléen précisant si le fichier comporte tous les sites ou uniquement les sites en contrat CARD
#' @param char_path le répertoire de sauvegarde du fichier généré
#'
#' @import tidyverse
#' @return un fichier par mécanisme, semaine et pas de temps au format csv et conforme aux règles SI
#' @export
#'
#' @examples
write_modcor = function(tbl_chron, char_path, lgl_all = FALSE)
{
  if(nrow(tbl_chron)==0)
  { 
    return(0)
    
  }else{
    
    transmute(.data = unnest(tbl_chron,.preserve = ref) %>% dplyr::filter(lgl_all | TYPE_CONTRAT == 'CARD')
              , MECANISME
              , CODE_ENTITE
              , CODE_SITE
              , DATE_APP = format(DATE,'%Y%m%d')
              , NB_POINT = as.integer(difftime(time2 = as_datetime(DATE), time1 = ceiling_date(as_datetime(DATE), change_on_boundary = TRUE, unit = 'day'), units = 'secs'))%/%PAS
              , TIME = 1 + (60 * as.integer(hour(HEURE)))%/%(PAS / 60) + as.integer(minute(HEURE))%/%(PAS / 60)
              , VAL = round(x = CHRONIQUE, digits = 3)
              , PAS
              , SEMAINE = floor_date(x = DATE, unit = 'week', week_start = 6)
              , CODE_EIC_GRD
    ) %>%
      distinct() %>%
      {
        add_row(
          .data = .
          , TIME = seq_len(
            case_when(
              unique(.$PAS) == 300 ~ 300
              , unique(.$PAS) ==  600 ~ 150
              , unique(.$PAS) == 1800 ~ 50
              , TRUE ~ NA_real_
            )
          )
        )
      } %>%
      spread(key = TIME , value = VAL, fill = 0) %>%
      dplyr::filter(!is.na(MECANISME)) %>%
      rename_all(gsub, pattern = '^([0-9]+)$', replacement = 'VAL\\1') %>%
      arrange(CODE_EIC_GRD, MECANISME, SEMAINE, PAS, CODE_ENTITE, CODE_SITE, DATE_APP) %>%
      group_by(CODE_EIC_GRD, MECANISME, SEMAINE, PAS) %>%
      nest() %>%
      {
        sum(
          0
          , pmap_dbl(
            .l = list(eic = .$CODE_EIC_GRD, meca = .$MECANISME, sem = .$SEMAINE, chron = .$data)
            , .f = function(eic, meca, sem, chron, path = char_path)
            {
              char_file = paste0(meca,'_CRMODECORRIGE_',format(as_date(sem), '%Y%m%d'),'_',eic,'_',format(now(), '%Y%m%d%H%M%S'),'.csv')
              
              if(meca == 'NEBEF')
              {
                write_delim(
                  x = select(.data = chron, CODE_EDE = CODE_ENTITE, CODE_SITE, DATE_APP, NB_POINT, starts_with('VAL')) %>%
                    mutate_at(.vars = vars(starts_with('VAL')), .funs = function(x){format(x,decimal.mark = ",")})
                  , path = paste0(path, '/', char_file)
                  , delim = ';'
                  , na = ''
                  , col_names = TRUE
                  , append = FALSE
                )
              }else
              {
                if(meca == 'MA')
                {
                  write_delim(
                    x = select(.data = chron, CODE_EDA = CODE_ENTITE, CODE_SITE, DATE_APP, NB_POINT, starts_with('VAL')) %>%
                      mutate_at(.vars = vars(starts_with('VAL')), .funs = function(x){format(x, decimal.mark = ",")})
                    , path = paste0(path, '/', char_file)
                    , delim = ';'
                    , na = ''
                    , col_names = TRUE
                    , append = FALSE
                  )
                }
              }
              
              cat("\nFichier ", char_file, " sauvegardé\n")
              
              1
            }
          )
        )
      }
  }
}

