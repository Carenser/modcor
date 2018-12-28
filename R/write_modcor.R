#' Fonction d'écriture des chroniques d'effacement ou report au format csv prévu dans les règles MA/NEBEF SI
#'
#' @param tbl_chron a tibble giving the load curtailement time series
#' @param char_path a repository to save comma separated file
#'
#' @import tidyverse
#' @import lubridate
#' @return un fichier par mécanisme, semaine et pas de temps au format csv et conforme aux règles SI
#' @export
#'
#' @examples
write_modcor = function(tbl_chron, char_path)
{
  transmute(tbl_chron
            , MECANISME
            , CODE_ENTITE
            , CODE_SITE
            , DATE_APP = format(DATE,'%Y%m%d')
            , NB_POINT = (as.numeric(days(1) + as_datetime(DATE_APP, tz='CET')) - as.numeric(as_datetime(DATE, tz='CET')))%/%PAS
            , TIME = 1 + (60 * as.integer(hour(HEURE)))%/%(PAS / 60) + as.integer(minute(HEURE))%/%(PAS / 60)
            , VAL = round(x = CHRONIQUE, digits = 3)
            , PAS
            , SEMAINE = floor_date(x = DATE, unit = 'week', week_start = 6)
            , CODE_EIC_GRD
  ) %>%
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
    split(list(.$CODE_EIC_GRD, .$MECANISME, .$SEMAINE, .$PAS), drop = TRUE) %>%
    map(
      .f = function(chron, path = char_path)
      {
        if(unique(chron$MECANISME) == 'NEBEF')
        {
          write_delim(
            x = select(.data = chron, CODE_EDE = CODE_ENTITE, CODE_SITE, DATE_APP, NB_POINT, starts_with('VAL')) %>%
              mutate_at(.vars = vars(starts_with('VAL')), .funs = function(x){format(x,decimal.mark = ",")})
            , path = str_c(path, paste0(unique(chron$MECANISME),'_CRMODECORRIGE_',format(unique(chron$SEMAINE), '%Y%m%d'),'_',unique(chron$CODE_EIC_GRD),'_',format(now(), '%Y%m%d%H%M%S'),'.csv'), sep='/')
            , delim = ';'
            , na = ''
            , col_names = TRUE
            , append = FALSE
          )
        }else
        {
          if(unique(chron$MECANISME) == 'MA')
          {
            write_delim(
              x = select(.data = chron, CODE_EDA = CODE_ENTITE, CODE_SITE, DATE_APP, NB_POINT, starts_with('VAL')) %>%
                mutate_at(.vars = vars(starts_with('VAL')), .funs = function(x){format(x,decimal.mark = ",")})
              , path = str_c(path, paste0(unique(chron$MECANISME),'_CRMODECORRIGE_',format(unique(chron$SEMAINE), '%Y%m%d'),'_',unique(chron$CODE_EIC_GRD),'_',format(now(), '%Y%m%d%H%M%S'),'.csv'), sep='/')
              , delim = ';'
              , na = ''
              , col_names = TRUE
              , append = FALSE
            )
          }
        }
      }
    )
}
