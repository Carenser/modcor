#' Transpose load curtailment chronicle to program with begin and end datetimes
#'
#' @param int_step integer value giving step between consecutive datetime values
#' @param char_group character string giving group name in timeseries tibble
#' @param char_pow character string giving power name in timeseries tibble
#' @param char_datetime character string giving datetime name in UTC timezone from timeseries tibble
#' @param tbl_ts tibble timeseries which contains load curtailment chronicle
#'
#' @export
#'
#' @import tidyverse
#' @import lubridate
#' @return a dataframe with three columns : group, begin(included) and end(excluded)
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#' chron2prog(tbl_ts = ex_tbl.PEC, int_step = 1800)
#' chron2prog(ex_tbl.PER, int_step =  600)
chron2prog <- function(tbl_ts, int_step = 1800, char_group = 'groupe', char_pow = 'puissance', char_datetime = 'horodate_UTC', char_oml = 'dmo')
{
  
  if(nrow(tbl_ts)==0)
  {
    tibble::tibble(group = character(), begin = as_datetime(numeric()), end = as_datetime(numeric()), sign = integer(), oml = integer())
    
  }else{
    
    tbl_ts %>%
      dplyr::rename(group = !!char_group, datetime = !!char_datetime, pow = !!char_pow, oml = !!char_oml) %>%
      dplyr::mutate(sign=sign(pow)) %>%
      split(select(., group, sign)) %>%
      purrr::map_dfr(
        .f = function(x)
        {
          dplyr::filter(x, pow != 0) %>%
            dplyr::arrange(datetime) %>%
            dplyr::mutate(
              begin = difftime(datetime, dplyr::lag(datetime),'UTC','secs')
              , end = rev(difftime(dplyr::lag(rev(datetime)), rev(datetime),'UTC','secs'))
            ) %>%
            tidyr::replace_na(replace = list(begin = int_step + 1, end = int_step + 1)) %>%
            dplyr::mutate(
              begin = begin > int_step
              , end = end > int_step
            )
        }
      ) %>%
      {
        dplyr::bind_cols(
          subset(., end, 'group')
          , subset(., begin, 'datetime')
          , subset(., end, 'datetime')
          , subset(., end, 'sign')
          , subset(., begin, 'oml')
        ) %>%
          dplyr::rename(begin = datetime, end = datetime1) %>%
          dplyr::mutate(end = end + seconds(int_step)) %>%
          dplyr::arrange(group, desc(end))
      }
  }
}
