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
#' @importFrom gtools na.replace
#' @return a dataframe with three columns : group, begin(included) and end(excluded)
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#' chron2prog(tbl_ts = ex_tbl.PEC, int_step = 1800)
#' chron2prog(ex_tbl.PER, int_step =  600)
chron2prog <- function(tbl_ts, int_step = 1800, char_group = 'groupe', char_pow = 'puissance', char_datetime = 'horodate_UTC')
{

  tbl_res = tbl_ts %>%
    rename(group = !!char_group, datetime = !!char_datetime, pow = !!char_pow) %>%
    mutate(sign=sign(pow)) %>%
    split(select(., group, sign)) %>%
    map(function(tbl_ts)
    {
      tbl_ts %>%
        filter(pow != 0) %>%
        arrange(datetime) %>%
        mutate(
          begin = difftime(datetime, lag(datetime),'UTC','secs')
          , end = rev(difftime(lag(rev(datetime)), rev(datetime),'UTC','secs'))
        ) %>%
        replace_na(replace = list(begin = int_step + 1, end = int_step + 1)) %>%
        mutate(
          begin = begin > int_step
          , end = end > int_step
        )
    }
    ) %>%
    map_df(rbind)

  #bourrin
  tbl_res = bind_cols(subset(tbl_res, end, 'group')
                      , subset(tbl_res, begin, 'datetime')
                      , subset(tbl_res, end, 'datetime')
                      , subset(tbl_res, end, 'sign')) %>%
    rename(begin = datetime, end = datetime1) %>%
    mutate(end = end + seconds(int_step)) %>%
    arrange(group, desc(end))

  return(tbl_res)
  # programme d'effacement borne de fin exclue
}
