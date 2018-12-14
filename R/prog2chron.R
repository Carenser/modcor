#' Transpose load curtailment program to chronicle with datetime
#'
#' @param int_step integer value giving step between consecutive datetime values to return
#' @param char_group character string giving group name in programs tibble
#' @param char_pow character string giving power name in programs tibble
#' @param char_begin character string giving datetime begin name in UTC timezone from programs tibble
#' @param char_end character string giving datetime end name in UTC timezone from programs tibble
#' @param char_oml character string giving minutes of offer mobilization leadtime from programs tibble
#' @param tbl_prog tibble timeseries which contains load curtailment programs
#' @param timezone begin and end timezone from programs tibble
#'
#' @export
#'
#' @import tidyverse
#' @import lubridate
#' @return a dataframe with three columns : group, datetime_UTC and pow
#'
#' @examples
#' library(tidyverse)
#' library(lubridate)
#' prog2chron(tbl_prog = ex_tbl.OA, int_step = 300)
prog2chron <- function(tbl_prog, int_step = 300, char_group = 'groupe', char_pow = 'puissance',char_begin = 'debut', char_end = 'fin', char_oml = 'dmo')
{
  if(nrow(tbl_prog)==0)
    return(tibble::tibble(group = character(), datetime_UTC = as_datetime(numeric()), pow = double(), oml = integer()))
  
  tbl_prog %>%
    dplyr::rename(group = !!char_group, begin = !!char_begin, end = !!char_end, pow = !!char_pow, oml = !!char_oml) %>%
    {
      pmap_dfr(
        .l = list(
          group = .$group
          , begin = .$begin
          , end = .$end
          , pow = .$pow
          , oml = .$oml
        )
        , int_step = int_step
        , .f = function(group,begin,end,pow,oml,int_step){
          
          tibble(datetime_UTC = seq.POSIXt(from = with_tz(time = as_datetime(begin) ,tzone = 'UTC'), to = with_tz(time = as_datetime(end), tzone = 'UTC') - int_step, by = int_step)) %>%
            tibble::add_column(group = group,.before = 1) %>%
            tibble::add_column(pow = pow) %>%
            tibble::add_column(oml = oml)
        }
      )
    } %>%
    group_by(group,datetime_UTC) %>%
    summarise(pow = sum(pow), oml = max(oml)) %>%
    ungroup() %>%
    dplyr::arrange(group, desc(datetime_UTC))
}

