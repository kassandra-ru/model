library(tidyverse)
library(tsibble)
library(tidyr)
library(dplyr)
Sys.setlocale("LC_TIME","C")


path <- ('http://www.gks.ru/bgd/regl/b19_02/IssWWW.exe/Stg/d010/1-06-0.xlsx')


tab_convert <- function(path, access_date) {
  data <- rio::import(path)
  data_vector <- data[4:23, 3:6]  %>% t() %>% as.vector()
  colnames(data_vector) <- NULL
  data_ts <- stats::ts(data_vector, start = c(1999, 1), freq = 4)
  data_tsibble <- tsibble::as_tsibble(data_ts)%>% rename(date = index)
  #data_ts <- as.Date(data_tsibl)
  
  data_tsibble = dplyr::mutate(data_tsibble, access_date=access_date, date = as.Date(date))
  return(data_tsibble)
}

inv <- tab_convert(path,Sys.Date())
