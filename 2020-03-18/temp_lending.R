convert_lendrate <- function(path_to_source = "http://www.cbr.ru/hd_base/mkr/mkr_monthes/",
                             access_date = Sys.Date()) {
  
  lendrate <- path_to_source %>%
    xml2::read_html() %>%
    rvest::html_nodes(xpath = '//*[@id="content"]/table[1]') %>%
    rvest::html_table()
  
  # observations are stored in reverse chronological order :)
  lendrate <- lendrate[[1]] %>% dplyr::as_tibble() %>% dplyr::arrange(-dplyr::row_number())
  
  colnames(lendrate) = c("date", "dur_1_day", "dur_2_7_days", "dur_8_30_days", "dur_31_90_days", "dur_91_180_days", "dur_181_plus_days")
  
  
  lendrate = dplyr::mutate_at(lendrate, dplyr::vars(dplyr::starts_with("dur")), ~ as_numeric_cyrillic(.))
  
  # we convert "сентябрь 2001" to "2001-09-01"
  # but dmy wants "мая" and not "май"
  lendrate = dplyr::mutate(lendrate, 
                           date = tsibble::yearmonth(lubridate::ymd("2000-08-01") + months(0:(nrow(lendrate) - 1))),
                           access_date = access_date)
  
  lendrate_tsibble <- tsibble::as_tsibble(lendrate, index = "date") 
  check_conversion(lendrate_tsibble)
  return(lendrate_tsibble)
}

