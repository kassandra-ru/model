#### 
#1.Поменял ссылку на новую
#2.Сменил дату скачивания с 2013 на 2015 год


convert_ind_okved2_xlsx <- function(path_to_source = 
                                      "https://gks.ru/storage/mediabank/ind-baza-2018.xlsx", 
                                    access_date = Sys.Date()) {
  indprod <- rio::import(path_to_source, skip = 2, sheet = 1)
  indprod_vector <- t(indprod[2, 3:ncol(indprod)])
  
  indprod_ts <- stats::ts(indprod_vector, start = c(2015, 1), frequency = 12)
  indprod_tsibble <- tsibble::as_tsibble(indprod_ts)
  indprod_tsibble = dplyr::rename(indprod_tsibble, date = index, ind_prod = value)
  indprod_tsibble = dplyr::mutate(indprod_tsibble, access_date = access_date)
  check_conversion(indprod_tsibble)
  return(indprod_tsibble)
}
