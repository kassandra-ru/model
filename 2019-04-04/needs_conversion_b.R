# Petr Garmider

# BB: здесь требуется ручное скачивание файла

# https://fred.stlouisfed.org/series/RBRUBIS
reer_convert <- function(path_to_source, access_data) {
  data <- rio::import("RBRUBIS.csv")
  data <- data.frame(data)

  data_vector <- as.numeric(stats::na.omit(t(data$RBRUBIS)))

  data_ts <- stats::ts(data_vector, start = c(1994, 1), freq = 12)

  data_tsibble <- tsibble::as_tsibble(data_ts) %>% dplyr::rename(date = index)

  data_tsibble <- dplyr::mutate(data_tsibble, access_date = access_date)
  return(data_tsibble)
}


# 
# Petr Garmider
# 
# m2 - http://www.cbr.ru/statistics/?PrtId=dkfs
# cbr.ru -> статистика -> Макроэкономическая финансовая статистика -> Денежно-кредитная и финансовая статистика -> Сезонно скорректированный ряд денежной массы (M2)
# BB: ok, не хватало прямой ссылки на файл
# 
# reer - https://fred.stlouisfed.org/series/RBRUBIS
# Нашел в поиске, как я понял этот показатель может посчитать любой участник. Данные актуальные.
# BB: не скачивается автоматически
# 
# reserves excluding gold - http://www.cbr.ru/hd_base/mrrf/mrrf_m/
#   cbr.ru -> Базы данных -> Международные резервы Российской Федерации -> Ежемесячные значения на начало отчетной даты
# BB: ok
# 
# retail_index (BAD) - http://www.gks.ru/wps/wcm/connect/rosstat_main/rosstat/ru/statistics/enterprise/retail/#
#   gks.ru -> официальная статистика -> Предпринимательство -> Оптовая торговля и товарные рынки
# 
# retail_index (good) - http://sophist.hse.ru/exes/tables/RTRD_M_I.htm 
# sophist.hse.ru -> СТАТИСТИЧЕСКИЕ РЯДЫ -> Производство товаров и услуг -> Розничная торговля -> Оборот розничной торговли -> Месячные показатели (RTRD_M_I)
# BB: не используем sophist





# Omelyusik


# RTS
get_rts_index <- function(access_date) {
  
  # BB: этот url не скачает данные через пару месяцев! :)
  url <- 'https://iss.moex.com/iss/engines/stock/markets/index/securities/RTSI/candles.csv?iss.only=history&interval=31&iss.reverse=true&from=1995-09-01&till=2019-01-01&iss.json=extended&callback=JSON_CALLBACK&lang=ru&limit=100&start=0&sort_order=TRADEDATE&sort_order_desc=desc&_=1548768931779'
  frame <- read.csv2(url, skip = 1)
  
  frame <- frame %>% select(open, close, high, low, value, begin)
  frame$begin <- as.Date(frame$begin, format = "%Y-%m-%d %H:%M:%S")
  frame$access_date <- Sys.Date()
  
  frame <- frame[,c(6, 7, 1, 2, 3, 4, 5)]
  colnames(frame) <- c('date', 'access_date', 'RTS_open', 'RTS_close', 'RTS_high', 'RTS_low', 'trade_vol_usd')
  
  frame <- frame[!find_duplicates(frame, index = date), ]
  ts_frame <- as_tsibble(frame, index = date)
  
  #write_csv(ts_frame, path = 'rts.csv')
  
  return(ts_frame)
}

# Commodity (oil)
get_oil_price <- function(access_date) {
  url <- 'https://www.theice.com/marketdata/reports/77'
  
  # Так как на сайте работают противороботные механизмы и ссылки с выбранным диапазоном дат и без него
  # совпадают, скачать данные можно только в ручном режиме (rvest не справляется) -- и далее обработать их
  # с помощью Excel.
  # BB: значит надо найти другой!!! Нам нужна автоматизация :)
  
  frame <- read.csv2('oil_price.csv')
  frame$Date <- as.Date(frame$Date, format = "%b %d, %Y")
  frame$access_date <- access_date
  
  frame <- frame[,c(1, 3, 2)]
  colnames(frame) <- c('date', 'access_date', 'oil_price')
  
  frame <- frame[!find_duplicates(frame, index = date), ]
  ts_frame <- as_tsibble(frame, index = date)
  
  #write_csv(ts_frame, path = 'oil.csv')
  
  return(ts_frame)
}



#' This script downloads macroeconomic variables
#' Dependencies: bdemeshev/cbr, dplyr
#'

# Maxim Alexeev



download_rar_unpack <- function(url, destfile) {
  download.file(url, destfile, method="auto", quiet=FALSE)
  command <- paste('unrar -o+ e', destfile, sep=' ')
  system(command)
}

download_rar_unpack('http://www.gks.ru/free_doc/doc_2018/bul_dr/trud/ors-2018-3kv.rar',
                    './empl_manuf.rar')


# из этого архива нам нужна только таблица Таб2.29.xls в одной из подпапок
# функция tab229_xls_convert конвертирует эксель но не распаковывает
#' Extracts the number of manufacturing employees from an xls Rosstat table
#'






