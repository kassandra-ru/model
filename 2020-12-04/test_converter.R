library(rio)
indprod = rio::import("ind_baza_2018.xlsx", skip = 2, sheet = 1)
#indprod <- read_excel("ind_baza_2018.xlsx", skip = 2, sheet = 1)
access_date = Sys.Date()

indprod_vector = t(indprod[2, 3:ncol(indprod)])%>% stats::na.omit()%>%as.numeric()
# for periods before January, 2020 use old link for the data and start = c(2015, 1)
indprod_ts = stats::ts(indprod_vector, start = c(2015, 1), frequency = 12)
indprod_tsibble = tsibble::as_tsibble(indprod_ts)%>% dplyr::rename(date =index, ind_prod = value)
indprod_tsibble = dplyr::mutate(indprod_tsibble, date = tsibble::yearmonth(lubridate::ymd("2000-08-01") + months(0:(nrow(indprod) - 1))),
                                access_date = access_date)
export(indprod_tsibble, "ind_baza_2018.csv")

