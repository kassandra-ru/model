access_date = Sys.Date()
indprod = rio::import("ind_baza_2018.xlsx", skip = 2, sheet = 1)
indprod_vector = t(indprod[2, 3:ncol(indprod)])
indprod_ts = stats::ts(indprod_vector, start = c(2015, 1), frequency = 12)
indprod_tsibble = tsibble::as_tsibble(indprod_ts)
indprod_tsibble = dplyr::rename(indprod_tsibble, date = index, ind_prod = value)
indprod_tsibble = dplyr::mutate(indprod_tsibble, access_date = access_date)
rio::export(indprod_tsibble, "ind_baza_2018.csv")

# New data for 2020 are taken from: inv20.xlsx 
# Главная страница
# Статистика
# Официальная статистика
# Предпринимательство
# Инвестиции
# Инвестиции в нефинансовые активы
data = rio::import("1-06-0.xlsx")
names(data)[1] = "year_col"
# ниже для уровней
# ind_lvl_start = which(
#  data$year_col[c(1:length(data$year_col))] == "1.6. Инвестиции в основной капитал1), млрд рублей")
# ind_lvl_finish = which(
#  data$year_col[c(1:length(data$year_col))] == "в % к соответствующему периоду предыдущего года")
# idx_start = ind_lvl_start + 2
# idx_finish = ind_lvl_finish - 1

idx_not_year_start = which(
  data$year_col[c(1:length(data$year_col))] == "/ percent of corresponding period of previous year"
)
idx_not_year_finish = which(
  data$year_col[c(1:length(data$year_col))] == "/ percent of previous period"
)
idx_start = idx_not_year_start + 1
idx_finish = idx_not_year_finish - 2

data_vector = data[idx_start:idx_finish, 3:6] %>% t() %>% as.vector()
colnames(data_vector) = NULL
data_vector = stats::na.omit(data_vector)
data_ts = stats::ts(data_vector, start = c(1999, 1), freq = 4)
data_tsibble = tsibble::as_tsibble(data_ts)
data_tsibble = dplyr::mutate(data_tsibble, access_date = access_date)
data_tsibble = dplyr::rename(data_tsibble, date = index, investment = value)
rio::export(data_tsibble, "invest.csv")

data = rio::import("i_ipc.xlsx")
  data = data[5:16, -1]
  data = tidyr::gather(data, year, value)
  data1 = dplyr::select(data, -year)
  cpi_ts = stats::ts(data1$value, start = c(1991, 1), freq = 12)
  cpi_infl = tsibble::as_tsibble(cpi_ts) %>% stats::na.omit() %>% dplyr::rename(date = index, cpi = value)
  
  data_tsibble = dplyr::mutate(cpi_infl, access_date = access_date)
  rio::export(data_tsibble, "i_ipc.csv")
  





