# devtools::install_github("kassandra-ru/kassandr")
library(kassandr)

library(tidyverse) # data manipulation
library(rio) # import - export data
library(tsibble) # ts data frames
#library(fable) # forecasting models
library(forecast) # forecasting models
library(lubridate) # working with dates
library(glmnet) # lasso
library(naniar) # missing values visualization
# library(fasster) # fasster model
library(ranger) # random forest
library(stringr) # character variables
library(rlang) # шаманство с бум-бум!
library(readxl) # чтение экселевских файлов


Sys.setlocale("LC_TIME", "C")


## important!
# Do Session - Set working directory - to source file location

raw_data_folder = "../../data/raw/2019-04-04/"

data_snapshot_folder = "data_snapshot/"
dir.create(data_snapshot_folder)


# daily tsibble should contain date, access_date and more columns!
# the last (!) value for each month will be used (!)
daily_to_monthly = function(daily_tsibble) {
  daily_tsibble = mutate(daily_tsibble, date = lubridate::ymd(date), .year = year(date), .month = month(date))
  monthly_tsibble = arrange(daily_tsibble, date) %>% group_by(.year, .month) %>% filter(date == last(date))
  monthly_tsibble = ungroup(monthly_tsibble) %>% mutate(date = ymd(paste0(.year, "-", .month, "-01"))) %>% select(-.year, -.month)
  return(monthly_tsibble)
}

# file_name - vector of file names
# read all csv files and join them in a huge tsibble :)
join_ts_files = function(file_name) {
  all_data = tibble(file_name = file_name)
  all_data = mutate(all_data, data = map(file, ~ rio::import(.)))
  all_data = mutate(all_data, data = map(data, ~ dplyr::select(.x, -access_date)))
  
  all_frame = all_data$data %>% reduce(full_join, by = "date") %>% arrange(date) %>% mutate(date = yearmonth(ymd(date)))
  
  all_tsibble = as_tsibble(all_frame, index = date)
  return(all_tsibble)
}


# aggregate exchange rate: from dayly to monthly ----------------------------------------------------

exch_rate_daily = import(paste0(raw_data_folder, "exchangerate.csv"))
exch_rate = daily_to_monthly(exch_rate_daily)
exch_rate
export(exch_rate, file = paste0(data_snapshot_folder, "exchangerate_m.csv"))



# join all small files into big table -------------------------------------


file = c(paste0(raw_data_folder, c("1-03.csv", "1-08.csv", "1-11.csv",
                                   "i_ipc.csv", "ind_okved2.csv", "lendrate.csv", "m2-m2_sa.csv",
                                   "reserves.csv", "trade.csv", "urov_12kv.csv")), 
         paste0(data_snapshot_folder, "exchangerate_m.csv"))
file
rus_ts = join_ts_files(file)
export(rus_ts, file = paste0(data_snapshot_folder, "rus_monthly.csv"))



# create model table ------------------------------------------------------

model_list = tribble(~model, ~predicted, ~predictors, ~options, ~h,
                     "arima", "cpi", "", "", "1,2,3,4,5,6",
                     "ets", "ind_prod", "", "", "1,2,3,4,5,6",
                     "arima", "cpi", "", "order=c(1,0,1),seasonal=c(1,1,1),method='ML'", "1,2,3,4,5,6",
                     "var", "cpi+ind_prod", "", "p=5", "1,2,3,4,5,6",
                     "arima", "cpi", "lag2_ind_prod", "order=c(1,0,1),seasonal=c(1,1,1)", "1,2",
                     "ranger", "cpi", "lag2_ind_prod+trend_lin+FOURIER_M", "", "1,2",
                     "tbats", "cpi", "", "", "1,2,3,4,5,6")
model_list

# здесь можно дописать конструктор model_list который по h подбирает лаги
# типа lah0 = lag1 при h=1 и lag2 при h=2
# TODO: подумать, а надо ли оно

# если будут предикторы высокочастотные — разделять их в предикторах через | или типа того!


# acronyms act on equation(+) and options(?)
acronyms = tribble(~acronym, ~meaning,
                   "FOURIER_M", "s1_12+s2_12+s3_12+s4_12+s5_12+c1_12+c2_12+c3_12+c4_12+c5_12+c6_12",
                   "FOURIER_Q", "s1_4+c1_4+c2_4",
                   "TRENDS", "trend_lin+trend_root")

first_useful_date = ymd("2011-10-01") # all previous info will be ignored
forecast_from_date = ymd("2019-04-01") # we play in a forecaster at this moment of time
proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

window_type = "sliding" # "sliding" or "stretching" as called in tsibble
h_max = pull(model_list, h) %>% str_split(",") %>% unlist() %>% as.numeric() %>% max()

# forecasting_dot ---------------------------------------------------------

# здесь мы составляем список точек прогнозирования. 
# одна точка — это прогноз конкретной переменной на конкретную дату по конкретной модели с опциями

model_list_h = mutate(model_list, h = as.list(str_split(h, ","))) %>% unnest()

model_list_h

# STOPPED here

  
# augment_dataset ---------------------------------------------------------

# add h_max new lines at the end
# add a lot of lags for each variable, trend, fourier cos/sin
augment_tsibble_4_forecasting = function(original_tsibble, 
                                         lags = 0:(2*frequency(original_tsibble)), 
                                         h_max = 1) {
  
  message("Augmenting data set")
  augmented_tsibble = tsibble::append_row(original_tsibble, n = h_max)
  
  message("Add lags ", lags, " for each variable")
  
  var_names = setdiff(colnames(original_tsibble), c("date", "access_date"))
  augmented_tsibble = add_lags(augmented_tsibble, var_names, lags = lags)
  
  message("Adding trend and periodic coefficients")
  augmented_tsibble = add_fourier(augmented_tsibble) %>% add_trend()
  
  return(augmented_tsibble)
}


forecasters_tsibble = filter(rus_ts, date >= first_useful_date, date <= forecast_from_date)



wide_ts = augment_tsibble_4_forecasting(rus_ts, h_max = h_max)

  
  
  









# cpi univariate models -------------------------------------------------------

start_date = ymd("2011-10-01")

I_ipc = import("../../data/raw/2019-03-10/i_ipc.csv")
I_ipc_tsibble = mutate(I_ipc, date = yearmonth(date)) %>% as_tsibble(index = date)%>%rename(value = cpi)
rus_m_full_stable = filter(I_ipc_tsibble, date >= start_date)


rus_m_full_stable %>% tail()
rus_m_full_stable %>% head()


# cpi quality evaluation --------------------------------------------------

# TODO: rename test to eval

proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

nobs_full = nrow(rus_m_full_stable)
nobs_test = round(proportion_test * nobs_full)

window_type = "sliding" # "sliding" or "stretching" as in tsibble


dates_test = tail(rus_m_full_stable$date, nobs_test)



h_all = 1:6


# model should take: h, model_sample (multivariate tsibble)
# produce some model :)
# may ignore h


model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor, 
                           "ets_fun", TRUE, "uni_model_2_scalar_forecast", 
                           "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima11_fun", TRUE, "uni_model_2_scalar_forecast")
#                           "lasso_fun", FALSE, "lasso_2_scalar_forecast",
#                           "ranger_fun", FALSE, "ranger_2_scalar_forecast")




cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_m_full_stable)
cv_results_new = estimate_and_forecast(cv_results)

mae_table = calculate_mae_table(cv_results_new)

mae_table %>% tail()
write_csv(mae_table, "estimation_results/mae_table_cpi.csv")


# real forecasting....



# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_m_full_stable)

the_forecasts_new = estimate_and_forecast(the_forecasts)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
write_csv(only_numbers, path = "estimation_results/forecasts_cpi.csv")



# gdp univariate models -------------------------------------------------------

tab6b = import("../../data/raw/2019-03-10/tab6b.csv")
tab6b_tsibble = mutate(tab6b, date = yearquarter(date)) %>% as_tsibble(index = date)%>%rename(value = gdp_2016_price)
tab6b_tsibble = rename(tab6b_tsibble, gdp_real_2016_price = value) %>% mutate(gdp_rate = (gdp_real_2016_price - lag(gdp_real_2016_price, 4))/lag(gdp_real_2016_price, 4))
# из-за того, что берём лаг 4 шага назад первое наблюдение появляется довольно поздно :)

tab6b_tsibble %>% head()
tab6b_tsibble %>% tail()

start_date = ymd("2012-01-01")
rus_q_full_stable = tab6b_tsibble %>% rename(value = gdp_rate) %>% filter(date >= start_date)


# gdp quality evaluation --------------------------------------------------

# TODO: rename test to eval

proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

nobs_full = nrow(rus_q_full_stable)
nobs_test = round(proportion_test * nobs_full)

window_type = "sliding" # "sliding" or "stretching" as in tsibble

dates_test = tail(rus_q_full_stable$date, nobs_test)

h_all = 1:3


model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor, 
                           "ets_fun", TRUE, "uni_model_2_scalar_forecast", 
                           "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima11_fun", TRUE, "uni_model_2_scalar_forecast")



# TODO: exact ML in case where ARMA(1,1)-SARMA(1,1) fails

cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_q_full_stable)
cv_results_new = estimate_and_forecast(cv_results)
mae_table = calculate_mae_table(cv_results_new)

mae_table
write_csv(mae_table, "estimation_results/mae_table_gdp_rate_real.csv")

# real forecasting....


# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_q_full_stable)
the_forecasts_new = estimate_and_forecast(the_forecasts)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
only_numbers
write_csv(only_numbers, path = "estimation_results/forecasts_gdp_rate_real.csv")

# industrial product univariate models -------------------------------------------------------

start_date = ymd("2013-01-01")

ind_prod = import("../../data/raw/2019-03-10/ind_okved2.csv")
ind_prod_tsibble = mutate(ind_prod, date = yearmonth(date)) %>% as_tsibble(index = date)%>%rename(value = ind_prod)
rus_m_full_stable = filter(ind_prod_tsibble, date >= start_date)


rus_m_full_stable %>% tail()
rus_m_full_stable %>% head()

# industrial product quality evaluation --------------------------------------------------

# TODO: rename test to eval

proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

nobs_full = nrow(rus_m_full_stable)
nobs_test = round(proportion_test * nobs_full)

window_type = "sliding" # "sliding" or "stretching" as in tsibble


dates_test = tail(rus_m_full_stable$date, nobs_test)



h_all = 1:6


# model should take: h, model_sample (multivariate tsibble)
# produce some model :)
# may ignore h


model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor, 
                           "ets_fun", TRUE, "uni_model_2_scalar_forecast", 
                           "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima111_fun", TRUE, "uni_model_2_scalar_forecast")
#                           "lasso_fun", FALSE, "lasso_2_scalar_forecast",
#                           "ranger_fun", FALSE, "ranger_2_scalar_forecast")




cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_m_full_stable)
                              
cv_results_new = estimate_and_forecast(cv_results)

mae_table = calculate_mae_table(cv_results_new)

mae_table %>% tail()
write_csv(mae_table, "estimation_results/mae_table_ind_prod.csv")


# real forecasting....



# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_m_full_stable)

the_forecasts_new = estimate_and_forecast(the_forecasts)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
write_csv(only_numbers, path = "estimation_results/forecasts_ind_prod.csv")


# investments univariate models -------------------------------------------------------

start_date = ymd("2013-01-01")

ind_prod = import("../../data/raw/2019-03-10/ind_okved2.csv")
ind_prod_tsibble = mutate(ind_prod, date = yearmonth(date)) %>% as_tsibble(index = date)%>%rename(value = ind_prod)
rus_m_full_stable = filter(ind_prod_tsibble, date >= start_date)


rus_m_full_stable %>% tail()
rus_m_full_stable %>% head()

# industrial product quality evaluation --------------------------------------------------

# TODO: rename test to eval

proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

nobs_full = nrow(rus_m_full_stable)
nobs_test = round(proportion_test * nobs_full)

window_type = "sliding" # "sliding" or "stretching" as in tsibble


dates_test = tail(rus_m_full_stable$date, nobs_test)



h_all = 1:6


# model should take: h, model_sample (multivariate tsibble)
# produce some model :)
# may ignore h


model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor, 
                           "ets_fun", TRUE, "uni_model_2_scalar_forecast", 
                           "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima111_fun", TRUE, "uni_model_2_scalar_forecast")
#                           "lasso_fun", FALSE, "lasso_2_scalar_forecast",
#                           "ranger_fun", FALSE, "ranger_2_scalar_forecast")




cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_m_full_stable)

cv_results_new = estimate_and_forecast(cv_results)

mae_table = calculate_mae_table(cv_results_new)

mae_table %>% tail()
write_csv(mae_table, "estimation_results/mae_table_ind_prod.csv")


# real forecasting....



# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_m_full_stable)

the_forecasts_new = estimate_and_forecast(the_forecasts)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
write_csv(only_numbers, path = "estimation_results/forecasts_ind_prod.csv")

