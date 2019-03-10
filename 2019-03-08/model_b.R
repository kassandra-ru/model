library(tidyverse) # data manipulation
library(rio) # import - export data
library(tsibble) # ts data frames
library(fable) # forecasting models
library(forecast) # forecasting models
library(lubridate) # working with dates
library(glmnet) # lasso
library(naniar) # missing values visualization
library(fasster) # fasster model
library(ranger) # random forest
library(stringr) # character variables
library(rlang) # шаманство с бум-бум!
library(readxl) # чтение экселевских файлов
library(tidyr) # 

library(kassandr)
# devtools::install_github("kassandra-ru/kassandr")
# devtools::install_github("tidyverts/fasster")
# devtools::install_github("tidyverts/fable")

Sys.setlocale("LC_TIME", "C")

# aggregate exchange rate: from dayly to monthly ----------------------------------------------------

exch_rate_dayly = import("data_snapshot/exchangerate.csv")
exch_rate_dayly %>% glimpse()
exch_rate_dayly = exch_rate_dayly %>% mutate(date = ymd(date), year = year(date), month = month(date))
exch_rate = exch_rate_dayly %>% arrange(date) %>% group_by(year, month) %>% summarise(exch_rate = last(exch_rate))
exch_rate = ungroup(exch_rate) %>% mutate(date = ymd(paste0(year, "-", month, "-01")),
                                          access_date = Sys.Date()) %>% select(-year, -month)

exch_rate
export(exch_rate, "data_snapshot/exchangerate_m.csv")

# load all monthly 

rus_m = tibble(file = c("1-03.csv", "1-08.csv", "1-11.csv", "exchangerate_m.csv", 
                  "i_ipc.csv", "ind_okved2.csv", "lendrate.csv", "m2-m2_sa.csv", 
                  "reserves.csv", "trade.csv", "urov_12kv.csv"))
rus_m = mutate(rus_m, data = map(file, ~ rio::import(paste0("data_snapshot/", .))))
rus_m = mutate(rus_m, data = map(data, ~ dplyr::select(.x, -access_date)))

rus_all = rus_m$data %>% reduce(full_join, by = "date") %>% arrange(date) %>% mutate(date = yearmonth(ymd(date)))

rus_ts = rus_all %>% as_tsibble(index = date)
rus_ts %>% export("rus_monthly.csv")



# cpi univariate models -------------------------------------------------------


fit = rus_ts %>% model(
  fasster = FASSTER(cpi ~ trig(12) + poly(1))
)



fit <- USAccDeaths %>% 
  as_tsibble %>% 
  model(fasster = FASSTER(value ~ poly(1) + trig(12)))
fit %>% summary


start_date = ymd("2011-10-01")

I_ipc = import("data_snapshot/i_ipc.csv")
I_ipc_tsibble = mutate(I_ipc, date = yearmonth(date)) %>% as_tsibble(index = date)
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

tab6b = import("data_snapshot/tab6b.csv")
tab6b_tsibble = mutate(tab6b, date = yearquarter(date)) %>% as_tsibble(index = date)
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





