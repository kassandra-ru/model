# probably we need packrat (!) to make packages stable

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

#devtools::install_github("kassandra-ru/kassandr")
library(kassandr)



Sys.setlocale("LC_TIME", "C")
dir.create("estimation_results")

# gdp univariate models -------------------------------------------------------

tab6b = import("Data/gdp.csv")
tab6b_tsibble = mutate(tab6b, date = yearquarter(date)) %>% as_tsibble(index = date)%>%rename(value = gdp)
tab6b_tsibble = rename(tab6b_tsibble, gdp = value) %>% mutate(gdp_rate = (gdp - lag(gdp, 1))/lag(gdp, 1))
# из-за того, что берём лаг 4 шага назад первое наблюдение появляется довольно поздно :)

tab6b_tsibble %>% head()
tab6b_tsibble %>% tail()

start_date = ymd("2001-02-01")
rus_q_full_stable = tab6b_tsibble %>% rename(value = gdp_rate) %>% filter(date >= start_date)



# gdp quality evaluation --------------------------------------------------

# TODO: rename test to eval

proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

nobs_full = nrow(rus_q_full_stable)
nobs_test = round(proportion_test * nobs_full)

window_type = "sliding" # "sliding" or "stretching" as in tsibble

dates_test = tail(rus_q_full_stable$date, nobs_test)

h_all = 1:4


model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor,
                           "ets_fun", TRUE, "uni_model_2_scalar_forecast",
                           "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima101_101_fun", TRUE, "uni_model_2_scalar_forecast")



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

# # investment univariate models -------------------------------------------------------
#
# invest = import("../../data/raw/2020-11-07/invest.csv")
# invest_tsibble = mutate(invest, date = yearquarter(date)) %>% as_tsibble(index = date)
#
# invest_tsibble %>% head()
# invest_tsibble %>% tail()
#
# start_date = ymd("2012-01-01")
# invest_full_stable = filter( invest_tsibble, date >= start_date)

#
# # investment quality evaluation --------------------------------------------------
#
# # TODO: rename test to eval
#
# proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов
#
# nobs_full = nrow(invest_full_stable)
# nobs_test = round(proportion_test * nobs_full)
#
# window_type = "sliding" # "sliding" or "stretching" as in tsibble
#
# dates_test = tail(invest_full_stable$date, nobs_test)
#
# h_all = 1:4
#
#
# model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor,
#                            "ets_fun", TRUE, "uni_model_2_scalar_forecast",
#                            "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
#                            "arima_fun", TRUE, "uni_model_2_scalar_forecast",
#                            "arima101_010_fun", TRUE, "uni_model_2_scalar_forecast")
#
#
#
# # TODO: exact ML in case where ARMA(1,1)-SARMA(1,1) fails
#
# cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test,
#                                 window_type = window_type, series_data = invest_full_stable, target = "investment")
# cv_results_new = estimate_and_forecast(cv_results)
# mae_table = calculate_mae_table(cv_results_new)
#
# mae_table
# write_csv(mae_table, "estimation_results/mae_table_investment.csv")
#
# # real forecasting....
#
#
# # models in tibble version ------------------------------------------------
#
# the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = invest_full_stable, target = "investment")
# the_forecasts_new = estimate_and_forecast(the_forecasts)
#
# only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
# only_numbers
# write_csv(only_numbers, path = "estimation_results/forecasts_investment.csv")
#
