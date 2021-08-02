library(tidyverse) # ggplot2 + tibble + tidyr + dplyr + stringr + forcats
library(rio) # import - export data
library(fpp3) # tsibble + feasts + fable + lubridate
# library(forecast) # forecasting models # этот пакет не используется! вместо него tsibble!
library(glmnet) # lasso
library(naniar) # missing values visualization
# library(fasster) # fasster model
library(ranger) # random forest
library(progressr) # чтобы слайдер был :)
library(rlang) # шаманство с бум-бум!

devtools::install_github("kassandra-ru/kassandr") # если пакет не изменялся, установки не будет :)
library(kassandr)



Sys.setlocale("LC_TIME", "C")
dir.create("estimation_results")


# cpi univariate models -------------------------------------------------------

start_date = ymd("2011-10-01")

I_ipc = import("../../data/raw/2021-02-25/i_ipc.csv")
I_ipc_tsibble = mutate(I_ipc, date = yearmonth(date)) %>%
    as_tsibble(index = date)
rus_m_full_stable = filter(I_ipc_tsibble, date >= start_date)

# quick check!
rus_m_full_stable %>% tail()
rus_m_full_stable %>% head()
nobs_full = nrow(rus_m_full_stable)

cat('Total number of observations: ', nobs_full, '\n')
cat('That is ~', round(nobs_full / 12), ' years\n')

# cpi quality evaluation --------------------------------------------------

window_size = 12 * 7 # cross validation window length for sliding window
# for stretching window that will be initial length

cv_folds = slide_tsibble(rus_m_full_stable, .size = window_size, .step = 1)
# also possible stretch_tsibble

# h_all = 1:6

models = with_progress( # чтобы веселее ждать, можно и без индикатора прогресса :)
    model(cv_folds,
               ets = ETS(cpi),
               naive = NAIVE(cpi),
               snaive = SNAIVE(cpi),
               arima_auto = ARIMA(cpi),
               arima_101_101 = ARIMA(cpi ~ 1 + pdq(1, 0, 1) + PDQ(1, 0, 1))
    )
)

fcsts = forecast(models, h = 6)

# это шаманство просто добавляет значение h рядом с реальной датой,
# чтобы потом можно было усреднять качество прогнозов не только по моделям, но и по моделям + горизонт
fcsts = group_by(fcsts, .id, .model) %>% mutate(h = row_number()) %>% ungroup()

accuracy_table_detailed = accuracy(fcsts, rus_m_full_stable, by = c('.model', 'h'))
accuracy_table_gross = accuracy(fcsts, rus_m_full_stable, by = '.model')

accuracy_table_detailed
accuracy_table_gross

# TODO: stopped here
# real forecasting....



# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_m_full_stable)

the_forecasts_new = estimate_and_forecast(the_forecasts)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)

write_csv(only_numbers, path = "estimation_results/forecasts_cpi.csv")



# gdp univariate models -------------------------------------------------------

tab6b = import("../../data/raw/2021-03-29/tab6b.csv")
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

# industrial product univariate models -------------------------------------------------------

start_date = ymd("2015-01-01")

ind_prod = import("../../data/raw/2021-03-29/ind_baza_2018.csv")
ind_prod_tsibble = mutate(ind_prod[,c(1,3,4)], date = yearmonth(date)) %>% as_tsibble(index = date)%>%rename(value = ind_prod_perc_last_month)
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
                           "arima101_010_fun", TRUE, "uni_model_2_scalar_forecast")
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



# investment univariate models -------------------------------------------------------

invest = import("../../data/raw/2021-03-29/invest.csv")
invest_tsibble = mutate(invest, date = yearquarter(date)) %>% as_tsibble(index = date)

invest_tsibble %>% head()
invest_tsibble %>% tail()

start_date = ymd("2012-01-01")
invest_full_stable = filter( invest_tsibble, date >= start_date)


# investment quality evaluation --------------------------------------------------

# TODO: rename test to eval

proportion_test = 0.2 # доля ряда, используемая для оценки качества прогнозов

nobs_full = nrow(invest_full_stable)
nobs_test = round(proportion_test * nobs_full)

window_type = "sliding" # "sliding" or "stretching" as in tsibble

dates_test = tail(invest_full_stable$date, nobs_test)

h_all = 1:4


model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor,
                           "ets_fun", TRUE, "uni_model_2_scalar_forecast",
                           "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                           "arima101_010_fun", TRUE, "uni_model_2_scalar_forecast")



# TODO: exact ML in case where ARMA(1,1)-SARMA(1,1) fails

cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test,
                                window_type = window_type, series_data = invest_full_stable, target = "investment")
cv_results_new = estimate_and_forecast(cv_results)
mae_table = calculate_mae_table(cv_results_new)

mae_table
write_csv(mae_table, "estimation_results/mae_table_investment.csv")

# real forecasting....


# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = invest_full_stable, target = "investment")
the_forecasts_new = estimate_and_forecast(the_forecasts)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
only_numbers
write_csv(only_numbers, path = "estimation_results/forecasts_investment.csv")

