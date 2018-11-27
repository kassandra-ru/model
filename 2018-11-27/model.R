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

source(file = "functions.R")


Sys.setlocale("LC_TIME", "C")



# load data ---------------------------------------------------------------

cpi = import("data_snapshot/cpi_inflation.csv")
gdp = import("data_snapshot/gdp.csv")
gdp_deflator = import("data_snapshot/gdp_deflator.csv")
gdp_real_2016_price = import("data_snapshot/gdp_real_2016_price.csv")

rus_m = import("data_snapshot/russia_monthly.csv")
rus_m_info = import("data_snapshot/russia_monthly_info.csv")

rus_q = import("data_snapshot/russia_quarterly.csv")
rus_q_info = import("data_snapshot/russia_quarterly_info.csv")


# manually expect data ----------------------------------------------------

tail(cpi)
head(cpi)
glimpse(cpi)

tail(gdp)
head(gdp)
glimpse(gdp)

tail(rus_m)
head(rus_m)
glimpse(rus_m)

tail(rus_q)
head(rus_q)
glimpse(rus_q)

# data cleanup ------------------------------------------------------------

# remove NA and duplicates by coincidence :)

# better variable names

# character to dates
# standartize dates to yyyy-mm-dd

rus_q = tail(rus_q, -20)
# rus_m = na.omit(rus_m)

colnames(rus_q) = c("date", "gdp_nominal", "gdp_index", "household", "export", "import", "cpi", "ppi",
                    "reer", "ir", "public_expen", "m2", "reserves", "oil_inc", "population2", 
                    "real_wage", "employment", "population", "bicurrency", "y_real", "y_sa", "y_percap",
                    "y_index", "c_real", "c_sa", "c_percap", "c_index", "cpi_inflation", 
                    "reer_us", "reer_euro", "reer_weight", "bal_percap", "m_index", "infl_domestic",
                    "real_wage_sa", "oil_inc_alt", "oil_inc_percap", "oil_inc_alt2")

colnames(rus_m) = c("date", "empl_manuf", "ind_prod", "cpi_index", "ib_rate", "lend_rate", 
                    "real_income", "unempl", "ppi_index", "constr_nat", "inv_realcap", "real_wage",
                    "m2", "rts_index", "reer_cpi", "gas_price", "trade_balance", "reserves_nongold", 
                    "exch_rate_us", "worker_demand", "agric_index", 
                    "retail_index", "budget", "export", "import")

cpi = mutate(cpi, date = ymd(date))
gdp = mutate(gdp, date = ymd(date))
gdp_deflator = mutate(gdp_deflator, date = ymd(date))
gdp_real_2016_price = mutate(gdp_real_2016_price, date = ymd(date))

rus_m = mutate(rus_m, date = as_date(yearmonth(date))) 
rus_q = mutate(rus_q, date = yq(date)) 


glimpse(rus_q)
glimpse(rus_m)

rus_m = left_join(cpi, rus_m, by = "date")
rus_q = left_join(gdp, rus_q, by = "date")

gdp_deflator = rename(gdp_deflator, gdp_deflator = value)
gdp_real_2016_price = rename(gdp_real_2016_price, gdp_real_2016_price = value)
rus_q = full_join(rus_q, gdp_deflator, by = "date")
rus_q = full_join(rus_q, gdp_real_2016_price, by = "date")


rus_m = mutate(rus_m, date = yearmonth(date))
rus_q = mutate(rus_q, date = yearquarter(date))

rus_m = as_tsibble(rus_m, index = date)
rus_q = as_tsibble(rus_q, index = date)



# plots -------------------------------------------------------------------
head(rus_m)

cpi_m_ts = ts(rus_m$value, frequency = 12, start = c(1991, 1))
ggtsdisplay(cpi_m_ts)
ggseasonplot(cpi_m_ts)


vis_miss(rus_m)
vis_miss(rus_q)


# select full variables ---------------------------------------------------



rus_m_full = select(rus_m, real_income, unempl, constr_nat, real_wage, rts_index, agric_index, retail_index, value)
vis_miss(rus_m_full)

rus_q_full = select(rus_q, value, access_date, ir, reserves, real_wage, population, gdp_deflator, gdp_real_2016_price)
vis_miss(rus_q_full)

# cpi univariate models -------------------------------------------------------

start_date = ymd("2011-10-01")

rus_m_full_stable = filter(rus_m_full, date >= start_date)




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
                           "arima11_fun", TRUE, "uni_model_2_scalar_forecast",
                           "lasso_fun", FALSE, "lasso_2_scalar_forecast",
                           "ranger_fun", FALSE, "ranger_2_scalar_forecast")




cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_m_full_stable)
cv_res_models = estimate_nonduplicate_models(cv_results)
cv_results_new = fill_duplicate_models(cv_res_models, cv_results)
cv_results_new = add_point_forecasts(cv_results_new)
mae_table = calculate_mae_table(cv_results_new)




# real forecasting....



# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_m_full_stable)
the_forecasts_fitted = estimate_nonduplicate_models(the_forecasts)
the_forecasts_new = fill_duplicate_models(the_forecasts_fitted, the_forecasts)
the_forecasts_new = add_point_forecasts(the_forecasts_new)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
# write_csv(only_numbers, path = "forecasts.csv")



# gdp univariate models -------------------------------------------------------


rus_q_full = mutate(rus_q_full, gdp_rate = (gdp_real_2016_price - lag(gdp_real_2016_price, 4))/lag(gdp_real_2016_price, 4))
# из-за того, что берём лаг 4 шага назад первое наблюдение появляется довольно поздно :)

start_date = ymd("2012-01-01")
rus_q_full_stable = filter(rus_q_full, date >= start_date)

rus_q_full_stable = rename(rus_q_full_stable, gdp_nominal = value, value = gdp_rate)


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
cv_res_models = estimate_nonduplicate_models(cv_results)
cv_results_new = fill_duplicate_models(cv_res_models, cv_results)
cv_results_new = add_point_forecasts(cv_results_new)
mae_table = calculate_mae_table(cv_results_new)

mae_table
# write_csv(mae_table, "mae_table_gdp_rate_real.csv")

# real forecasting....


# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_q_full_stable)
the_forecasts_fitted = estimate_nonduplicate_models(the_forecasts)
the_forecasts_new = fill_duplicate_models(the_forecasts_fitted, the_forecasts)
the_forecasts_new = add_point_forecasts(the_forecasts_new)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
only_numbers
# write_csv(only_numbers, path = "forecasts_gdp_rate_real.csv")





