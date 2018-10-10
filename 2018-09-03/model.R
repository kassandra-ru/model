# install dev packages ----------------------------------------------------

# devtools::install_github("tidyverts/fable")

# attach packages -----------------------------------------------------------

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

Sys.setlocale("LC_TIME", "C")

# load data ---------------------------------------------------------------

cpi = import("../2018-08-31/data_snapshot/cpi_inflation.csv")
gdp = import("../2018-08-31/data_snapshot/gdp.csv")
gdp_deflator = import("../2018-09-27/data_snapshot/gdp_deflator.csv")
gdp_real_2016_price = import("../2018-09-27/data_snapshot/gdp_real_2016_price.csv")

rus_m = import("../2018-08-31/data_snapshot/russia_monthly.csv")
rus_m_info = import("../2018-08-31/data_snapshot/russia_monthly_info.csv")

rus_q = import("../2018-08-31/data_snapshot/russia_quarterly.csv")
rus_q_info = import("../2018-08-31/data_snapshot/russia_quarterly_info.csv")


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


extract_value = function(model_sample) {
  y = model_sample %>% select(value) %>% as.ts()
  return(y)
}

ets_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = ets(y)
  return(model)
}


arima_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = Arima(y)
  return(model)
}

arima11_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = Arima(y, order = c(1, 0, 1), seasonal = c(1, 0, 1))
  return(model)
}

tbats_fun = function(model_sample, h) {
  # h is ignored!
  y = extract_value(model_sample)
  model = tbats(y)
  return(model)
}

forecast_2_scalar = function(forecast_object, h = 1) {
  y_hat = forecast_object$mean[h]
  return(y_hat)
}

uni_model_2_scalar_forecast = function(model, h = 1, model_sample = NA) {
  # model_sample is unused in univariate models
  forecast_object = forecast(model, h = h)
  y_hat = forecast_2_scalar(forecast_object, h = h)
  return(y_hat)
}


# make augmented tsibble ------------------------------------------------------------

# input: tsibble
# output: tsibble


add_fourier = function(original, K_fourier = Inf) {
  original_ts = as.ts(original)
  freq = frequency(original)
  K_fourier = min(floor(freq/2), K_fourier)
  
  X_fourier = fourier(original_ts, K = K_fourier)
  fourier_names = colnames(X_fourier)
  fourier_names = str_replace(fourier_names, "-", "_") %>% str_to_lower()
  colnames(X_fourier) = fourier_names
  X_fourier_tibble = as_tibble(X_fourier)
  
  augmented = bind_cols(original, X_fourier_tibble)
  return(augmented)
}

add_trend = function(original) {
  nobs = nrow(original)
  augmented = mutate(original, trend_lin = 1:nobs, trend_root = sqrt(1:nobs))
  return(augmented)
}



# works only for one variable (without quotes)
add_lags = function(original, variable, lags = c(1, 2)) {
  variable = enquo(variable)
  variable_name = quo_name(variable)
  for (lag in lags) {
    new_variable_name = paste0("lag", lag, "_", variable_name)
    original = mutate(original, !!new_variable_name := lag(!!variable, lag))
  }
  return(original)
}


# works for many quoted variables
add_lags2 = function(original, variable_names, lags = c(1, 2)) {
  for (variable_name in variable_names) {
    for (lag in lags) {
      new_variable_name = paste0("lag", lag, "_", variable_name)
      new_value = original %>% pull(variable_name) %>% lag(lag)
      original = mutate(original, !!new_variable_name := new_value)
    }
  }
  return(original)
}



get_last_date = function(original) {
  date_variable = index(original)
  date = original %>% pull(!!date_variable) 
  # interval = pull_interval(date)
  last_date = max(date)
  return(last_date)
}



# forecast for h using lasso ----------------------------------------------------------


augment_tsibble_4_regression = function(original, h = 1) {
  frequency = frequency(original)
  augmented = original %>% append_row(n = h) %>% 
    add_trend() %>% add_fourier() %>% 
    add_lags(value, lags = c(h, h + 1, frequency, frequency + 1))
  regressor_names = setdiff(colnames(original), c("value", "date"))
  augmented = augmented %>% add_lags2(regressor_names, lags = c(h, h + 1, frequency, frequency + 1))
  augmented = augmented %>% select(-!!regressor_names)
  return(augmented)
}


lasso_augmented_estimate = function(augmented, seed = 777) {
  yX_tsibble = na.omit(augmented)
  y = yX_tsibble %>% pull("value")
  X = as_tibble(yX_tsibble) %>% select(-value, -date) %>% as.matrix()
  
  set.seed(seed)
  lasso_model = cv.glmnet(X, y)
  return(lasso_model)
}



ranger_augmented_estimate = function(augmented, seed = 777) {
  yX_tsibble = na.omit(augmented)
  
  set.seed(seed)
  ranger_model = ranger(data = yX_tsibble, value ~ . - date)
  return(ranger_model)  
}



lasso_fun = function(model_sample, h = 1) {
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  model = lasso_augmented_estimate(augmented_sample)

  return(model)
}

ranger_fun = function(model_sample, h = 1) {
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  model = ranger_augmented_estimate(augmented_sample)
  
  return(model)
}





lasso_2_scalar_forecast = function(model, h = 1, model_sample, s = c("lambda.min", "lambda.1se")) {
  s = match.arg(s)
  
  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  yX_future_tsibble = tail(augmented_sample, 1)
  X_future = as_tibble(yX_future_tsibble) %>% select(-value, -date) %>% as.matrix()
  
  point_forecast = predict(model, X_future, s = s)
  
  return(point_forecast)
}


ranger_2_scalar_forecast = function(model, h = 1, model_sample, seed = 777) {

  augmented_sample = augment_tsibble_4_regression(model_sample, h = h)
  yX_future_tsibble = tail(augmented_sample, 1)

  ranger_pred = predict(model, data = yX_future_tsibble)
  point_forecast = ranger_pred$predictions
  
  return(point_forecast)
}






model_fun_tibble = tribble(~model_fun, ~h_agnostic, ~forecast_extractor, 
                          "ets_fun", TRUE, "uni_model_2_scalar_forecast", 
                          "tbats_fun", TRUE, "uni_model_2_scalar_forecast",
                          "arima_fun", TRUE, "uni_model_2_scalar_forecast",
                          "arima11_fun", TRUE, "uni_model_2_scalar_forecast",
                          "lasso_fun", FALSE, "lasso_2_scalar_forecast",
                          "ranger_fun", FALSE, "ranger_2_scalar_forecast")
                          

# for quality evaluation
prepare_model_list = function(h_all = 1, model_fun_tibble, series_data, dates_test, window_type = "sliding") {
  model_list = crossing(date = dates_test, h = h_all, model_fun = model_fun_tibble$model_fun)
  model_list = left_join(model_list, select(series_data, value), by = "date")

  model_list = mutate(model_list, train_end_date = date - months(h * 12 / frequency(series_data)))
  
  full_sample_start_date = min(series_data$date)
  full_sample_last_date = max(series_data$date)
  test_sample_start_date = min(model_list$date)
  window_min_length = round(interval(full_sample_start_date, test_sample_start_date) /  months(12 / frequency(series_data))) - max(h_all) + 1
  
  
  if (window_type == "stretching") {
    model_list = mutate(model_list, train_start_date = min(pull(series_data, date)))
  } else {
    # sliding window case
    model_list = mutate(model_list, train_start_date = train_end_date - months((window_min_length - 1) * 12 / frequency(series_data) ))
  }
  
  model_list = mutate(model_list, 
                      train_sample = pmap(list(x = train_start_date, y = train_end_date), 
                                          ~ filter(series_data, date >= .x, date <= .y)))
  
  
  # we estimate some models only with maximal h -----------------------------------
  
  model_list = left_join(model_list,  model_fun_tibble, by = "model_fun")
  
  model_list = model_list %>% group_by(train_end_date, train_start_date, model_fun) %>%
    mutate(duplicate_model = h_agnostic & (h < max(h))) %>% ungroup()
  
  return(model_list)
}




# for forecasts
prepare_model_list2 = function(h_all = 1, model_fun_tibble, series_data) {
  
  full_sample_last_date = as.Date(max(series_data$date))
  full_sample_start_date = as.Date(min(series_data$date))
    
  model_list = crossing(h = h_all, model_fun = model_fun_tibble$model_fun)
  model_list = mutate(model_list, date = full_sample_last_date + months(h * 12 / frequency(series_data)))
  model_list = mutate(model_list, train_end_date = full_sample_last_date)
  model_list = mutate(model_list, train_start_date = full_sample_start_date)
  
  
  model_list = mutate(model_list, 
                         train_sample = pmap(list(x = train_start_date, y = train_end_date), 
                                             ~ filter(series_data, date >= .x, date <= .y)))
  
  
  # we estimate some models only with maximal h -----------------------------------
  
  model_list = left_join(model_list,  model_fun_tibble, by = "model_fun")
  
  model_list = model_list %>% group_by(train_end_date, train_start_date, model_fun) %>%
    mutate(duplicate_model = h_agnostic & (h < max(h))) %>% ungroup()
  return(model_list)
}


# TODO: reconsider two prepare_model functions



# models in tibble version ------------------------------------------------


estimate_nonduplicate_models = function(model_list, store_models = c("tibble", "file")) {
  store_models = match.arg(store_models)
  
  if (store_models == "file") {
    stop("File storage of models not implemented yet")
  }
  
  model_list_half_fitted = filter(model_list, !duplicate_model)
  model_list_half_fitted = model_list_half_fitted %>% mutate(
      fitted_model = pmap(list(train_sample, h, model_fun), ~ do.call(..3, list(h = ..2, model_sample = ..1)))
    )
  return(model_list_half_fitted)
}


# fill duplicate models ---------------------------------------------------

fill_duplicate_models = function(model_list_half_fitted, full_model_list) {
  right_tibble = model_list_half_fitted %>% filter(h_agnostic) %>%
    select(model_fun, train_start_date, train_end_date, fitted_model) 
  
  duplicate_models = full_model_list %>% filter(duplicate_model)
  
  duplicate_models_fitted = left_join(duplicate_models, right_tibble, 
                                  by = c("model_fun", "train_start_date", "train_end_date"))
  
  model_list_fitted = bind_rows(model_list_half_fitted, duplicate_models_fitted)
  return(model_list_fitted)
}

add_point_forecasts = function(model_list_fitted) {
  model_list_fitted = mutate(model_list_fitted, 
                          point_forecast = pmap_dbl(list(fitted_model, h, train_sample, forecast_extractor), 
                                                    ~ do.call(..4, list(model = ..1, h = ..2, model_sample = ..3))
                          ))
  return(model_list_fitted)
}

calculate_mae_table = function(model_list_fitted) {
  mae_table = model_list_fitted %>% select(h, model_fun, value, point_forecast) %>%
    mutate(abs_diff = abs(value - point_forecast))  %>%
    group_by(h, model_fun) %>% summarise(mae = mean(abs_diff))
  
  # sort by mae for each h:
  mae_table = mae_table %>% arrange(h, mae) 
  
  return(mae_table)
}


# all the code is here!

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

start_date = ymd("2011-10-01")

rus_q_full_stable = filter(rus_q_full, date >= start_date)
rus_q_full_stable = rename(rus_q_full_stable, gdp_nominal = value, value = gdp_real_2016_price)



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
                           "arima_fun", TRUE, "uni_model_2_scalar_forecast")
#                           "arima11_fun", TRUE, "uni_model_2_scalar_forecast")



# TODO: exact ML in case where ARMA(1,1)-SARMA(1,1) fails

cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_q_full_stable)
cv_res_models = estimate_nonduplicate_models(cv_results)
cv_results_new = fill_duplicate_models(cv_res_models, cv_results)
cv_results_new = add_point_forecasts(cv_results_new)
mae_table = calculate_mae_table(cv_results_new)

mae_table
write_csv(mae_table, "mae_table_gdp_real.csv")

# real forecasting....


# models in tibble version ------------------------------------------------

the_forecasts = prepare_model_list2(h_all = h_all, model_fun_tibble = model_fun_tibble, series_data = rus_q_full_stable)
the_forecasts_fitted = estimate_nonduplicate_models(the_forecasts)
the_forecasts_new = fill_duplicate_models(the_forecasts_fitted, the_forecasts)
the_forecasts_new = add_point_forecasts(the_forecasts_new)

only_numbers = select(the_forecasts_new, date, h, model_fun, point_forecast)
only_numbers
write_csv(only_numbers, path = "forecasts_gdp_real.csv")

