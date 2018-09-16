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

rus_m = mutate(rus_m, date = as_date(yearmonth(date))) 
rus_q = mutate(rus_q, date = yq(date)) 


glimpse(rus_q)
glimpse(rus_m)

rus_m = left_join(cpi, rus_m, by = "date")
rus_q = left_join(gdp, rus_q, by = "date")

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

rus_m_full = select(rus_m, real_income, unempl, constr_nat, real_wage, rts_index, agric_index, retail_index, value)
vis_miss(rus_m_full)

rus_q_full = select(rus_q, value, access_date, ir, reserves, real_wage, population)
vis_miss(rus_q_full)

# cpi univariate models -------------------------------------------------------

start_date = ymd("2011-10-01")

rus_m_full_stable = filter(rus_m_full, date >= start_date)


cpi_value = ts(rus_m_full_stable$value, freq = 12, start = c(year(start_date), month(start_date)))

tbats_model = cpi_value %>% tbats() 
ets_model = cpi_value %>% ets()
arima_model = Arima(cpi_value)
arima_11_11_model = Arima(cpi_value, order = c(1, 0, 1), seasonal = c(1, 0, 1))



cpi_tbats_forecast = tbats_model %>% forecast(h = 6)
cpi_arima_forecast = arima_model %>% forecast(h = 6)
cpi_arima_11_11_forecast = arima_11_11_model %>% forecast(h = 6)
cpi_ets_forecast = ets_model %>% forecast(h = 6)


cpi_ets_forecast
cpi_arima_forecast
cpi_arima_11_11_forecast
cpi_tbats_forecast



# cpi cross validation --------------------------------------------------

# input: rus_m_full_stable


train_proportion = 0.8
window_type = "moving" # "moving" or "growing"

nobs_full = nrow(rus_m_full_stable)
starting_window_length = round(train_proportion * nobs_full)


cv_model = tibble(last_included_obs = starting_window_length:nobs_full)
if (window_type == "moving") {
  cv_model = mutate(cv_model, first_included_obs = last_included_obs - starting_window_length + 1)
} else {
  cv_model = mutate(cv_model, first_included_obs = 1)
}

# cv_model = mutate(cv_model, data = map2(first_included_obs, last_included_obs, 
#                                       rus_m_full_stable[.x:.y, ]))

?map

# gdp univariate models -------------------------------------------------------

rus_q = filter(rus_q_full, date >= ymd("2000-01-01"))

fable_gdp1_arima = rus_q %>% ARIMA(value) %>% forecast(h = 3)
fable_gdp1_ets = rus_q %>% ETS(value) %>% forecast(h = 3)

fable_gdp1_arima %>% autoplot
fable_gdp1_ets %>% autoplot


fable_gdp1_arima$forecast[[1]] %>% .$mean
fable_gdp1_ets$forecast[[1]] %>% .$mean


rus_q2 = filter(rus_q_full, date >= ymd("2011-10-01"))

fable_gdp2_arima = rus_q2 %>% ARIMA(value) %>% forecast(h = 3)
fable_gdp2_ets = rus_q2 %>% ETS(value) %>% forecast(h = 3)

fable_gdp2_arima %>% autoplot()
fable_gdp2_ets %>% autoplot()

fable_gdp2_arima$forecast[[1]] %>% .$mean
fable_gdp2_ets$forecast[[1]] %>% .$mean

gdp2_value = ts(rus_q2$value, freq = 4, start = c(2011, 4))
gdp_tbats = gdp2_value %>% tbats() 
gdp_tbats_forecast = gdp_tbats %>% forecast(h = 3)
gdp_tbats_forecast
autoplot(gdp_tbats_forecast)


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
  date = rus_m_full %>% pull(!!date_variable) 
  interval = pull_interval(date)
  last_date = max(date)
  return(last_date)
}



# forecast for h using lasso ----------------------------------------------------------


augment_tsibble_4_regression = function(original, h = 1, frequency = 12) {
  augmented = original %>% append_row(n = h) %>% 
    add_trend() %>% add_fourier() %>% 
    add_lags(value, lags = c(h, h + 1, frequency, frequency + 1))
  regressor_names = setdiff(colnames(original), c("value", "date"))
  augmented = augmented %>% add_lags2(regressor_names, lags = c(h, h + 1, frequency, frequency + 1))
  augmented = augmented %>% select(-!!regressor_names)
  return(augmented)
}

lasso_forecast = function(augmented, s = c("lambda.min", "lambda.1se")) {
  s = match.arg(s)
  
  yX_future_tsibble = tail(augmented, 1)
  X_future = as_tibble(yX_future_tsibble) %>% select(-value, -date) %>% as.matrix()

  yX_tsibble = na.omit(augmented)
  y = yX_tsibble %>% pull("value")
  X = as_tibble(yX_tsibble) %>% select(-value, -date) %>% as.matrix()

  lasso_model = cv.glmnet(X, y)
  point_forecast = predict(lasso_model, X_future, s = s)
  return(point_forecast)
}

ranger_forecast = function(augmented, seed = 777) {
  yX_future_tsibble = tail(augmented, 1)

  yX_tsibble = na.omit(augmented)

  set.seed(seed)
  ranger_model = ranger(data = yX_tsibble, value ~ . - date)
  ranger_pred = predict(ranger_model, data = yX_future_tsibble)
  point_forecast = ranger_pred$predictions
  
  return(point_forecast)
}

rus_augmented = augment_tsibble_4_regression(rus_m_full, h = 1)


lasso_forecast(rus_augmented)
ranger_forecast(rus_augmented)









