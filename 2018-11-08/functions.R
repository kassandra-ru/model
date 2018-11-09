

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




get_last_n_obs = function(hf_data, lf_date, n_lags = 1, one_row = TRUE) {
  
  ts_subset = filter(hf_data, date <= lf_date) %>% tail(n_lags)

  n_actual = nrow(ts_subset)
  if (n_actual < n_lags) {
    # create empty data frame
    empty_df = ts_subset[FALSE, ]
    empty_df[1:(n_lags - n_actual), ] = NA
    # append it before
    ts_subset = bind_rows(empty_df, ts_subset)
  } 

  if (one_row) {
    ts_subset = mutate(ts_subset, lag_no = n_lags:1)
    ts_long = ts_subset %>% as_tibble() %>% select(-date) %>% gather(key = "variable", value = "value", -lag_no) %>%
      unite(variable, lag_no, col = "var_lag", sep = "_")
    
    ts_one_row = spread(ts_long, key = "var_lag", value = "value")
    return(ts_one_row)
  } else {
    ts_subset = rename(ts_subset, hf_date = date)
    return(ts_subset)
  }
}

ts_2_tibble = function(ts_data) {
  if (is.ts(ts_data)) {
    ts_data = tsibble::as_tsibble(ts_data, gather = FALSE) %>% rename(date = index) 
  } 
  ts_data = as_tibble(ts_data)
  return(ts_data)
}

add_hf_lags = function(lf_data, hf_data, hf_variable = NULL, n_lags = 1, one_row = TRUE) {

  # go from ts class to tsibble (dataframe)
  lf_data = ts_2_tibble(lf_data)
  hf_data = ts_2_tibble(hf_data)

  # if not specified take all variables
  if (is.null(hf_variable)) {
    hf_variable = setdiff(colnames(hf_data), "date")
  }
  hf_data = select(hf_data, date, !!hf_variable) %>% na.omit() 
  
  augmented_lf_data = mutate(lf_data, hf_obs = map(date, 
                ~ get_last_n_obs(hf_data = hf_data, ., n_lags = n_lags, one_row = one_row)))
  augmented_lf_data = unnest(augmented_lf_data)
  return(augmented_lf_data)
}


read_datastream = function(filename) {
  data = rio::import(filename, skip = 5)
  data_cols = rio::import(filename, skip = 4)
  colnames(data) = colnames(data_cols)
  return(data)
}

