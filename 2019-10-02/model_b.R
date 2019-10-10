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

raw_data_folder = "../../data/raw/2019-05-15/"

data_snapshot_folder = "data_snapshot/"
dir.create(data_snapshot_folder)

fit_folder = "fit/"
dir.create(fit_folder)

fcst_folder = "fcst/"
dir.create(fcst_folder)




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
                     "arima", "cpi", "lag2_ind_prod", "", "1,2",
                     "ets", "ind_prod", "", "", "1,2,3,4,5,6",
                     "arima", "cpi", "", "p=1,d=1,q=0,pseas=1,dseas=1,qseas=0,method='ML'", "1,2,3,4,5,6",
                     "var", "cpi+ind_prod", "", "p=5", "1,2,3,4,5,6",
                     "arima", "cpi", "lag2_ind_prod", "p=1,d=0,q=0,pseas=1,dseas=1,qseas=0", "1,2",
                     "ranger", "cpi", "lag2_ind_prod+trend_lin+FOURIER_M", "", "1,2",
                     "tbats", "cpi", "", "", "1,2,3,4,5,6")
model_list = mutate(model_list, window_type = "sliding", frequency = 12, cv_proportion = 0.2) # add sliding or stretching window 
# we may use separate window for each model. Why not? :)


model_list = filter(model_list, model %in% c("arima"))
model_list

# convention: predicted, h are strings with possible , or +
# predicted_, h_ are melted versions of predicted and h
# h = "1,2,3,4,5" => h_ will be a vector with 5 elements



general_model_info = tribble(~model, ~h_dependent, ~multivariate, ~allows_regressors, 
                             "arima", FALSE, FALSE, TRUE,
                             "ets", FALSE, FALSE, FALSE,
                             "ranger", TRUE, FALSE, TRUE,
                             "var", FALSE, TRUE, TRUE,
                             "tbats", FALSE, FALSE, FALSE)

model_2_estimator = function(model_name) {
  return(paste0(model_name, "_estimator"))
}

model_2_forecastor = function(model_name) {
  return(paste0(model_name, "_forecastor"))
}



general_model_info = mutate(general_model_info, estimator = model_2_estimator(model),
                            forecastor = model_2_forecastor(model))
general_model_info


# здесь можно дописать конструктор model_list который по h подбирает лаги
# типа lah0 = lag1 при h=1 и lag2 при h=2
# TODO: подумать, а надо ли оно

# если будут предикторы высокочастотные — разделять их в предикторах через | или типа того!


# acronyms act on equation(+) and options(?)
acronyms = tribble(~acronym, ~meaning,
                   "FOURIER_M", "s1_12+s2_12+s3_12+s4_12+s5_12+c1_12+c2_12+c3_12+c4_12+c5_12+c6_12",
                   "FOURIER_Q", "s1_4+c1_4+c2_4",
                   "TRENDS", "trend_lin+trend_root")
acronyms

first_useful_date = ymd("2011-10-01") # all previous info will be ignored
forecast_from_date = ymd("2019-04-01") # we play in a forecaster at this moment of time





# precalculated vectors and consts ----------------------------------------


all_h = pull(model_list, h) %>% numbers_string_2_vector()
all_h
h_max = all_h %>% max()




  
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


forecasters_tsibble = filter(rus_ts, date >= first_useful_date)

predictors = split_variable_names(model_list$predictors, acronyms = acronyms)
predicted = split_variable_names(model_list$predicted, acronyms = acronyms)
useful_vars = c(predictors, predicted, "date") %>% unique()

forecasters_tsibble = augment_tsibble_4_forecasting(forecasters_tsibble, h_max = h_max)  # точно с запасом :)
forecasters_tsibble = select(forecasters_tsibble, useful_vars)
glimpse(forecasters_tsibble)

frequency = frequency(forecasters_tsibble)
frequency
# при наличии рваного края (последнее наблюдение приходится на разные даты у разных переменных)
# дополним набор данных лишними строками по максимуму
# а прогнозировать будем только заказанный h для каждой переменной, то есть forecasting_dot будет по заказанным h
  
  


# variable availability ---------------------------------------------------

get_variable_availability = function(x, remove_names = c("date", "access_date")) {
  variable_availability = tibble(var_name = setdiff(colnames(x), remove_names), first_obs_row = NA, last_obs_row = NA,
                                 first_obs = as.Date(NA), last_obs = as.Date(NA), complete_obs = NA)
  for (var_no in 1:nrow(variable_availability)) {
    var_name = variable_availability$var_name[var_no]
    omitted_x = na.omit(x[, c("date", var_name)])
    
    variable_availability$first_obs_row[var_no] = min(which(!is.na(x[, var_name])))
    variable_availability$last_obs_row[var_no] = max(which(!is.na(x[, var_name])))
    
    
    variable_availability$first_obs[var_no] = head(omitted_x$date, 1)
    variable_availability$last_obs[var_no] = tail(omitted_x$date, 1)

    variable_availability$complete_obs[var_no] = nrow(omitted_x)
  }
  variable_availability = mutate(variable_availability, between_missings = 1 + last_obs_row - first_obs_row - complete_obs)
  return(variable_availability)
}

variable_availability = get_variable_availability(forecasters_tsibble)
variable_availability


# model dates ---------------------------------------------------------





# здесь мы составляем список точек прогнозирования. 
# одна точка — это прогноз конкретной переменной на конкретную дату по конкретной модели с опциями

# unabbreviate functions ---------------------------------------------------------
model_list_plus = mutate(model_list, predictors_full = unabbreviate_vector(predictors, acronyms))
model_list_plus = mutate(model_list_plus, multivariate = str_detect(predicted, "[\\+,]"))
model_list_plus = mutate(model_list_plus, has_predictors = (predictors != ""))
model_list_plus = mutate(model_list_plus, options_tibble = map(options, ~ param_string_2_tibble(.x)))
model_list_plus = mutate(model_list_plus, h_max = map_int(h, ~ numbers_string_2_vector(.x) %>% as.integer() %>% max()))

# если multivariate модель поддерживает рваный край, то можно ей добавить опцию rugged в опциях
# TODO: подумать



calculate_model_dates = function(model_list, variable_availability) {
  model_list = mutate(model_list, 
                time_unit = case_when(frequency == 12 ~ months(1),
                                      frequency == 4 ~ months(3)))
  
  model_list = mutate(model_list, all_used_vars = pmap(list(x = predictors_full, y = predicted),
                                                       ~ c(split_variable_names(.x), split_variable_names(.y))),
                                  variable_info = map(all_used_vars, ~ filter(variable_availability, var_name %in% .x)),
                                  vars_first_date = map(variable_info, ~ max(.x$first_obs)),
                                  vars_last_date = map(variable_info, ~ min(.x$last_obs)),
                                  vars_first_row = map_int(variable_info, ~ max(.x$first_obs_row)),
                                  vars_last_row = map_int(variable_info, ~ min(.x$last_obs_row))) %>% unnest(vars_first_date, vars_last_date)
  model_list = dplyr::select(model_list, -all_used_vars, -variable_info)


  model_list = mutate(model_list, useful_rows = vars_last_row - vars_first_row + 1,
                      cv_rows = round(cv_proportion * useful_rows),
                      initial_window_length = useful_rows - cv_rows,
                      future_first_date = vars_last_date + time_unit,
                      future_last_date = vars_last_date + h_max * time_unit,
                      cv_first_date = case_when(cv_proportion > 0 ~ vars_last_date - (cv_rows - 1) * time_unit,
                                                  TRUE ~ as.Date(NA)),
                      cv_last_date = case_when(cv_proportion > 0 ~ vars_last_date,
                                                 TRUE ~ as.Date(NA)),
                      initial_window_first_date = vars_first_date,
                      initial_window_last_date = initial_window_first_date + (initial_window_length - 1) * time_unit)
  return(model_list)
}

model_list_dated = calculate_model_dates(model_list_plus, variable_availability)


# calculate forecasting dots ----------------------------------------------





melt_h_predicted = function(model_list_dated) {
  model_list_upd = mutate(model_list_dated, h_ = as.list(str_split(h, ",")))
  model_list_upd = unnest(model_list_upd, h_, .drop = FALSE) %>% mutate(h_ = as.integer(h_))
  model_list_upd = mutate(model_list_upd, predicted_ = as.list(str_split(predicted, "[\\+,]")))
  model_list_upd = unnest(model_list_upd, predicted_, .drop = FALSE)
  return(model_list_upd)  
}




glimpse(model_list_dated)
model_list_h_predicted = melt_h_predicted(model_list_dated)
glimpse(model_list_h_predicted)



# функция создаёт табличку обучающих выборок и точек прогнозирования
forecasting_goal_2_dots = function(frequency, h, time_unit, future_first_date, cv_first_date, cv_last_date, 
                                   initial_window_first_date, window_type, initial_window_length, cv_rows) {
  time_string = case_when(frequency == 12 ~ "1 months",
                          frequency == 4 ~ "3 months")
  new_date = future_first_date  + (h - 1) * time_unit
  forecasting_dots = tibble(dot_date = c(seq(from = cv_first_date, to = cv_last_date, by = time_string), new_date))
    
  
  forecasting_dots = mutate(forecasting_dots, in_cv = c(rep(TRUE, cv_rows), FALSE))
  forecasting_dots = mutate(forecasting_dots, train_last_date = dot_date - h * time_unit)
  if (!window_type %in% c("stretching", "sliding")) {
    stop("window_type should be 'stretching' or 'sliding'")
  }
  if (window_type == "stretching") {
    forecasting_dots = mutate(forecasting_dots, train_first_date = initial_window_first_date)
  } else if (window_type == "sliding") {
    forecasting_dots = mutate(forecasting_dots, train_first_date = train_last_date - (initial_window_length - 1) * time_unit)
  }
  return(forecasting_dots)
}




forecasting_dots = mutate(model_list_h_predicted, 
                          dots = pmap(list(frequency = frequency, h = h_, time_unit = time_unit, 
                                      future_first_date = future_first_date, cv_first_date = cv_first_date, cv_last_date = cv_last_date, 
                                      initial_window_first_date = initial_window_first_date, 
                                      window_type = window_type, initial_window_length = initial_window_length,
                                      cv_rows = cv_rows), 
                                      forecasting_goal_2_dots))
forecasting_dots_unnested = unnest(forecasting_dots, dots)


forecasting_dots_unnested = mutate(forecasting_dots_unnested, 
      model_id = group_indices(forecasting_dots_unnested, model, predicted, predictors, options),
      fit_id = group_indices(forecasting_dots_unnested, model, predicted, predictors, options, train_first_date, train_last_date))

glimpse(forecasting_dots_unnested)




# fill dots ---------------------------------------------------------------


# forecasting_dots = fill_fits(forecasting_dots, "arima", arima_estimator)
# forecasting_dots = fill_fits(forecasting_dots, "tbats", tbats_estimator)

construct_sample = function(full_data, sample_first_date, sample_last_date, predicted, predictors) {
  vars = c(split_variable_names(predictors), split_variable_names(predicted), "date")
  train_sample = dplyr::select(full_data, vars) %>% filter(date >= sample_first_date, date <= sample_last_date)
  # train_sample = augment_tsibble_4_forecasting(train_sample, h_max = 0)
  return(train_sample)
}



estimate_one_fit = function(train_sample, predicted, predictors, options, model) {
  estimator = model_2_estimator(model)
  fit = do.call(estimator, 
                list(train_sample = train_sample,  
                     predicted = predicted, options = options, predictors = predictors))
  return(fit)
}



forecast_one_fit = function(fit, predicted, predictors, options, model, predicted_, h = 1, test_sample, frequency) {
  forecastor = model_2_forecastor(model)
  forecast = do.call(forecastor, 
                list(fit = fit, predicted_ = predicted_, h = h, test_sample = test_sample, 
                     predicted = predicted, options = options, predictors = predictors, frequency = frequency))
  return(forecast)
}


save_if_requested = function(fit, fits_folder, fit_file, save_to = c("memory", "disk")) {
  save_to = match.arg(save_to)
  if (save_to == "memory") {
    return(fit)
  } else {
    write_rds(fit, path = paste0(fits_folder, fit_file))
    return(NA)
  }
}


# "memory"

# step 1. create tt sample
# tt sample = train + test sample united
forecasting_dots_upd = mutate(forecasting_dots_unnested,
                          tt_sample = pmap(list(train_first_date, train_last_date, predicted, predictors_full, h_, time_unit), 
                                              ~ construct_sample(full_data = forecasters_tsibble, 
                                                                       sample_first_date = ..1, 
                                                                       sample_last_date = ..2 + ..5 * ..6, 
                                                                       predicted = ..3, 
                                                                       predictors = ..4)))

glimpse(forecasting_dots_upd)

# step 1.5 estimate model 


pr_bar = progress_estimated(nrow(forecasting_dots_upd))
forecasting_dots_upd = mutate(forecasting_dots_upd,
                              fit = pmap(list(tt_sample, train_last_date, predicted, predictors_full, options, model),
                                         ~ {
                                           pr_bar$tick()$print()
                                           estimate_one_fit(train_sample = filter(..1, date <= ..2), 
                                                            predicted = ..3, predictors = ..4, options = ..5, model = ..6)}))


# step 2.5 forecast (as object)
forecasting_dots_upd = mutate(forecasting_dots_upd,
                              fcst_object = pmap(list(tt_sample, h_, predicted, predictors_full, options, model, fit, predicted_,
                                                      frequency),
                                         ~ forecast_one_fit(test_sample = tail(..1, ..2), h = ..2, fit = ..7,
                                                            predicted_ = ..8,
                                                            predicted = ..3, predictors = ..4, options = ..5, model = ..6, frequency = ..9)))


forecasting_dots_upd$fcst_object = as.list(rep(NA, nrow(forecasting_dots_upd)))
for (fit_no in 1:nrow(forecasting_dots_upd)) {
  row = forecasting_dots_upd[fit_no, ]
  cat(fit_no, ":\n")
  forecasting_dots_upd$fcst_object[[fit_no]] = forecast_one_fit(test_sample = tail(row$tt_sample[[1]], row$h_),
                                                              h = row$h_,
                                                              fit = row$fit[[1]],
                                                              predicted = row$predicted,
                                                              predictors = row$predictors_full, 
                                                              predicted_ = row$predicted_,
                                                              options = row$options,
                                                              model = row$model,
                                                              frequency = row$frequency)
}
glimpse(forecasting_dots_upd)



# step 3. forecast objec to point forecast 
forecasting_dots_upd = mutate(forecasting_dots_upd, 
                          point_forecast = pmap_dbl(list(fcst_object, h_), ~ forecast_2_scalar(..1, ..2)))
glimpse(forecasting_dots_upd)




# STOPPED here

# "file"
# step 5
forecasting_dots = mutate(forecasting_dots, point_forecast = forecast_2_scalar(read_rds(paste0(fcst_folder, fcst_file))))





