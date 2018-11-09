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


monthly = read_datastream("../../Project_15/Data/monthly.xlsx")
colnames(monthly) = str_replace_all(colnames(monthly), "\\.", "_")
colnames(monthly) = str_replace_all(colnames(monthly), "%", "p")




monthly2 = mutate(monthly, infl = log(RSCONPRCF) - lag(log(RSCONPRCF)))
glimpse(monthly2)


monthly3 = filter(monthly2, Code > "2000-01-01")
monthly_no_na = na.omit(monthly3)

base_model = ranger(data = monthly_no_na, formula = infl ~ . - Code - RSCONPRCF, importance = "impurity_corrected")
importance_vector = importance(base_model) 

importance = tibble(variable = names(importance_vector), impurity_adj = importance_vector) %>% arrange(-impurity_adj)
importance



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
