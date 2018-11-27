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



monthly <- read_datastream("../../Project_15/Data/monthly_all_wo_components.xlsx")
weekly <- read_datastream("../../Project_15/Data/weekly.xlsx")
daily <- read_datastream("../../Project_15/Data/daily.xlsx")







cv_results = prepare_model_list(h_all = h_all, model_fun_tibble = model_fun_tibble, dates_test = dates_test, 
                                window_type = window_type, series_data = rus_m_full_stable)
cv_res_models = estimate_nonduplicate_models(cv_results)
cv_results_new = fill_duplicate_models(cv_res_models, cv_results)
cv_results_new = add_point_forecasts(cv_results_new)
mae_table = calculate_mae_table(cv_results_new)




