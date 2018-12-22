# install dev packages ----------------------------------------------------

# devtools::install_github("tidyverts/fable")

# attach packages -----------------------------------------------------------

library(tidyverse) # data manipulation
library(rio) # import - export data
library(tsibble) # ts data frames
# library(fable) # forecasting models
library(forecast) # forecasting models
library(lubridate) # working with dates
library(glmnet) # lasso
library(naniar) # missing values visualization
# library(fasster) # fasster model
library(ranger) # random forest
library(stringr) # character variables
library(rlang) # шаманство с бум-бум!


Sys.setlocale("LC_TIME", "C")

library(kassandr)


# load data ---------------------------------------------------------------

cpi = import("data_snapshot/I_ipc_converted.csv")

rus_m = import("data_snapshot/russia_monthly.csv")
rus_m_info = import("data_snapshot/russia_monthly_info.csv")


# manually expect data ----------------------------------------------------

tail(cpi)
head(cpi)
glimpse(cpi)


tail(rus_m)
head(rus_m)
glimpse(rus_m)


# data cleanup ------------------------------------------------------------

# remove NA and duplicates by coincidence :)

# better variable names

# character to dates
# standartize dates to yyyy-mm-dd


colnames(rus_m) = c("date", "empl_manuf", "ind_prod", "cpi_index", "ib_rate", "lend_rate", 
                    "real_income", "unempl", "ppi_index", "constr_nat", "inv_realcap", "real_wage",
                    "m2", "rts_index", "reer_cpi", "gas_price", "trade_balance", "reserves_nongold", 
                    "exch_rate_us", "worker_demand", "agric_index", 
                    "retail_index", "budget", "export", "import")

cpi = mutate(cpi %>% select(-access_date), date = ymd(date))

rus_m = mutate(rus_m, date = as_date(yearmonth(date))) 


glimpse(rus_m)

rus_m = left_join(cpi, rus_m, by = "date")



rus_m = mutate(rus_m, date = yearmonth(date))

rus_m = as_tsibble(rus_m, index = date) %>% select(-cpi_index)



# plots -------------------------------------------------------------------
head(rus_m)

cpi_m_ts = ts(rus_m$value, frequency = 12, start = c(1991, 1))
ggtsdisplay(cpi_m_ts)
ggseasonplot(cpi_m_ts)


vis_miss(rus_m)


# select full variables ---------------------------------------------------



# cpi ranger model -------------------------------------------------------


start_date = ymd("2010-10-01")

rus_m2_subset = filter(rus_m, date >= start_date)
vis_miss(rus_m2_subset)

rus_m2_augmented = kassandr:::augment_tsibble_4_regression(rus_m2_subset)
rus_m2_augmented


vis_miss(rus_m2_augmented)
rus_m2_cleaned = na.omit(rus_m2_augmented)

ggtsdisplay(rus_m2_cleaned$value)
les = ranger(data = rus_m2_cleaned, formula = value ~ . - date, num.trees = 5000, importance = "impurity_corrected")
importance_vector = importance(les)

import_tibble = tibble(variable = names(importance_vector), importance = importance_vector) %>% arrange(-importance)
import_tibble




# ind_prod ranger model -------------------------------------------------------

rus_m = rename(rus_m, cpi = value, value = ind_prod)

start_date = ymd("2010-10-01")

rus_m2_subset = filter(rus_m, date >= start_date)
vis_miss(rus_m2_subset)

rus_m2_augmented = kassandr:::augment_tsibble_4_regression(rus_m2_subset)
rus_m2_augmented


vis_miss(rus_m2_augmented)
rus_m2_cleaned = na.omit(rus_m2_augmented)

ggtsdisplay(rus_m2_cleaned$value)

les = ranger(data = rus_m2_cleaned, formula = value ~ . - date, num.trees = 5000, importance = "impurity_corrected")
importance_vector = importance(les)

import_tibble = tibble(variable = names(importance_vector), importance = importance_vector) %>% arrange(-importance)
import_tibble


# inv_realcap ranger model -------------------------------------------------------

rus_m = rename(rus_m, ind_prod = value, value = inv_realcap)

start_date = ymd("2010-10-01")

rus_m2_subset = filter(rus_m, date >= start_date)
vis_miss(rus_m2_subset)

rus_m2_augmented = kassandr:::augment_tsibble_4_regression(rus_m2_subset)
rus_m2_augmented


vis_miss(rus_m2_augmented)
rus_m2_cleaned = na.omit(rus_m2_augmented)

ggtsdisplay(rus_m2_cleaned$value)

les = ranger(data = rus_m2_cleaned, formula = value ~ . - date, num.trees = 5000, importance = "impurity_corrected")
importance_vector = importance(les)

import_tibble = tibble(variable = names(importance_vector), importance = importance_vector) %>% arrange(-importance)
import_tibble



