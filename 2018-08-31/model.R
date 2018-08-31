# install dev packages ----------------------------------------------------

# devtools::install_github("tidyverts/fable")


# attach packages -----------------------------------------------------------

library(tidyverse)
library(rio)
library(tsibble)
library(fable)
library(forecast)
library(lubridate)

Sys.setlocale("LC_TIME", "C")

# load data ---------------------------------------------------------------

cpi = import("data_snapshot/cpi_inflation.csv")
gdp = import("data_snapshot/gdp.csv")

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

rus_q = na.omit(rus_q)
rus_m = na.omit(rus_m)

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

# cpi univariate models -------------------------------------------------------

rus_m = filter(rus_m, date >= ymd("2000-01-01"))

fable_cpi_arima = rus_m %>% ARIMA(value) %>% forecast(h = 6)
fable_cpi_ets = rus_m %>% ETS(value) %>% forecast(h = 6)

fable_cpi_arima$forecast[[1]] %>% autoplot()
fable_cpi_ets$forecast[[1]] %>% autoplot()

fable_cpi_arima$forecast[[1]] %>% .$mean
fable_cpi_ets$forecast[[1]] %>% .$mean

fable_cpi_arima %>% autoplot
fable_cpi_ets %>% autoplot


rus_m2 = filter(rus_m, date >= ymd("2011-10-01"))

fable_cpi_arima = rus_m2 %>% ARIMA(value) %>% forecast(h = 6)
fable_cpi_ets = rus_m2 %>% ETS(value) %>% forecast(h = 6)

fable_cpi_arima$forecast[[1]] %>% autoplot()
fable_cpi_ets$forecast[[1]] %>% autoplot()

fable_cpi_arima$forecast[[1]] %>% .$mean
fable_cpi_ets$forecast[[1]] %>% .$mean

fable_cpi_arima %>% autoplot
fable_cpi_ets %>% autoplot

cpi_value = ts(rus_m2$value, freq = 12, start = c(2011, 10))
cpi_tbats = cpi_value %>% tbats() 
cpi_tbats_forecast = cpi_tbats %>% forecast(h = 6)
autoplot(cpi_tbats)



