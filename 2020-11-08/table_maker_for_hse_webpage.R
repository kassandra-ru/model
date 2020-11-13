library(tidyverse)
library(tidyr)
library(rio)
library(kassandr)
dir.create("web_tables")


#------------ inflation


mae_table = import("estimation_results/mae_table_cpi.csv")
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 месяц", 
                                           h == 2 ~ "2 месяца",
                                           h == 3 ~ "3 месяца",
                                           h == 4 ~ "4 месяца",
                                           h == 5 ~ "5 месяцев",
                                           h == 6 ~ "6 месяцев"), mae = round(mae, 2))

mae_wide = spread(mae_post, key = model_fun, value = mae)
writexl::write_xlsx(mae_wide, "web_tables/mae_cpi_wide.xlsx")


forecast_table = import("estimation_results/forecasts_cpi.csv")
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 2))%>%
  mutate(date = date_to_string(date, freq = 12))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
writexl::write_xlsx(forecast_wide, "web_tables/forecast_cpi_wide.xlsx")


#------------ GDP growth



mae_table = import("estimation_results/mae_table_gdp_rate_real.csv")
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 квартал", 
                                           h == 2 ~ "2 квартала",
                                           h == 3 ~ "3 квартала",
                                           h == 4 ~ "4 квартала",
                                           h == 5 ~ "5 кварталов",
                                           h == 6 ~ "6 кварталов"), mae = round(mae, 4))
mae_wide = spread(mae_post, key = model_fun, value = mae)
writexl::write_xlsx(mae_wide, "web_tables/mae_gdp_wide.xlsx")


forecast_table = import("estimation_results/forecasts_gdp_rate_real.csv")
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 3)) %>% 
  mutate(date = date_to_string(date, freq = 4))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
writexl::write_xlsx(forecast_wide, "web_tables/forecast_gdp_wide.xlsx")

#------------industrial production

mae_table = import("estimation_results/mae_table_ind_prod.csv")
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 месяц", 
                                           h == 2 ~ "2 месяца",
                                           h == 3 ~ "3 месяца",
                                           h == 4 ~ "4 месяца",
                                           h == 5 ~ "5 месяцев",
                                           h == 6 ~ "6 месяцев"), mae = round(mae, 2))

mae_wide = spread(mae_post, key = model_fun, value = mae)
writexl::write_xlsx(mae_wide, "web_tables/mae_ind_prod_wide.xlsx")


forecast_table = import("estimation_results/forecasts_ind_prod.csv")
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 2))%>%
  mutate(date = date_to_string(date, freq = 12))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
writexl::write_xlsx(forecast_wide, "web_tables/forecast_ind_prod_wide.xlsx")

#------------fixed capital investment

#------------ GDP growth



mae_table = import("estimation_results/mae_table_investment.csv")
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 квартал", 
                                           h == 2 ~ "2 квартала",
                                           h == 3 ~ "3 квартала",
                                           h == 4 ~ "4 квартала",
                                           h == 5 ~ "5 кварталов",
                                           h == 6 ~ "6 кварталов"), mae = round(mae, 4))
mae_wide = spread(mae_post, key = model_fun, value = mae)
writexl::write_xlsx(mae_wide, "web_tables/mae_investment_wide.xlsx")


forecast_table = import("estimation_results/forecasts_investment.csv")
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 3)) %>% 
  mutate(date = date_to_string(date, freq = 4))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
writexl::write_xlsx(forecast_wide, "web_tables/forecast_investment.xlsx")
