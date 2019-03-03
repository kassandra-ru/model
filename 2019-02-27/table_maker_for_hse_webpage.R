library(tidyverse)
library(tidyr)
library(rio)


mae_table = import("mae_table_cpi.csv")
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 месяц", 
                                           h == 2 ~ "2 месяца",
                                           h == 3 ~ "3 месяца",
                                           h == 4 ~ "4 месяца",
                                           h == 5 ~ "5 месяцев",
                                           h == 6 ~ "6 месяцев"), mae = round(mae, 2))
mae_wide = spread(mae_post, key = model_fun, value = mae)
write_csv2_cp1251(mae_wide, "mae_cpi_wide.csv")

forecast_table = import("forecasts_cpi.csv")
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 2)) %>% 
  mutate(date = case_when(date == "2018-09-01" ~ "сентябрь 2018",
                          date == "2018-10-01" ~ "октябрь 2018",
                          date == "2018-11-01" ~ "ноябрь 2018",
                          date == "2018-12-01" ~ "декабрь 2018",
                          date == "2019-01-01" ~ "январь 2019",
                          date == "2019-02-01" ~ "февраль 2019",
                          date == "2019-03-01" ~ "март 2019",
                          date == "2019-04-01" ~ "апрель 2019",
                          date == "2019-05-01" ~ "май 2019",
                          date == "2019-06-01" ~ "июнь 2019",
                          date == "2019-07-01" ~ "июль 2019",
                          date == "2019-08-01" ~ "август 2019"
                          ))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
write_csv2_cp1251(forecast_wide, "forecast_cpi_wide.csv")



# gdp


mae_table = import("mae_table_gdp_rate_real.csv")
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 квартал", 
                                           h == 2 ~ "2 квартала",
                                           h == 3 ~ "3 квартала",
                                           h == 4 ~ "4 квартала",
                                           h == 5 ~ "5 кварталов",
                                           h == 6 ~ "6 кварталов"), mae = round(mae, 4))
mae_wide = spread(mae_post, key = model_fun, value = mae)
write_csv2_cp1251(mae_wide, "mae_gdp_wide.csv")

forecast_table = import("forecasts_gdp_rate_real.csv")
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 3)) %>% 
  mutate(date = case_when(date == "2018-10-01" ~ "IV квартал 2018",
                          date == "2019-01-01" ~ "I квартал 2019",
                          date == "2019-04-01" ~ "II квартал 2019",
                          date == "2019-07-01" ~ "III квартал 2019",
                          date == "2019-10-01" ~ "IV квартал 2019"
                          ))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
write_csv2_cp1251(forecast_wide, "forecast_gdp_wide.csv")




