library(tidyverse)
library(tidyr)
library(rio)

month_from = "08"

mae_table = import(paste0("mae_table_cpi_from_month_", month_from, ".csv"))
mae_post = mutate(mae_table, h = case_when(h == 1 ~ "1 месяц", 
                                           h == 2 ~ "2 месяца",
                                           h == 3 ~ "3 месяца",
                                           h == 4 ~ "4 месяца",
                                           h == 5 ~ "5 месяцев",
                                           h == 6 ~ "6 месяцев"), mae = round(mae, 2))
mae_wide = spread(mae_post, key = model_fun, value = mae)
write_csv(mae_wide, paste0("mae_wide_from_", month_from, ".csv"))

forecast_table = import(paste0("forecasts_cpi_from_month_", month_from, ".csv"))
forecast_post = mutate(forecast_table, point_forecast = round(point_forecast, 2)) %>% 
  mutate(date = case_when(date == "2018-09-01" ~ "сентябрь 2018",
                          date == "2018-10-01" ~ "октябрь 2018",
                          date == "2018-11-01" ~ "ноябрь 2018",
                          date == "2018-12-01" ~ "декабрь 2018",
                          date == "2019-01-01" ~ "январь 2019",
                          date == "2019-02-01" ~ "февраль 2019",
                          date == "2019-03-01" ~ "март 2019",
                          date == "2019-04-01" ~ "апрель 2019"
                          ))

forecast_wide = spread(forecast_post, key = model_fun, value = point_forecast) %>% arrange(h) %>% select(-h)
write_csv(forecast_wide, paste0("forecast_wide_from_", month_from, ".csv"))




