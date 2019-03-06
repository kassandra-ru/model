devtools::install_github("kassandra-ru/kassandr")

library(kassandr)
library(tidyverse)
library(rio)
library(lubridate)
library(docxtractr)

set_libreoffice_path("/usr/bin/libreoffice")  # ubuntu
# set_libreoffice_path("/usr/bin/libreoffice")  # macos
# set_libreoffice_path("C:/Program Files/LibreOffice/program/soffice.exe")  # windows


path = "~/Documents/kassandra/data/raw/"


watchdog_file = paste0(path, "watchdog.csv")
download_log_file = paste0(path, "download_log.csv")

watchdog = import(watchdog_file)

# this was done to setup download log file for the first time
# download_log_first = watchdog %>% mutate(access_date = Sys.Date(), access_status = NA, hash_raw = NA, hash_main = NA) %>% head(0)
# export(download_log_first, download_log_file)



download_log_new = download_statistics(path, watchdog)

# first time:
# export(download_log_new, download_log_file)

# second and more:
download_log = import(download_log_file) %>% mutate(access_date = ymd(access_date))
download_log = bind_rows(download_log, download_log_new)
export(download_log, download_log_file)
