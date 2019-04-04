# devtools::install_github("kassandra-ru/kassandr")
# Sys.setenv("TAR" = "internal") # if install_github() fails on Windows OS


library(kassandr)
library(tidyverse)
library(rio)
library(lubridate)
library(docxtractr)

set_libreoffice_path("/usr/bin/libreoffice")  # ubuntu or macos
set_libreoffice_path("C:/Program Files/LibreOffice/program/soffice.exe")  # windows

Sys.setenv(LD_LIBRARY_PATH = "/usr/lib/libreoffice/program/") # ubuntu protection against libreglo.so not found

path = "~/Documents/kassandra/data/raw/"
path = "D:/Research/Kassandra/data/raw/"

path_day = paste0(path, "/", Sys.Date(), "/") # add current date to path

watchdog_file = paste0(path, "watchdog.csv")
watchdog = import(watchdog_file)

# this was done to setup download log file for the first time
# download_log_first = watchdog %>% mutate(access_date = Sys.Date(), access_status = NA, hash_raw = NA, hash_main = NA) %>% head(0)
# export(download_log_first, download_log_file)


# download all
download_log_new = download_statistics(path, watchdog)

# write log information (what has been successful...)
glimpse(download_log_new)
export(download_log_new, paste0(path_day, "download_log.csv"))

# adding raw files to gitignore
write_lines(na.omit(watchdog$file_raw), path = paste0(path_day, ".gitignore"))
