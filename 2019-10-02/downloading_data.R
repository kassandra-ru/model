info = Sys.info() # получаем информацию о системе
library(docxtractr)

if (info[1] == "Linux") {
  set_libreoffice_path("/usr/bin/libreoffice")  # ubuntu or macos
  Sys.setenv(LD_LIBRARY_PATH = "/usr/lib/libreoffice/program/") # ubuntu protection against libreglo.so not found
}

if (info[1] == "Windows") {
  Sys.setenv("TAR" = "internal") # if install_github() fails on Windows OS
  set_libreoffice_path("C:/Program Files/LibreOffice/program/soffice.exe")  # windows
}

path = "~/Documents/kassandra/data/raw/"
path = "D:/Research/Kassandra/data/raw/"
#library(devtools)
devtools::install_github("kassandra-ru/kassandr")

library(kassandr)
library(tidyverse)
library(rio)

library(lubridate)


path_day = paste0(path, "/", Sys.Date(), "/") # add current date to path

watchdog_file = paste0(path, "watchdog.csv")
watchdog = import(watchdog_file)

# download all
download_log_new = download_statistics(path, watchdog)

# write log information (what has been successful...)
glimpse(download_log_new)
export(download_log_new, paste0(path_day, "download_log.csv"))

# adding raw files to gitignore
write_lines(na.omit(watchdog$file_raw), path = paste0(path_day, ".gitignore"))
