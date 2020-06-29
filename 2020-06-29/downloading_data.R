library(docxtractr)
# devtools::install_github("kassandra-ru/kassandr")
library(kassandr)
library(tidyverse)
library(rio)
library(lubridate)

info = Sys.info() # получаем информацию о системе

if (info[1] == "Linux") {
  set_libreoffice_path("/usr/bin/libreoffice")  # ubuntu or macos
  Sys.setenv(LD_LIBRARY_PATH = "/usr/lib/libreoffice/program/") # ubuntu protection against libreglo.so not found
  path = "~/Documents/kassandra/data/raw/"
}

if (info[1] == "Windows") {
  Sys.setenv("TAR" = "internal") # if install_github() fails on Windows OS
  set_libreoffice_path("C:/Program Files/LibreOffice/program/soffice.exe")  # windows
  path = "D:/Research/Kassandra/data/raw/"
}

# download all
download_log_new = download_statistics(path, watchdog)

# write log information (what has been successful...)
glimpse(download_log_new)
export(download_log_new, paste0(path_day, "download_log.csv"))

# adding raw files to gitignore
write_lines(na.omit(watchdog$file_raw), path = paste0(path_day, ".gitignore"))
