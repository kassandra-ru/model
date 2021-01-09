# update package if necessary ----
# devtools::install_github("kassandra-ru/kassandr")


# load packages ----
library(docxtractr)
library(kassandr)
library(tidyverse)
library(rio)
library(lubridate)


# set data folder ----
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


# create today's folder ----

access_date = Sys.Date()
today_folder = paste0(path, access_date, "/")
if (!dir.exists(today_folder)) {
  dir.create(today_folder)
}

# download setup ----
method = "curl" # maybe "curl", "wget", "libcurl", "auto", "internal", "wininet"
extra = "-L" # options for downloading files, passed to `download.file()`: used for "wget" and "curl" methods

# i_ipc.xlsx ----
url_from = "https://rosstat.gov.ru/storage/mediabank/a2Bf2bEU/i_ipc.xlsx"
raw_path_to = "i_ipc.xlsx"
csv_path_to = "i_ipc.csv"
univariate = TRUE
frequency = 12
comment = "Monthly chained CPI from Russian Statistical Agency"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
if (length(grep("Доступ запрещен", read_lines(raw_path_to_full))) > 0) {
  warning("Probably file moved to another location")
  stop("Fucking `Access denied` inside a file :(")
}
data_processed = convert_i_ipc_xlsx(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)


# tab5a.xls ----
url_from = "http://www.gks.ru/free_doc/new_site/vvp/kv/tab5a.xls"
raw_path_to = "tab5a.xls"
csv_path_to = "tab5a.csv"
univariate = TRUE
frequency = 4
comment = "Gross domestic product quarterly current prices"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
if (length(grep("Доступ запрещен", read_lines(raw_path_to_full))) > 0) {
  warning("Probably file moved to another location")
  stop("Fucking `Access denied` inside a file :(")
}
data_processed = convert_tab5a_xls(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)



# tab9a.xls ----
url_from = "http://www.gks.ru/free_doc/new_site/vvp/kv/tab9a.xls"
raw_path_to = "tab9a.xls"
csv_path_to = "tab9a.csv"
univariate = TRUE
frequency = 4
comment = "Deflator index in percent to the previous quarter"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
if (length(grep("Доступ запрещен", read_lines(raw_path_to_full))) > 0) {
  warning("Probably file moved to another location")
  stop("Fucking `Access denied` inside a file :(")
}
data_processed = convert_tab9a_xls(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)










