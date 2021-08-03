#update package if necessary ----

devtools::install_github("kassandra-ru/kassandr", force = TRUE)
  
  
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


simple_check_file = function(file) {
  if (str_detect(file, ".xlsx$") | str_detect(file, ".xls$")) {
    res = try(readxl::read_excel(file))
    if ('try-error' %in% class(res)) {
      stop('The file ', file, ' is not a honest excel file!!!')
    }
  }
  
  # more simple checks are better!!!
  
  return('OK')
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
url_from = "https://rosstat.gov.ru/storage/mediabank/9lgSWwOj/i_ipc_1991-2021.xlsx"
raw_path_to = "i_ipc.xlsx"
csv_path_to = "i_ipc.csv"
univariate = TRUE
frequency = 12
comment = "Monthly chained CPI from Russian Statistical Agency"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
simple_check_file(raw_path_to_full)
data_processed = convert_i_ipc_xlsx(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)
#if (file.exists(raw_path_to_full)) {
#  file.remove(raw_path_to_full)
#}


# tab5a.xls ----
# 2021-07-30, boris, works fine
url_from = "https://gks.ru/storage/mediabank/e6uKSphi/tab5a.xls"
raw_path_to = "tab5a.xls"
csv_path_to = "tab5a.csv"
univariate = TRUE
frequency = 4
comment = "Gross domestic product quarterly current prices"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
simple_check_file(raw_path_to_full)
data_processed = convert_tab5a_xls(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)
#if (file.exists(raw_path_to_full)) {
#  file.remove(raw_path_to_full)
#}



# tab9a.xls ----
# 2021-07-30, boris, works fine
url_from = "http://www.gks.ru/free_doc/new_site/vvp/kv/tab9a.xls"
raw_path_to = "tab9a.xls"
csv_path_to = "tab9a.csv"
univariate = TRUE
frequency = 4
comment = "Deflator index in percent to the previous quarter"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
simple_check_file(raw_path_to_full)
data_processed = convert_tab9a_xls(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)
#if (file.exists(raw_path_to_full)) {
#  file.remove(raw_path_to_full)
#}


# tab9.xls ----
# 2021-07-30, boris, works fine
url_from = "http://www.gks.ru/free_doc/new_site/vvp/kv/tab9.xls"
raw_path_to = "tab9.xls"
csv_path_to = "tab9.csv"
univariate = TRUE
frequency = 4
comment = "Deflator index in percent to the previous quarter early data"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
simple_check_file(raw_path_to_full)
data_processed = convert_tab9_xls(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)
#if (file.exists(raw_path_to_full)) {
# file.remove(raw_path_to_full)
#}




# tab6b.xls ----
# 2021-07-30, boris, works fine
# previous file name was tab6b, so we use it as local file name
url_from = "https://rosstat.gov.ru/storage/mediabank/5jmJa164/%D0%92%D0%92%D0%9F%20%D0%BA%D0%B2%D0%B0%D1%80%D1%82%D0%B0%D0%BB%D1%8B%20(%D1%81%201995%20%D0%B3.).xls"
raw_path_to = "tab6b.xls"
csv_path_to = "tab6b.csv"
univariate = TRUE
frequency = 4
comment = "Gross domestic product quarterly 2016 prices"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
simple_check_file(raw_path_to_full)
data_processed = convert_tab6b_xls(raw_path_to_full, access_date, sheet = 8)
export_with_safe_date(data_processed, csv_path_to_full)
#if (file.exists(raw_path_to_full)) {
#  file.remove(raw_path_to_full)
#}



# lendrate.html ----
# 2021-07-30, boris, works fine
url_from = "http://www.cbr.ru/hd_base/mkr/mkr_monthes/?UniDbQuery.Posted=True&UniDbQuery.From=08.2000&UniDbQuery.To=01.2100&UniDbQuery.st=SF&UniDbQuery.st=HR&UniDbQuery.st=MB&UniDbQuery.Currency=-1&UniDbQuery.sk=Dd1_&UniDbQuery.sk=Dd7&UniDbQuery.sk=Dd30&UniDbQuery.sk=Dd90&UniDbQuery.sk=Dd180&UniDbQuery.sk=Dd360"
raw_path_to = "lendrate.html"
csv_path_to = "lendrate.csv"
univariate = FALSE
frequency = 12
comment = "Monthly lending rate multiple duration periods"

csv_path_to_full = paste0(today_folder, csv_path_to)
raw_path_to_full = paste0(today_folder, raw_path_to)

utils::download.file(url = url_from, destfile = raw_path_to_full, method = method, extra = extra)
simple_check_file(raw_path_to_full)
data_processed = convert_lendrate(raw_path_to_full, access_date)
export_with_safe_date(data_processed, csv_path_to_full)
#if (file.exists(raw_path_to_full)) {
#  file.remove(raw_path_to_full)
#}

