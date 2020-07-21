# Требования к прогнозируемому скрипту

## На вход скрипт принимает csv файл 

csv файл имеет такую структуру

param, value, comment
max_h_yearly, 3, maximal forecasting horizon for yearly data in years
max_h_monthly, 12, maximal forecasting horizon for monthly data in months
max_h_quarterly, 4, maximal forecasting horizon for quarterly data in quarters
expanding_window, TRUE, true for expanding cross validation window and false for sliding
window_length, 4.5, starting window length for cross validation in years
cv_start, 2010-01-01, starting date for cross-validation 

Параметры для унификации должны называться именно так, как в колонке param.
Скрипт должен читать колонку value и менять своё поведение соответственно. 

## На выходе скрипт

1. Создаёт слепок всех использованных исходных данных в виде csv файлов. 
Один файл на одну частоту.
ts_yearly.csv
ts_monthly.csv
ts_quaterly.csv

формат:
date, xxx, yyy, zzz
2020-05-01, 6.4, -3, 12

Создаёт краткое описание рядов.
ts_name, comment
xxx, это я взял рандомные числа от фонаря
yyy, это я взял рандомные числа от другого фонаря

2. Создаёт csv файл с прогнозами на будущее.
Один файл на одну частоту.

fcst_monthly.csv
date, h, ts, model, value, 
2020-06-01, 1, "xxx", "XYZAR(1,1,1)", 17

3. Создаёт csv файл с прогнозами в прошлое согласно требованиям на кросс-валидацию.
Один файл на одну частоту
cv_monthly.csv
date, h, ts, model, value, observed, window_from, window_to
2020-05-01, 1, "xxx", "XYZAR(1,1,1)", 17, 6.4, 2010-01-01, 2020-04-01

4. Скрипт может сохранять и что-то ещё по доброй воле автора :)


Если какая-то модель иногда не оценивается, то правильно использовать try, 
и записать в файл с прогнозами NA, а не просто выдать ошибку.
Одна несошедшаяся модель не должна мешать другим :)



