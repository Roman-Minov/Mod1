setwd("D:/Ромина папка/Учеба/IT")
library(tidyverse)
library(rnoaa)
library(lubridate)

station_data = read.csv("station_data.csv")
#список метеостанций
Chelyabinsk = data.frame(id = "CHELYABINSK", latitude = 55.15402,  longitude = 61.42915)
#найдем станции
Chelyabinsk_around = meteo_nearby_stations(lat_lon_df = Chelyabinsk, station_data = station_data,
                                           limit = 15, var = "TAVG", 
                                           year_min = 2004, year_max = 2009)

#Получим данные с 1, 3 метеостанции
#Создадим пустую таблицу куда запишем данные с метеостанций
all_data = tibble()
for (i in c(1,3))
{
# Определим станцию:
Chelyabinsk_id = Chelyabinsk_around[["CHELYABINSK"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = Chelyabinsk_id,
                          var="TAVG",
                          date_min="2004-01-01",
                          date_max="2017-12-31")
#объединим данные в таблице
all_data = bind_rows(all_data, data %>%
                         #группировка по году и месяцу
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         summarise (tavg = sum(tavg[tavg<30], na.rm = TRUE)/10 )

af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
df = c(0.00,0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

#Вычислим di для каждого месяца
di = summarize(all_data, di = length(tavg[tavg>70])/length(tavg))[,-1]

#вычислим среднюю температуру
St = summarize(all_data, St = sum(tavg[tavg<30])/10/2)[,-1]

#Найдем урожаность по формуле:
Fi = af + bf * 1.0 * St
yield = 10^6*sum(Fi*di*Kf/(Qj*Lj*(100-Ej)))
yield

