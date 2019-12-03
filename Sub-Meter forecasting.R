## Analysing Sub-Meter 1, 2 and 3


## Loading libraries and data

library(RMySQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(tidyverse)
library(arules)
library(forecast)
library(tseries)
library(TSstudio)



con = dbConnect(
  MySQL(),
  user = 'deepAnalytics',
  password = 'Sqltask1234!',
  dbname = 'dataanalytics2018',
  host = 'data-analytics-2018.cbrosir2cswx.us-east-1.rds.amazonaws.com'
)


summary(con)
attributes(con)

dbListTables(con)

dbListFields(con, "yr_2006")
dbListFields(con, "yr_2007")


## Queries for getting the tables


yr07_select <- dbGetQuery(con, "SELECT id, 
                                       Date, 
                                       Time, 
                                       Global_active_power, 
                                       Global_reactive_power, 
                                       Global_intensity, 
                                       Voltage, 
                                       Sub_metering_1, 
                                       Sub_metering_2, 
                                       Sub_metering_3 FROM yr_2007")

yr08_select <- dbGetQuery(con, "SELECT id, 
                                       Date, 
                                       Time, 
                                       Global_active_power, 
                                       Global_reactive_power, 
                                       Global_intensity, 
                                       Voltage, 
                                       Sub_metering_1, 
                                       Sub_metering_2, 
                                       Sub_metering_3 FROM yr_2008")

yr09_select <- dbGetQuery(con, "SELECT id, 
                                       Date, 
                                       Time, 
                                       Global_active_power, 
                                       Global_reactive_power, 
                                       Global_intensity, 
                                       Voltage, 
                                       Sub_metering_1, 
                                       Sub_metering_2, 
                                       Sub_metering_3 FROM yr_2009")



## combining year 2007, 2008 and 2009
newDF <- bind_rows(yr07_select, yr08_select, yr09_select)


newDF$date.time <- paste(newDF$Date, newDF$Time)



## date and time settings / convertion 

newDF$date.time <- as.POSIXct(newDF$date.time, 
                              tz = "Europe/Paris", 
                              format = c("%Y-%m-%d %H:%M:%S"))


## Separating from date.time year, month and day

newDF$year <- year(newDF$date.time)
newDF$month <- month(newDF$date.time)
newDF$week <- isoweek(newDF$date.time)
newDF$day <- day(newDF$date.time)
newDF$hour <- hour(newDF$date.time)



## transformning the remaining power usage into sub meter 4

smart_meters <- newDF %>% 
  mutate(kw_per_min =(Global_active_power*1000)/60) %>% 
  mutate(Sub_metering_4 = kw_per_min 
         - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)


## renaming Submeters

colnames(smart_meters)[colnames(smart_meters)=="Sub_metering_1"] <- 
  "kitchen"
colnames(smart_meters)[colnames(smart_meters)=="Sub_metering_2"] <- 
  "laundry"
colnames(smart_meters)[colnames(smart_meters)=="Sub_metering_3"] <- 
  "water_AC"
colnames(smart_meters)[colnames(smart_meters)=="Sub_metering_4"] <- 
  "rest"



## gathering the sub-meters together in one variable "Meter"

smart_tidy <- smart_meters %>%
  gather(Meter, Watt_hr, kitchen, laundry, water_AC, rest)


## omitting missing values and stuff.. 

smart_tidy <- na.omit(smart_tidy, cols = year)


smart_tidy %>% as_tibble(smart_tidy)

is_tibble(smart_tidy)
smart_tidy$Meter <- factor(smart_tidy$Meter)


## total power usage splitted by all sub meters


ggplot(smart_tidy %>% 
         filter(year == 2007) %>% 
         group_by(Meter) %>% 
         summarise(mean_watthour = mean(Watt_hr)),
       aes(Meter, mean_watthour)) + 
  geom_col() +
  ggtitle("Summed up Watt hour consumption per Meter in 2007")


ggplot(smart_tidy %>%
         filter(year == 2007 & month == 3 & Meter == "kitchen"), 
        aes(hour, Watt_hr)) + 
        geom_col() +
        ggtitle("Watt hour consumption per hour in March 2007 for Kitchen")



# group by day of the year and year, summing mean of Sub-Meter 1 (kitchen)

smart_meters_kitchen <- smart_meters %>% 
  filter(year == 2008) %>% 
  select(year, day_of_year, kitchen) %>% 
  group_by(year, day_of_year) %>%
  summarise(mean_kitchen = mean(kitchen))


# create unique id for every week
my_weeks <- rep(1:1000, each = 7, length.out = nrow(smart_meters_kitchen))
smart_meters_kitchen$my_weeks <- my_weeks


# group by week id in order not to cut weeks at the end of the year
smart_meters_kitchen <- smart_meters_kitchen %>% 
  group_by(day_of_year) %>% 
  summarise(mean_kitchen = mean(mean_kitchen))


# creating the time series for kitchen sub meter

smart_meters_kitchen <-
  ts(smart_meters_kitchen$mean_kitchen,
     frequency = 365.25/7,
     start = c(2008, 1))

plot.ts(smart_meters_kitchen)



## simple moving average for kitchen

SMA_smart_meters_kitchen <- SMA(smart_meters_kitchen, n = 3)

plot.ts(SMA_smart_meters_kitchen)



smart_meters_kitchen_dec <- decompose(smart_meters_kitchen)
plot(smart_meters_kitchen_dec)


## forecasting energy consumption using Holt Winters...

forecast_kitchen_HW <- HoltWinters(smart_meters_kitchen, 
                                   beta = FALSE,
                                   gamma = FALSE,
                                   l.start = 2.474)


forecast_kitchen_HW

plot(forecast_kitchen_HW)

forecast_kitchen_HW$fitted


## sum of sqared errors and root mean squared errors

forecast_kitchen_HW$SSE

forecast_kitchen_HW$SSE/52

sqrt((forecast_kitchen_HW$SSE/52))



## ... for 52 weeks more (so for one year more, which is 2010)

forecast_kitchen <- forecast(smart_meters_kitchen,
                                   h = 150,
                                   level=c(80,90))

forecast_kitchen

plot(forecast_kitchen)
plot(forecast_kitchen, start(2010))   # don´t get it..
plot.forecast(forecast_kitchen)  # ??? no longer visible ??



## ACF of residuals of Holt Winters forecast

acf(forecast_kitchen$residuals, 
    lag.max = 10,
    na.action = na.pass)

plot(forecast_kitchen$residuals)


## .. and the partial ACF likewise

pacf(forecast_kitchen$residuals, 
     lag.max = 10,
     na.action = na.pass)

plot.ts(forecast_kitchen$residuals)  # what exactly is the difference
                                        # to the plot in line 242 ?


## Ljung Box Test

Box.test(forecast_kitchen$residuals, 
         lag = 10, 
         type = "Ljung-Box")



