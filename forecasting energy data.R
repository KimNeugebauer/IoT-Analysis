## Forecasting energy consumption

## Loading libraries and data

library(RMySQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(tidyverse)
library(arules)
library(TTR)
library(forecast)



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
  gather(Meter, Watt_hr,  `kitchen`,`laundry`,`heating`,`rest`)


## omitting missing values and stuff.. 

smart_tidy <- na.omit(smart_tidy, cols = year)


smart_tidy %>% as_tibble(smart_tidy)

is_tibble(smart_tidy)
smart_tidy$Meter <- factor(smart_tidy$Meter)


## total power usage splitted by all sub meters


energy_per_meter <- smart_tidy %>% 
  filter(smart_tidy$year == 2007 &
           smart_tidy$month == 1 &
           smart_tidy$day == 19) %>% 
  group_by(Meter) %>% 
  summarise(mean(Watt_hr))



## percentage of use by every sub meter per hour in march 07

march_07_new <- aggregate(cbind(kitchen, laundry, rest, water_AC) ~ hour, data=march_07, FUN=mean)


march_07_new <- march_07_new %>% mutate(perc.kitchen = (kitchen/(kitchen+laundry+water_AC+rest))*100)

march_07_new <- march_07_new %>% mutate(perc.laundry = (laundry/(kitchen+laundry+water_AC+rest))*100)

march_07_new <- march_07_new %>% mutate(perc.water_AC = (water_AC/(kitchen+laundry+water_AC+rest))*100)

march_07_new <- march_07_new %>% mutate(perc.rest = (rest/(kitchen+laundry+water_AC+rest))*100)


## just checking percentages...

march_07_new$perc.kitchen+march_07_new$perc.laundry+march_07_new$perc.water_AC+march_07_new$perc.rest




ggplot(smart_tidy %>% 
        filter(year == 2007 & month == 3), 
         aes(hour, Watt_hr)) + 
          geom_col(aes(fill = Meter)) +
          ggtitle("Summed up Watt hour consumption per hour in March 2007")



ggplot(march_07_new, 
      aes(hour, (perc.water_AC+perc.kitchen+perc.laundry+perc.rest))) + 
        geom_col(aes(fill = perc.rest)) +
        ggtitle("Share of remaining energy consumption compared to the Meters                 - per hour in March 2007")



## time series analysis

# Aggregating by week without falling into the trap of counting the same
# week twice because it gets cut by the end of the year you know what I mean


# create day of the year column
smart_meters$day_of_year <-strftime(smart_meters$date.time, format = "%j")

# group by day of the year and year
smart_meters_ts <- smart_meters %>% 
  select(year, day_of_year, kw_per_min) %>% 
  group_by(year, day_of_year) %>%
  summarise(mean_kwmin = mean(kw_per_min))


# create unique id for every week
my_weeks <- rep(1:1000, each = 7, length.out = nrow(smart_meters_ts))
smart_meters_ts$my_weeks <- my_weeks

# group by week id in order not to cut weeks at the end of the year
smart_meters_ts <- smart_meters_ts %>% 
  group_by(my_weeks) %>% 
  summarise(mean_kwmin = mean(mean_kwmin))


## creating the time series

smart_meters_ts <-
  ts(smart_meters_ts$mean_kwmin,
     frequency = 365.25/7,
     start = c(2007, 1))


plot.ts(smart_meters_ts)


## Aggregating to weekly data, taking mean of kitchen

smart_meters_kitchen <- smart_meters %>% 
  select(year, week, kitchen) %>% 
  group_by(year,week) %>% 
  summarise(mean_kitchen = mean(kitchen))


smart_meters_kitchen <-
  ts(smart_meters_kitchen$mean_kitchen,
     frequency = 52,
     start = c(2007, 1))

plot.ts(smart_meters_kitchen)


## simple moving average for kW per minute

SMA_smart_meters_ts <- SMA(smart_meters_ts, n = 3)

plot.ts(SMA_smart_meters_ts)



## decomposing the weekly time series for kW per minute

dec_smart_meters_ts <- decompose(smart_meters_ts)

plot(dec_smart_meters_ts)



## forecasting energy consumption using Holt Winters...

forecast_energycons <- HoltWinters(smart_meters_ts, 
                                   beta = FALSE,
                                   gamma = FALSE,
                                   l.start = 24.74)


forecast_energycons
plot(forecast_energycons)

forecast_energycons$SSE
forecast_energycons$fitted

## ... for 52 weeks more (so for one year more, which is 2010)

forecast_energycons_HW <- forecast(forecast_energycons,
                                               h = 52)
forecast_energycons_HW

plot(forecast_energycons_HW)
plot.forecast(forecast_energycons_HW)  # ??? no longer visible ??


## ACF of residuals of Holt Winters forecast

acf(forecast_energycons_HW$residuals, 
    lag.max = 10,
    na.action = na.pass)

plot(forecast_energycons_HW$residuals)


## .. and the partial ACF likewise

pacf(forecast_energycons_HW$residuals, 
    lag.max = 10,
    na.action = na.pass)

plot.ts(forecast_energycons_HW$residuals)  # same as above or ??


## Ljung Box Test

Box.test(forecast_energycons_HW$residuals, 
         lag = 10, 
         type = "Ljung-Box")

