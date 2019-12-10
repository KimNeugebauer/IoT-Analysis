## Forecasting energy consumption

## Loading libraries and data

library(RMySQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(plotly)
library(tidyverse)
library(forecast)
library(tseries)
library(TSstudio)
library(scales)


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
  gather(Meter, Watt_hr,  `kitchen`,`laundry`,`water_AC`,`rest`)

smart_tidy <- smart_tidy %>% 
  mutate(kW_hr = Watt_hr/1000)


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



## Plots to visualize total energy consumption of the household

ggplot(smart_tidy %>% 
         filter(year == 2007 & month == 10), 
       aes(hour, Watt_hr)) + 
  geom_col(aes(fill = Meter)) +
  theme_economist() + scale_colour_economist() +
  theme(legend.position = "bottom") +
  ggtitle("Hourly total energy consumption in 10 / 2007 per Meter")



## the next two plots take forever..

ggplot(smart_tidy, 
      aes(year, kW_hr)) + 
      geom_col(aes(fill = Meter)) +
      theme_economist() + scale_colour_economist() +
      theme(legend.position = "bottom") +
      xlab("Year") + ylab("Kilowatt per hour") +
      ggtitle("Total yearly energy consumption devided by Meter")



ggplot(smart_tidy %>% filter(year == 2007), 
       aes(month, kW_hr)) + 
  geom_col(aes(fill = Meter)) +
  theme_economist() + scale_colour_economist() +
  theme(legend.position = "bottom") +
  xlab("Month") + ylab("Kilowatt per hour") +
  ggtitle("Monthly energy consumption in 2007 by Meter")
  #scale_x_date(labels=date_format("%m"))

smart_tidy$month <- sprintf("%02d", smart_tidy$month)




## Saving the pre-processed data frame

saveRDS(smart_meters, "smart_meters.R")

