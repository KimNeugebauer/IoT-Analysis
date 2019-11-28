## Exploratory Analysis

## Loading libraries and data

library(RMySQL)
library(DBI)
library(dplyr)
library(ggplot2)
library(lubridate)
library(plotly)
library(tidyverse)
library(arules)



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

yr06_select <- dbGetQuery(con, "SELECT id, 
                                       Date, 
                                       Time, 
                                       Global_active_power, 
                                       Global_reactive_power, 
                                       Global_intensity, 
                                       Voltage, 
                                       Sub_metering_1, 
                                       Sub_metering_2, 
                                       Sub_metering_3 FROM yr_2006")

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

yr10_select <- dbGetQuery(con, "SELECT id, 
                                       Date, 
                                       Time, 
                                       Global_active_power, 
                                       Global_reactive_power, 
                                       Global_intensity, 
                                       Voltage, 
                                       Sub_metering_1, 
                                       Sub_metering_2, 
                                       Sub_metering_3 FROM yr_2010")

## combining year 2007, 2008 and 2009
newDF <- bind_rows(yr07_select, yr08_select, yr09_select)


## Investigating the structure of attributes, explring the new DF

summary(newDF$Global_active_power)
summary(newDF$Global_reactive_power)
summary(newDF$Global_intensity)

summary(newDF$Sub_metering_1)
summary(newDF$Sub_metering_2)
summary(newDF$Sub_metering_3)


head(newDF$Time)
tail(newDF$Time)

head(newDF$Date)
tail(newDF$Date)


## plots and histograms

hist(newDF$Sub_metering_1)
hist(newDF$Sub_metering_2)
hist(newDF$Sub_metering_3)


## Combining date and time 

newDF$date.time <- NULL
newDF$date.time <- paste(newDF$Date, newDF$Time)


## date and time settings / convertion 

newDF$date.time <- as.POSIXct(newDF$date.time, 
                              tz = "Europe/Paris", 
                              format = c("%Y-%m-%d %H:%M:%S"))


str(newDF)
summary(newDF)
summary(newDF$date.time)


newDF$date.tim[1:30]


## Separating from date.time year, month and day

newDF$year <- year(newDF$date.time)
newDF$month <- month(newDF$date.time)
newDF$week <- week(newDF$date.time)
newDF$day <- day(newDF$date.time)
newDF$hour <- hour(newDF$date.time)

table(newDF$hour)
table(newDF$month)


## Factoring the time data

newDF$day <- as.factor(newDF$day)
newDF$year <- as.factor(newDF$year)
newDF$month <- as.factor(newDF$month)
newDF$hour <- as.factor(newDF$hour)


hist(newDF$year, 
     xlab = "Year", 
     labels = TRUE)

hist(newDF$month,
     xlab = "Month")

hist(newDF$day,
     xlab = "Day Number")

hist(newDF$hour,
     xlab = "Hour of the day")

ggplot(newDF, aes(hour)) +
  geom_histogram(bins = 24)


## Plot and table for the usage of power per year ..

newDF %>% 
  group_by(year) %>% 
  summarise(mean_per_year = mean(Global_active_power)) %>% 
  ggplot(mapping = aes(x = year, 
                       y= mean_per_year)) + 
  ggtitle("Mean of global active power per year") + 
  geom_bar(stat = "identity")

newDF %>% group_by(newDF$year) %>% 
  summarise(mean(Global_active_power))


# ... and per month

newDF %>% 
  group_by(month) %>% 
  summarise(mean_per_month = mean(Global_active_power)) %>% 
  ggplot(mapping = aes(x = month, 
                       y= mean_per_month)) + 
  ggtitle("Mean of global active power per month") + 
  geom_bar(stat = "identity")

newDF %>% group_by(newDF$month) %>% 
  summarise(mean(Global_active_power))


# ... and per hour

newDF %>% 
  group_by(hour) %>% 
  summarise(mean_per_hour = mean(Global_active_power)) %>% 
  ggplot(mapping = aes(x = hour, 
                       y= mean_per_hour)) + 
  ggtitle("Mean of global active power per hour") + 
  geom_bar(stat = "identity")


newDF %>% group_by(newDF$hour) %>% 
  summarise(mean(Global_active_power))



## analyzing the sub meters

sd(newDF$Sub_metering_1)
sd(newDF$Sub_metering_2)
sd(newDF$Sub_metering_3)


## transformning the remaining power usage into sub meter 4

smart_meters <- newDF %>% 
  mutate(kw_per_min =(Global_active_power*1000)/60) %>% 
  mutate(Sub_metering_4 = kw_per_min 
         - Sub_metering_1 - Sub_metering_2 - Sub_metering_3)

sum(newDF$Sub_metering_1, newDF$Sub_metering_2, newDF$Sub_metering_3)
sum(smart_meters$Sub_metering_4) 


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



# plot with only the winter months

smart_meters %>% 
  group_by(month) %>% 
  summarise(sum_laundry = sum(laundry)) %>% 
  filter(month == c(1,2,12))  %>% 
  ggplot(aes(as.factor(month), sum_laundry)) +
  geom_bar(stat = "identity")

# plot with only the summer months

newDF %>% 
  group_by(month) %>% 
  summarise(sum_laundry = sum(laundry)) %>% 
  filter(month == 6 | month == 7 | month ==8)  %>% 
  ggplot(aes(as.factor(month), sum_laundry)) +
  geom_bar(stat = "identity")


smart_meters %>% 
  group_by(day) %>% 
  filter(year == 2007 & month == 2) %>% 
  summarise(sum_rest = sum(rest)) %>% 
  ggplot(aes(day, sum_rest)) +
  geom_bar(stat = "identity")



## Investigating the 18th of January 2007 (Thursday)

weekday.winter <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$month == 1 & 
           smart_meters$day == 18)

plot(weekday.winter$hour,weekday.winter$laundry)
plot(weekday.winter$hour,weekday.winter$kitchen)
plot(weekday.winter$rest)
plot(weekday.winter$kw_per_min)


plot_ly(weekday.winter, 
        x = ~weekday.winter$date.time, 
        y = ~weekday.winter$laundry, 
        name = "Laundry", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~weekday.winter$kitchen, 
            name = "Kitchen", mode = 'lines') %>%
  add_trace(y = ~weekday.winter$water_AC, 
            name = "Water-heating and AC", mode = 'lines') %>% 
  add_trace(y = ~weekday.winter$rest, 
            name = "Remaining energy consumption", mode = 'lines') %>% 
  layout(title = "Power Consumption January 18th 2007",
         xaxis = list(title = "Time"),
         yaxis = list(title = "watt-hour of active energy consumption"))



## Investigating the 20th of January 2007 (Saturday)

weekend.winter <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$month == 1 & 
           smart_meters$day == 20)

plot(weekend.winter$laundry)
plot(weekend.winter$kitchen)
plot(weekend.winter$heating)
plot(weekend.winter$rest)
plot(weekend.winter$kw_per_min)


plot_ly(weekend.winter, 
        x = ~weekend.winter$date.time, 
        y = ~weekend.winter$laundry, 
        name = "Laundry", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~weekend.winter$kitchen, 
            name = "Kitchen", mode = 'lines') %>%
  add_trace(y = ~weekend.winter$water_AC, 
            name = "Water-heating and AC", mode = 'lines') %>% 
  add_trace(y = ~weekend.winter$rest, 
            name = "Remaining energy consumption", mode = 'lines') %>% 
  layout(title = "Power Consumption January 20th 2008",
  xaxis = list(title = "Time"),
  yaxis = list(title = "watt-hour of active energy consumption"))


## Investigating the 15th of August 2007 (Wednesday)

weekday.summer <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$month == 8 & 
           smart_meters$day == 15)

plot(weekday.summer$hour,weekday.summer$laundry)
plot(weekday.summer$hour,weekday.winter$kitchen)
plot(weekday.summer$hour,weekday.winter$heating)
plot(weekday.summer$rest)
plot(weekday.summer$kw_per_min)


plot_ly(weekday.summer, 
        x = ~weekday.summer$date.time, 
        y = ~weekday.summer$laundry, 
        name = "Laundry", type = 'scatter', mode = 'lines') %>%
  add_trace(y = ~weekday.summer$kitchen, 
            name = "Kitchen", mode = 'lines') %>%
  add_trace(y = ~weekday.summer$water_AC, 
            name = "Water-heating and AC", mode = 'lines') %>% 
  add_trace(y = ~weekday.summer$rest, 
            name = "Remaining energy consumption", mode = 'lines') %>% 
  layout(title = "Power Consumption August 15th 2007",
         xaxis = list(title = "Time"),
         yaxis = list(title = "watt-hour of active energy consumption"))


## Investigating the 18th of August 2007 (Saturday)

weekend.summer <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$month == 8 & 
           smart_meters$day == 18)

plot(weekend.summer$hour,weekend.summer$laundry)
plot(weekend.summer$hour,weekend.summer$kitchen)
plot(weekend.summer$hour, weekend.summer$heating)
plot(weekend.summer$hour,weekend.summer$rest)
plot(weekend.summer$hour,weekend.summer$kw_per_min)


plot_ly(weekend.summer, 
        x = ~weekend.summer$date.time, 
        y = ~weekend.summer$laundry, 
        name = "Laundry", type = 'scatter', mode = 'bar') %>%
  add_trace(y = ~weekend.summer$kitchen, 
            name = "Kitchen", mode = 'bar') %>%
  add_trace(y = ~weekend.summer$water_AC, 
            name = "Water-heating and AC", mode = 'bar') %>% 
  add_trace(y = ~weekend.summer$rest, 
            name = "Remaining energy consumption", mode = 'bar') %>% 
  layout(title = "Power Consumption August 18th 2007",
         xaxis = list(title = "Time"),
         yaxis = list(title = "watt-hour of active energy consumption"))


## Investigating KW 4 in 2007 (22.-28. January 2007)

kw4_07 <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$week == 4)

plot(kw4_07$day,kw4_07$laundry)
plot(kw4_07$day,kw4_07$kitchen)
plot(kw4_07$day,kw4_07$heating)



plot_ly(kw4_07, 
        x = ~kw4_07$date.time, 
        y = ~kw4_07$laundry, 
        name = "Laundry", type = 'scatter', mode = 'bar') %>%
  add_trace(y = ~kw4_07$kitchen, 
            name = "Kitchen", mode = 'bar') %>%
  add_trace(y = ~kw4_07$water_AC, 
            name = "Water-heating and AC", mode = 'bar') %>% 
  add_trace(y = ~kw4_07$rest, 
            name = "Remaining energy consumption", mode = 'bar')


## Investigating March 2007

march_07 <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$month == 3)

plot(march_07$laundry)
plot(march_07$heating)



plot_ly(march_07, 
        x = ~march_07$date.time, 
        y = ~march_07$laundry, 
        name = "Laundry", type = 'scatter', mode = 'bar') %>%
  add_trace(y = ~march_07$kitchen, 
            name = "Kitchen", mode = 'bar') %>%
  add_trace(y = ~march_07$heating, 
            name = "Water-heating and AC", mode = 'bar')


## Investigating July 2007

july_07 <- smart_meters %>% 
  filter(smart_meters$year == 2007 & 
           smart_meters$month == 7)

plot(july_07$laundry)
plot(july_07$heating)



plot_ly(july_07, 
        x = ~july_07$date.time, 
        y = ~july_07$laundry, 
        name = "Laundry", type = 'scatter', mode = 'bar') %>%
  add_trace(y = ~july_07$kitchen, 
            name = "Kitchen", mode = 'bar') %>%
  add_trace(y = ~july_07$water_AC, 
            name = "Water-heating and AC", mode = 'bar')


## total power usage splitted by all sub meters


energy_per_meter <- smart_tidy %>% 
                         filter(smart_tidy$year == 2007 &
                                smart_tidy$month == 1 &
                                smart_tidy$day == 19) %>% 
                         group_by(Meter) %>% 
                         summarise(mean(Watt_hr))


ggplot( smart_tidy %>% 
          filter(smart_tidy$year == 2007 &
                   smart_tidy$month == 9)  %>% 
          group_by(Meter) %>% 
          summarise(mean_watt = mean(Watt_hr))) +
  geom_bar(aes(Meter,mean_watt), 
           stat = "identity",
           fill = "lightblue")


## year proportional

f = smart_tidy %>%
  group_by(year, Meter) %>% 
  summarise(sum=sum(Watt_hr))

ggplot(f) +
  geom_bar( aes(x =year, y =sum),
            group=f$Meter,
            stat='identity') +
  labs(x='Year', y='Proportion of Energy Usage') +
  ggtitle('Proportion of Total Yearly Energy Consumption')
 




## mean watt per hour

ggplot( smart_tidy %>% 
          filter(smart_tidy$year == 2007 &
                   smart_tidy$month == 1)  %>% 
          group_by(hour) %>% 
          summarise(mean_watt = mean(Watt_hr))) +
    geom_bar(aes(hour,mean_watt), 
             stat = "identity",
             fill = "lightgreen")

