pacman::p_load("forecast")

combined_data_relhum_forpanel <- readRDS(here::here("data","combined_data_relhum_forpanel_revised.rds"))

head(combined_data_relhum_forpanel)
names(combined_data_relhum_forpanel)


library(tidyverse)
fortsdata <- combined_data_relhum_forpanel |> 
  dplyr::filter(state_name=="Assam")

library(fpp)

# for average 25
time_series_avg25=ts(fortsdata$avg_25,
                     frequency = 1, start = c(2011,1))


## Plotting graph without forecasting 
plot(time_series_avg25, main = "Graph without forecasting", 
     col.main = "darkgreen")

library(zoo)
library(ggplot2)
library(scales)

autoplot(as.zoo(time_series_avg25), geom="line")+
  scale_y_continuous(limits = c(0,25))+
  scale_x_discrete(limits = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  labs(x="", y="Temperature in Celsius",
       title="Average Temperature", subtitle = "2.5 percentiles")+
  theme_minimal()


## Fitting model using arima model  
fit_avg25 <- auto.arima(time_series_avg25) 

## Next 10 forecasted values  
forecastedValues_avg25 <- forecast(fit_avg25, 10)

## Print forecasted values 
print(forecastedValues_avg25) 

plot(forecastedValues_avg25, main = "Graph with forecasting", 
     col.main = "darkgreen") 

autoplot(as.zoo(time_series_avg25), geom="line")+
  scale_y_continuous(limits = c(0,25))+
  scale_x_discrete(limits = c(2011,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  labs(x="", y="Temperature in Celsius",
       title="Average Temperature", subtitle = "2.5 percentiles")+
  theme_minimal()


autoplot(forecastedValues_avg25)+ 
  ggtitle("Assam 2.5 average temperatures forecast") + 
  xlab("Time (year)") + ylab("Temperatures (C)") +
  theme_bw() +
  scale_y_continuous(limits = c(0,25))+
  #scale_x_discrete(limits = c(2020,2012,2013,2014,2015,2016,2017,2018,2019,2020,2021))+
  
  theme(plot.title = element_text(hjust = 0.5)) 
 # scale_x_continuous(breaks = seq(from = 2020, to =  2029, by = 1))


stat_smooth(
  color = "#FC4E07", fill = "#FC4E07",
  method = "loess"
)


# For average 975
time_series_avg975=ts(fortsdata$avg_975,
                     frequency = 1, start = c(2011,1))


## Plotting graph without forecasting 
plot(time_series_avg975, main = "Graph without forecasting", 
     col.main = "darkgreen")


## Fitting model using arima model  
fit_975avg <- auto.arima(time_series_avg975) 

## Next 10 forecasted values  
forecastedValues <- forecast(fit_975avg, 10)

## Print forecasted values 
print(forecastedValues) 

plot(forecastedValues, main = "Graph with forecasting", 
     col.main = "darkgreen") 


# for variation 25
time_series_var25=ts(fortsdata$variation_25,
                     frequency = 1, start = c(2011,1))


## Plotting graph without forecasting
plot(time_series_var25, main = "Graph without forecasting", 
     col.main = "darkgreen")


## Fitting model using arima model
fit_var25 <- auto.arima(time_series_var25)

## Next 10 forecasted values
forecastedValues <- forecast(fit_var25, 10)

## Print forecasted values
print(forecastedValues)

plot(forecastedValues, main = "Graph with forecasting", 
     col.main = "darkgreen")

# For variation 975
time_series_var975=ts(fortsdata$variation_975,
                     frequency = 1, start = c(2011,1))


## Plotting graph without forecasting
plot(time_series_var975, main = "Graph without forecasting", 
     col.main = "darkgreen")


## Fitting model using arima model
fit_var975 <- auto.arima(time_series_var975)

## Next 10 forecasted values
forecastedValues <- forecast(fit_var975, 10)

## Print forecasted values
print(forecastedValues)

plot(forecastedValues, main = "Graph with forecasting", 
     col.main = "darkgreen")

