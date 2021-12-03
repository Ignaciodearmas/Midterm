## For this midterm exam the goal is to accuratly forecast Covid-19 cases in the United States.
## The dataset consists of 969,640 observations divided in 9 rows: 
## ID: which is just an identifier column ranging from 1 to 969,640
## County: Specifying the counties within countries. Only 8 Countries have specified counties (Australia, Canada, China, Denmark, France, Netherlands,UK and the US)
## Province: Specifying the province within countries. Only 8 Countries have specified counties (Australia, Canada, China, Denmark, France, Netherlands,UK and the US)
## Country: There are 187 countries in the dataset.
## Population: The row population shows the amount of people that live in the specific County, Province and Region.
## Weight: is the weight of the population against the entire sample.
## Date: The date in the data set ranges from 01/23/2020 to 06/10/2020.
## Target: is an identifier column with two variables fatalities and confirm cases which correlated to what happened that day.
## TargetValue: is a numerical row that expreses how many fatalities and confirm cases occured that specific day.

## Because the dataset has to much information a wanted to focus mainly in the United States as a whole. This was done as the US is the country the most information
## and cases. I did not wanted to compromise the quality of the forecast with countries with missrepresented data.
## The objective of the paper is to Forecast future covid cases in order to calculate and prevent the spread of the virus.


## In this Midterm I am going to use 5 forecasting techniques: 
## Naive model: This simple model uses last observation carried forward method to predict future results.
## Average model: This simple model uses the arithmetic mean of the data to forecast new data. Similar to arima (0,0,0)(0,1,0)
## Drift model: This model uses difference between the first and last observation and does a projection. Similar to arima (0,1,0)
## ETS Model: This model focuses in trends and seasonal components of the data.
## auto.arima model: Lastly the model last model is a autoregresive model that uses past values to predict new ones.

library(forecast)
library(readr)
library(tidyverse)
library(gapminder)
library(ggplot2)
library(fpp2)
library(rdatamarket)
library(tseries)

Country_Count_Train <- Covid_Train %>%
  group_by(Country_Region) %>%
  summarise(counts = n())
##
class(Covid_Train)

str(Covid_Train)
###
US_Cases<-Covid_Train[ which(Covid_Train$Country_Region=='US' & Covid_Train$Target=='ConfirmedCases'),]
US_Cases$Date <-  as.Date(US_Cases$Date, "%Y-%m-%d") 
US_Con <- aggregate(US_Cases$TargetValue, by=list(Category=US_Cases$Date), FUN=sum)
###
USts <- ts(US_Con$x, start = c(2020,01,23), frequency=365)
autoplot(USts)+ylab("Covid Cases")+xlab("Year 2020")+ggtitle("US Covid Cases (2020)")

## Now we can start our forecast models:

## fist we can start with three very simple models. 

Mean_US <- meanf(USts, h = 80)
Naive_US <- naive(USts, h= 80)
drift_US <- rwf(USts, h= 80, drift = T)
plot(Mean_US, plot.conf = F, main = "")
lines(Naive_US$mean, col=123, lwd =2)
lines(drift_US$mean, col=22, lwd=2)
legend("topleft", lty=1, col=c(4,123,22),legend =c("Mean Method", "Naive Method", "Drift Method"))

checkresiduals(Mean_US)
accuracy(Mean_US)
checkresiduals(Naive_US)
accuracy(Naive_US)
checkresiduals(drift_US)
accuracy(drift_US)


## as we can oberserve we..
## Now we are going to do an ETS model

USts_ETS <- ets(USts, model = "ZZZ")
ETS_for <- forecast(USts, h = 60)
autoplot(ETS_for)

checkresiduals(USts_ETS)
accuracy(USts_ETS)

## now we are going to frecast using a arima model.

US_auto <- forecast(auto.arima(USts),h = 40)
US_auto
autoplot(US_auto)

checkresiduals(US_auto)
accuracy(US_auto)

## Limitations: This work has many limitations first being the quantiy of data in the dataset. I beleive that the models can perform better is more datapoint are added to the sample, rightnow I encounter problems implemting Holt-Winters models due to the lack of data. Another limitation are my coding skills
## as I am a begginer in forecasting techniques, there is alwyas the possibility of revisiting this paper to make changes and improve the models as my knowlege increases. Annother limitation is the simplicity of the models as they do not take into consideration many other variables that may be important for more accurate forecasts.

## Future Work: For future work is in my plans to revisit the code and improve in current models to tune them into more accurate ones as well as to implement new forecasting techniques to the data.

## Conclusions: The models of the paper seem to bee very innacurate with high RMSE and MAE values. The models that had the lowest scores in the study were the naive model and auto arima (both with similar scores around RMSE = 7250 and MAE = 4457). This tells us that the forecast in this models is not very reliable. However, it can be used as a base to understant the behavior of Covid within the US population.


