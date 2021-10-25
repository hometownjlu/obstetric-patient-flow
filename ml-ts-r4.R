setwd("D:/R test")
library(keras)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(lubridate)
library(forecast)
library(ggplot2)

y=read.csv('tst-mas-d-96.csv')
y=y[,-1]
flen=14

y=ts(y,frequency =7)
train=y[50:189]
test=y[190:203]

plot(ts(y[]))
test
train
train=ts(train,frequency =7)


stl <- stlm(train, modelfunction = ar)
STLM <- forecast(stl, h=flen)
STLM
STLM$model

pdf("STLM96.pdf")
plot(forecast(STLM, h =flen ))
dev.off()


ARIMA <- forecast(auto.arima(train, lambda=0, biasadj=TRUE),h=flen)
plot(forecast(ARIMA, h =flen ))

ARIMA
ARIMA$model
pdf("ARIMA96.pdf")
plot(ARIMA)
dev.off()


NNAR <- forecast(nnetar(train), h=flen)
plot(forecast(NNAR, h =flen ))
NNAR$model


pdf("NNAR96.pdf")
plot(NNAR)
dev.off()

HTW=forecast(HoltWinters(train), h =flen )
HTW
HTW$model
pdf("HTW96.pdf")
plot(HTW)
#autoplot(y)+autolayer(HTW,PI=FALSE)+xlab("Day") + ylab("Num")
dev.off()

library("forecastxgb")
model <- xgbar(train)
xgb <- forecast(model, h = 14)
model$model
plot(xgb)
pdf("XGB96.pdf")
plot(xgb)
dev.off()

accuracy(xgb,test)
accuracy(NNAR,test)
accuracy(STLM,test)
accuracy(HTW,test)
accuracy(ARIMA,test)


ARIMA
NNAR
HTW
STLM










summary(NNAR$model)
str(NNAR)
HTW
ARIMA

shape(STLM)
STLM$residuals

ST=STLM[,-1]
STLM[2,]
NN=NNAR[,-1]
HT=HTW[,-1]
AR=ARIMA[,-1]

data1=STLM$Forecast
data1
r2_general <-function(preds,actual){ 
  return(1- sum((preds - actual) ^ 2)/sum((actual - mean(actual))^2))
}

r2_general(NNAR,test)

rsq <- function(x, y) summary(lm(y~x))$r.squared
rsq(NNAR,test)

ptrain=ts(ptrain)
plot(ptrain)
library(ggplot2)
test=ts(test)
autoplot(y)+autolayer(NNAR,series="NNAR")+xlab("Day")+ ylab("Num")+ggtitle("Australian")
autoplot(y)+autolayer(STLM,series="STLM", PI=FALSE)+xlab("Day") + ylab("Num")+ggtitle("Australian")
autoplot(y)+autolayer(HTW,series="HTW", PI=FALSE)+xlab("Day") + ylab("Num")+ggtitle("Australian")
autoplot(y)+autolayer(ARIMA,series="ARIMA", PI=FALSE)+xlab("Day") + ylab("Num")+ggtitle("Australian")


?xlab




accuracy(TSM)
accuracy(stl)
accuracy(ARIMA)
accuracy(NNAR)
accuracy(HTW)


Forecast=HoltWinters(y)
plot(forecast(Forecast, h =flen ))




Combination <- (TSM[["mean"]] + ARIMA[["mean"]] +NNAR[["mean"]] )/5

autoplot(y) +
  autolayer(TSM, series="ETS", PI=FALSE) +
  autolayer(ARIMA, series="ARIMA", PI=FALSE) +
  autolayer(NNAR, series="NNAR", PI=FALSE) +
  xlab("Year") + ylab("$ billion") +
  ggtitle("Australian monthly expenditure on eating out")
autoplot(y)
autoplot(TSM, series="ETS", PI=FALSE)
hltwin <- HoltWinters(y, gamma = TRUE, beta = FALSE)
plot(hltwin)
m2 <- HoltWinters(y, gamma = FALSE, beta = FALSE)
lines(fitted(m2)[,1], col = 3)

fcsthot=forecast(y,h=12)



Forecast=forecast.HoltWinters(y,h=12)
plot(Forecast)

etsmod <- ets(y)
plot(forecast(etsmod))

?ets



ETS <- forecast(ets(train), h=flen)

train=ts(train,frequency =12)
STL <- stlf(train, lambda=0, h=h, biasadj=TRUE)


TBATS <- forecast(tbats(train, biasadj=TRUE), h=flen)


??NNAR



autoplot(y)
autoplot(snaive(y, h=10))
autoplot(meanf(y, h=10))
autoplot(naive(y, h=10))
autoplot(rwf(y, h=10)) # Equivalent alternative
autoplot(snaive(y, h=10))


fit <- nnetar(y)
autoplot(forecast(fit,h=10))
ggAcf(fit)

autoplot(meanf(y, h=10)) 

autolayer(rwf(y, h=10)


          
          meanf(y,h=10)
          naive(y, h=10)
          rwf(y, h=10) # Equivalent alternative
          snaive(y, h=10)

autolayer(meanf(y, h=10),series="Mean", PI=FALSE) 
autolayer(rwf(y, h=10),series="Naïve", PI=FALSE)
autolayer(rwf(y, drift=TRUE, h=10), series="Drift", PI=FALSE)
ggtitle("Google stock (daily ending 6 Dec 2013)") +


googfc1 <- meanf(y, h=10)
googfc2 <- rwf(y, h=10)
googfc3 <- rwf(y, drift=TRUE, h=10)
googfc1

autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)





beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)


googfc1 <- meanf(y, h=40)
googfc2 <- rwf(y, h=40)
googfc3 <- rwf(y, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)


