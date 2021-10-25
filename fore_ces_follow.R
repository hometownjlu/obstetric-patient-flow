#time series of Obstetric follow

library(forecast)

setwd("D:/R test")
Mat_fol=read.csv('ts-mas12-bak.csv')

Mat_fol=Mat_fol[,-1]

summary(Mat_fol)
names(Mat_fol)
Mat_fol

Mat_folts=ts(Mat_fol,frequency =12)
plot.ts(Mat_folts)
Mat_folts
#Mat_folts=ts(Mat_fol)
#plot(Mat_fol)

#decompose random seasonal trend
debirts=decompose(Mat_folts)
debirts$seasonal
plot(debirts)

#del seasonal and random factor
debirtsdel=Mat_fol-debirts$seasonal-debirts$random
plot(debirtsdel)

#forecast

Forecast=forecast.HoltWinters(Mat_folts,h=12)
plot(Forecast)

Forecast=forecast(HoltWinters(train), h =12 )
plot(Forecast)

tsdiag(Mat_fol)
##去平滑时间序???
library(TTR)
Mat_folma=SMA(Mat_folts,n=5)
plot.ts(Mat_folma)
#log for smoonth
logbirthsts=log(Mat_folts)
logbirthsts
plot.ts(logbirthsts)
Forecast=forecast(logbirthsts,h=12)
Forecast=forecast(Mat_folma,h=12)
plot(Forecast)

#“l.start”和“b.start”的参数指定水平和趋势的初始值
#常见的设定水平初始值为时间序列的第一个值（608）
#斜率的初始值则是其第二个值减去第一个值（9）
#Mat_fol_forest=HoltWinters(Mat_folts,gamma = FALSE,l.start = 580,b.start = 10)
Mat_fol_forest=HoltWinters(Mat_folts,gamma = FALSE)
Mat_fol_forest
Mat_fol_forest$SSE
#sse 是误差
Mat_fol_forest$fitted
plot(Mat_fol_forest)
#
fcsthot=forecast(Mat_fol_forest,h=12)
Forecast=forecast:::forecast.HoltWinters(Mat_fol_forest,h=12)
plot(Forecast)
Forecast$residuals
Forecast
plot(fcsthot)
#log for smoonth
logMat_fol=log(Mat_folts)
Mat_fol_forest=HoltWinters(logMat_fol,gamma = FALSE,l.start = 580,b.start = 10)
Mat_fol_forest
Mat_fol_forest$SSE
#sse 是误差
Mat_fol_forest$fitted
plot(Mat_fol_forest)



#两个大、小阴影区间分别代表 80,95%的可能性
#lag.max 是预测误差延迟1-20阶的相关图
acf(Forecast$residuals,na.action = na.pass,lag.max = 20)
acf(Forecast,na.action = na.pass,lag.max = 20)
Forecast
plotForecastErrors(Forecast$residuals)
Box.test(Forecast$residuals,lag=20,type = "Ljung-Box")
#P值小???0.05 ，证??? 预测样本内预测误差在1-20阶内???0自相关？？？
#p??? 0.114
pacf(Forecast$residuals,lag.max = 20)
plot.ts(Forecast$residuals)
#residuals 正态分布，且均值为0，则模型不可再优化
hist(Forecast$residuals)
#用图形的方式显示正太分布


# diff 1阶差
bir_dif=diff(Mat_folts,differences = 1)
plot.ts(bir_dif)
#差分后，如不平稳再加1???
ski_dif=diff(Mat_folts,differences = 2)
plot.ts(ski_dif)

#holt 只预测已有时间内的数据，不预测未来数据。forecast 预测将来

auto.arima(Mat_folts)
ski_ari=arima(Mat_folts,order=c(0,1,0))
plot.ts(ski_ari)
ski_ari
ski_ari_fo=forecast(ski_ari)
plot(ski_ari_fo)
Box.test(ski_ari_fo$residuals,lag=20,type = 'Ljung-Box')
hist(ski_ari_fo$residuals)

tsdiag(ski_ari)
#Ljung p 值大于0.1时就是准确的。
