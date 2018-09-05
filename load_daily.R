
library(forecast)
library(tseries)
library(lubridate)

blue <- read_csv('export_blue.csv')

agg <- setNames(aggregate(blue$value_sell, by=list(as.Date(blue$date)), FUN=mean), c('date', 'x'))

days <- data.frame ( date = as.Date(seq.POSIXt(ymd(min(agg$date)), ymd(max(agg$date)), by = "1 day")))

final <- na.locf(merge(days, agg, by="date", all.x=TRUE))
final$x <- as.numeric(final$x)

offset = 730

numtest = round(sqrt(length(final$x)-offset))
numbase = length(final$x)-numtest


base = final[offset:numbase,]
test = final[(numbase+1):length(final$x),]


basedate = min(ymd(base$date))
testdate = min(ymd(test$date))

bts <- ts(base$x, start=c(year(basedate), yday(basedate)), frequency=365)
tts <- ts(test$x, start=c(year(testdate), yday(testdate)), frequency=365)
plot(bts)
plot(tts)

picos_data = data.frame(
#    p1=as.numeric(seq (bts) %in% c(1319:1406)),
#    p2=as.numeric(seq (bts) %in% c(1088:1194)),
#    p3=as.numeric(seq (bts) %in% c(821:872))
    p1=as.numeric(seq (bts) == 1319 - offset),
    p2=as.numeric(seq (bts) == 1088 - offset),
    p3=as.numeric(seq (bts) == 821 - offset)
)

picos = as.matrix(picos_data)


seasonal <- stl(bts, s.window=4)
plot(seasonal)
monthplot(seasonal$time.series[,"seasonal"], main="", ylab="Seasonal")
plot(bts, col="grey")
lines(seasadj(seasonal),col="red",ylab="Seasonally adjusted")
eeadj <- seasadj(seasonal)
fcast_season <- forecast(seasonal, method="naive")
plot(fcast_season)
lines(tts, col="purple", lwd=2)
     
plot(bts)
lines(ma(bts,3),col="red")


dec <- decompose(bts, type="multiplicative")
plot(dec)

tsdisplay(bts)

tsdisplay(diff(bts,30))

tsdisplay(diff(diff(bts,30)))

tsdisplay(diff(diff(diff(bts,30))))


plot(diff(log(bts),7))

adf.test(bts, alternative = "stationary")
ns <- nsdiffs(bts)

fit <- Arima(bts, order=c(2,1,2), include.drift=TRUE,  method="ML")
summary(fit)
tsdiag(fit)
plot(residuals(fit))
Box.test(residuals(fit), lag=60, fitdf=4, type="Ljung")
fcast <- forecast(fit,  h=30)
plot(fcast)
lines(tts, col="purple", lwd=2)

resid <-residuals(fit)
ks.test(resid, 'pnorm', mean(resid), sd(resid))



fit_arima <- auto.arima(bts, d=1,approximation=FALSE, trace=TRUE)
summary(fit_arima)
plot(fit_arima$x,col="red")
lines(fitted(fit_arima),col="blue")
fcast_arima <- forecast(fit_arima, h=30 )
plot(fit_arima$residuals)
ac_arima <- accuracy(fcast_arima, tts)
ac_arima
plot(fcast_arima)
lines(tts, col="purple", lwd=2)


resid <-residuals(fit_arima)
Box.test(resid, lag=60, fitdf=4, type="Ljung")

ks.test(resid, 'pnorm', mean(resid), sd(resid))


fit_ets <- stlf(bts, method="rwdrift", h=30, s.window="periodic")
fcast_ets <- forecast(fit_ets, n.head=30)
summary(fit_ets)
plot(fcast_ets)
lines(tts, col="purple", lwd=2)

plot(fit_ets$x,col="red")
lines(fitted(fit_ets),col="blue")
lines(fitted(fit_ets),col="green")

ac_ets <- accuracy(fcast_ets, tts)
ac_ets


resid <-residuals(fit_ets)
plot(resid)
Box.test(resid, lag=60, fitdf=4, type="Ljung")
ks.test(resid, 'pnorm', mean(resid), sd(resid))

fit_nnet <- nnetar(bts)
fit_nnet
fcast_nnet <- forecast(fit_nnet)
plot(fit_nnet$residuals)
plot(fcast_nnet)
ac_nnetar <- accuracy(fcast_nnet, tts)
ac_nnetar
lines(fit_nnet$fitted, col="red", lwd=2)
lines(tts, col="purple", lwd=2)



