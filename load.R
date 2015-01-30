
library(forecast)

dat <- read.csv('/home/sicarul/dev/blueforecast/export_blue.csv')

blue <- subset(dat, source_id == 'la_nacion')

agg <- aggregate(blue$value_sell, by=list(strftime(blue$date,format="%y/%m")), FUN=mean)

base = agg[1:36,]
test = agg[37:46,]

bts <- ts(base$x, start=c(2011, 1), frequency=12)
tts <- ts(test$x, start=c(2014, 1), frequency=12)
plot(bts)
plot(tts)


seasonal <- stl(bts, s.window=12)
plot(seasonal)
monthplot(bts)
seasonplot(bts)
bts_adj <- seasadj(seasonal)


fit <- Arima(bts, order=c(0,1,1))
summary(fit)
Acf(residuals(fit))
Box.test(residuals(fit), lag=24, fitdf=4, type="Ljung")
plot(forecast(fit))

fit_ar <- ar(bts, method = "mle", n.ahead=12)
fcast_ar <- forecast(fit_ar, h=12)
plot(fcast_ar)
ac_ar <- accuracy(fcast_ar, tts)
ac_ar

fit_arima <- auto.arima(bts, trace=TRUE, allowdrift=TRUE)
#fit_arima <- Arima(bts, order=c(0,1,1), seasonal=c(0,1,0))
fcast_arima <- forecast(fit_arima, h=12)
ac_arima <- accuracy(fcast_arima, tts)
ac_arima
plot(fcast_arima)
lines(tts, col="purple", lwd=2)

fit_ets <- ets(bts)
fcast_ets <- forecast(fit_ets, h=12)
plot(fcast_ets)
ac_ets <- accuracy(fcast_ets, tts)
lines(tts, col="purple", lwd=2)
