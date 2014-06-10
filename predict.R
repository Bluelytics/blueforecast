if (file.exists('local_config.R')){
  source('local_config.R')  
}


library(forecast)
library(RPostgreSQL)
library(RJSONIO)
add.months= function(date,n) seq(date, by = paste (n, "months"), length = 2)[2]

drv <- dbDriver("PostgreSQL")

con <- dbConnect(drv, user='postgres', password='postgres', host='127.0.0.1', dbname="bluelytics")
dbSendQuery(con,"SET datestyle TO iso")
rs <- dbSendQuery(con, "select avg(value_sell) as value, date_trunc('month', date) as date from dolar_blue_dolarblue where source_id <> 'oficial' group by date_trunc('month', date) order by date;")
base_data <- fetch(rs,n=-1)

dbDisconnect(con)
dbUnloadDriver(drv)

start_date <- base_data$date[1]
start_year <- as.numeric(strftime(start_date, format="%Y"))
start_month <- as.numeric(strftime(start_date, format="%m"))

bts <- ts(base_data$value, start=c(start_year, start_month), frequency=12)

fit_arima <- Arima(bts, order=c(0,1,1), seasonal=c(0,1,0))


forecasted <- forecast(fit_arima, h=12)

lower_80 <- forecasted$lower[,1]
lower_95 <- forecasted$lower[,2]
upper_80 <- forecasted$upper[,1]
upper_95 <- forecasted$upper[,2]
mean <- as.numeric(forecasted$mean)

data_forecasted = data.frame(lower_95, lower_80, mean, upper_80, upper_95)

dates_history <- seq(start_date, by='month', length.out=nrow(base_data))
dates_forecasted <- seq(add.months(tail(dates_history, n=1), 1), by='month', length.out=12)

export_history <- strftime(dates_history, '%m/%Y')
export_forecasted <- strftime(dates_forecasted, '%m/%Y')

json_forecasted <- toJSON(data_forecasted)
json_history <- toJSON(data.frame(history=base_data$value))
json_dates_history <- toJSON(export_history)
json_dates_forecasted <- toJSON(export_forecasted)

write(json_forecasted, file.path(path_output, 'json_forecasted.json'))
write(json_history, file.path(path_output, 'json_history.json'))
write(json_dates_history, file.path(path_output, 'json_dates_history.json'))
write(json_dates_forecasted , file.path(path_output, 'json_dates_forecasted.json'))