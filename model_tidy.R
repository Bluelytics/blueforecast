
library(forecast)
library(sweep)
library(timetk)
library(tidyverse)
library(tidyquant)
library(lubridate)

blue <- read_csv('export_blue.csv') %>% select(date, value)
agg <- setNames(aggregate(blue$value, by=list(as.Date(blue$date)), FUN=mean), c('date', 'value'))
days <- data.frame ( date = as.Date(seq.Date(ymd(min(agg$date)), ymd(max(agg$date)), by = "1 day")))
blue_data <- na.locf(merge(days, agg, by="date", all.x=TRUE)) %>% mutate(value=as.numeric(value))


blue_data %>%
    ggplot(aes(date, value)) +
    geom_line(col = palette_light()[1]) +
    geom_point(col = palette_light()[1]) +
    theme_tq() +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") 

blue_ts <- tk_ts(blue, start = as.Date('2013-01-02'), freq = 365)
has_timetk_idx(blue_ts)

fit_arima <- auto.arima(blue_ts)

fit_arima
sw_tidy(fit_arima)
sw_glance(fit_arima) %>%
    glimpse()

sw_augment(fit_arima, timetk_idx = TRUE) %>%
    ggplot(aes(x = index, y = .resid)) +
    geom_point() + 
    geom_hline(yintercept = 0, color = "red") + 
    labs(title = "Residual diagnostic") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    theme_tq()

fcast_arima <- forecast(fit_arima, h = 12)
fcast_tbl <- sw_sweep(fcast_arima, timetk_idx = TRUE)

fcast_tbl %>%
    ggplot(aes(x = index, y = value, color = key)) +
    # 95% CI
    geom_ribbon(aes(ymin = lo.95, ymax = hi.95), 
                fill = "#D5DBFF", color = NA, size = 0) +
    # 80% CI
    geom_ribbon(aes(ymin = lo.80, ymax = hi.80, fill = key), 
                fill = "#596DD5", color = NA, size = 0, alpha = 0.8) +
    # Prediction
    geom_line() +
    geom_point() +
    # Aesthetics
    labs(title = "Dolar Blue Forecast: ARIMA", x = "", y = "Thousands of Tons",
         subtitle = "sw_sweep tidies the auto.arima() forecast output") +
    scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
    scale_color_tq() +
    scale_fill_tq() +
    theme_tq()
