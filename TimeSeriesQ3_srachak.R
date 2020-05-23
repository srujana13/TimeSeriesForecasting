require(fpp)
require(fma)
data(package = "fma")
data(package="fpp")

data("books")
plot(books)

# A- Apply Holt's linear model to the paperback and hardback book series (data(books), holt()) and compute four-day forecasts in each case.
paperback_forecast <- holt(books[,"Paperback"], h=4)
paperback_forecast.pred <- forecast(paperback_forecast)
# Forecast values of the next 4 days for paperback
paperback_forecast.pred
# plotting of the forecasts
plot(paperback_forecast.pred)

hardcover_forecast <- holt(books[,"Hardcover"], h=4)
hardcover_forecast.pred <- forecast(hardcover_forecast)
# Forecast values of the next 4 days for hardcover
hardcover_forecast.pred
# plotting of the forecasts
plot(hardcover_forecast.pred)

# B- Compare the SSE measures of Holt's method for these two series to those of simple exponential smoothing with ses() method.
ses_paperback <- ses(books[,"Paperback"], h=4)
ses_paperback.pred <- forecast(ses_paperback)
ses_paperback.pred
plot(ses_paperback.pred)

ses_hardcover <- ses(books[,"Hardcover"], h=4)
ses_hardcover.pred <- forecast(ses_hardcover)
ses_hardcover.pred
plot(ses_hardcover.pred)

# SSE for ses models
SSE_ses_paperback <- sum(residuals(ses_paperback)*residuals(ses_paperback)) # --> 33944.82
SSE_ses_hardcover <- sum(residuals(ses_hardcover)*residuals(ses_hardcover)) # --> 30587.69

# SSE for holt models
SSE_holt_paperback <- sum(residuals(paperback_forecast)*residuals(paperback_forecast)) # --> 29085.24
SSE_holt_hardcover <- sum(residuals(hardcover_forecast)*residuals(hardcover_forecast)) # --> 22184.72

# SSE of holt models are lower than ses models in both paperback and hardback series. 

# C- Compare the forecasts for the two series using both methods. Which is best and why?


# Since the SSE values of holt models is less compared to simple exponential smoothing models, the holt models are better at forecasting. 
# Also, the dataset consists of a trend component which hasn't been stripped off before applying ses. And ses models only work for data with no trend and no seasonality. 
# Holt models however can handle linear trend, which is possibly why it's SSE values are lower than ses models. 
# Holt models are best for this data.
