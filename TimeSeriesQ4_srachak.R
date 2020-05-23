require(fpp)
require(fma)
data(package = "fma")
data(package="fpp")

data(ukcars)
plot(ukcars, ylab = "Production, thousands of cars")
stlFit <- stl(ukcars, s.window = "periodic")
plot(stlFit)
adjusted <- seasadj(stlFit)
plot(adjusted)

fcastHoltDamp = holt(adjusted, damped=TRUE, h = 8)
plot(ukcars, xlim = c(1977, 2008))
lines(fcastHoltDamp$mean + 
        stlFit$time.series[2:9,"seasonal"], 
      col = "red", lwd = 2)

dampHoltRMSE = sqrt(mean(((fcastHoltDamp$fitted + stlFit$time.series[,"seasonal"]) - ukcars)^2))
dampHoltRMSE # --> 25.15986

fcastHolt = holt(adjusted, h = 8)
plot(ukcars, xlim = c(1997, 2008))
lines(fcastHolt$mean + stlFit$time.series[2:9,"seasonal"], 
      col = "red", lwd = 2)

holtRMSE = sqrt(mean(((fcastHolt$fitted + stlFit$time.series[,"seasonal"]) - ukcars)^2))
holtRMSE # --> 25.26041


# A - You are tasked with using the ets() method for automatically selecting a seasonal model for the data. Which model was selected as the better one?
ets_ukcars <- ets(ukcars)
# Model selected by ets is the ANA model. It shows additve error, no trend and additive seasonality. 
ets_ukcars # aic value --> 1277.752

# B - Compare the RMSE of the fitted ets() model with the RMSE of the model that Sam obtained using an STL decomposition with Holt's method. Which gives better in-sample fit (if any)?
accuracy_ukcars <- accuracy(ets_ukcars)
accuracy_ukcars[,'RMSE'] # --> 25.23244
dampHoltRMSE # --> 25.15986
holtRMSE # --> 25.26041
# There isn't a lot of difference in the RMSE values of all 3 models. Therefore, there is no model that is better than the others in terms of insample fit. 

# C - Compare the two-year forecasts from these two approaches? Which seems more reasonable?
ets_forecast <- forecast(ets_ukcars)
ets_forecast
#        Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2005 Q2       427.4885 394.2576 460.7195 376.6662 478.3109
# 2005 Q3       361.3329 322.2353 400.4305 301.5383 421.1275
# 2005 Q4       404.5358 360.3437 448.7280 336.9497 472.1219
# 2006 Q1       431.8154 383.0568 480.5741 357.2455 506.3854
# 2006 Q2       427.4885 374.5571 480.4200 346.5369 508.4401
# 2006 Q3       361.3329 304.5345 418.1313 274.4672 448.1986
# 2006 Q4       404.5358 344.1174 464.9542 312.1338 496.9378
# 2007 Q1       431.8154 367.9809 495.6500 334.1890 529.4419
plot(ets_forecast)
fcastHolt
#       Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2005 Q2       407.6327 374.6716 440.5939 357.2230 458.0425
# 2005 Q3       408.4646 369.9401 446.9890 349.5465 467.3826
# 2005 Q4       409.2964 365.9149 452.6779 342.9501 475.6427
# 2006 Q1       410.1282 362.3798 457.8767 337.1033 483.1531
# 2006 Q2       410.9601 359.2107 462.7095 331.8162 490.1039
# 2006 Q3       411.7919 356.3282 467.2556 326.9675 496.6163
# 2006 Q4       412.6237 353.6783 471.5692 322.4744 502.7731
# 2007 Q1       413.4556 351.2217 475.6894 318.2771 508.6340
plot(fcastHolt)
fcastHoltDamp
# Point Forecast    Lo 80    Hi 80    Lo 95    Hi 95
# 2005 Q2       406.4670 373.4854 439.4486 356.0260 456.9080
# 2005 Q3       406.4664 368.4736 444.4593 348.3614 464.5715
# 2005 Q4       406.4659 364.0486 448.8833 341.5942 471.3377
# 2006 Q1       406.4655 360.0424 452.8885 335.4675 477.4634
# 2006 Q2       406.4651 356.3546 456.5755 329.8277 483.1024
# 2006 Q3       406.4647 352.9194 460.0099 324.5743 488.3551
# 2006 Q4       406.4643 349.6911 463.2376 319.6371 493.2915
# 2007 Q1       406.4640 346.6361 466.2919 314.9651 497.9629
plot(fcastHoltDamp)

# In terms of RMSE there isn't a significant difference in any of the models therefore, it's hard to tell which model is performing better. 
# However, holt models can only handle trend and the data clearly has a seasonality component. Which is why, while ploting we notice that the predicted values with the holt models do not follow the seasonality maintained with the rest of the plot. 
# On the other hand, the ets ANA model chosen takes care of the additive seasonality component. Therefore, the forecasted values also follow the seasonality component. 
# Therefore, ets - ANA model seems more reasonable. 
