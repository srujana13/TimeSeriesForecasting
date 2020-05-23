install.packages("fpp", dependencies=TRUE)
library(fpp)
data(package="fpp")

#1 Plot and examine for any unusual patterns in the electrical equipment orders time series data (elecequip)
# The data clearly is seasonal as we can see repetitive patterns. It has some trend since the data is not uniform, trend is non linear. 
plot(elecequip)

#2 Decompose this ts using stl() (use s.windows="periodic")
fit <- stl(elecequip, s.window = "periodic")

#3 If the data is seasonal, create and plot the seasonally adjusted data using seasadj()
# Visualizing the decomposed data for seasonality
plot(fit, main="Electrical equipment manufacturing")

# Since, data is seasonal, plotting seasonally adjusting data
seasadj_fit <- seasadj(fit)
plot(elecequip, col="grey", main="Electrical equipment manufacturing")
lines(seasadj_fit, col="red", ylab="Seasonally adjusted")

#4 There is no need to stabilize the data as the slope of the seasonality component in the decomposed data was horizontal, indicating stable variance. 
# It can be verified using the following two commands. The plot after applying BoxCox transformation is simialar to the plot itself, indicating stable variance.
# lambda = BoxCox.lambda(elecequip)
# plot(BoxCox(elecequip, lambda))

#5 Confirm if the resulting data after Step 4 is stationary
# Data is not stationary since it still consists of the trend component. We can also verify this using the acf() as follows: 
# acf shows that the data is slowly decreasing which indicates trend. 
acf(seasadj_fit)

#6 If the data is non-stationary, then take the first difference of the data or run the unit root test to get the proper differencing order and check whether the differencing converted non-stationary ts to a stationarity one
# Since the data is non-stationary and the data is non-seasonal (As we have done above) we will use ndiffs() unit root test to check for number of differences required.
ns <- ndiffs(seasadj_fit) # ns --> 1

# ndiffs() gives us a value of 1, therefore we take the first difference
seasadj_diff_fit <- diff(seasadj_fit, differences = ns)

# Running acf on the new data gives us the following auto-correlation plot, which shows that the data is almost stationary. 
acf(seasadj_diff_fit)

#7 Use auto.arima() on the time series data obtained after Step #6. What are the values of p, d, and q in ARIMA (p, d, q) model that the model has chosen (use summary() on the model)
# p = 3, d = 0, q = 1
arima_fit <- auto.arima(seasadj_diff_fit, seasonal = FALSE)
summary(arima_fit)

#8 Generate your own ARIMA(4, 0, 0), ARIMA (3, 0,0) and ARIMA (2, 0, 0) models using the following command: Arima(ts.data, order=c(4,0,0))
arima_400 <- Arima(seasadj_diff_fit, order=c(4,0,0))
arima_400$aicc # aicc --> 981.3637

arima_300 <- Arima(seasadj_diff_fit, order=c(3,0,0))
arima_300$aicc # aicc --> 981.6565

arima_200 <- Arima(seasadj_diff_fit, order=c(2,0,0))
arima_200$aicc # aicc --> 998.8694

arima_fit$aicc # aicc --> 978.4925
# Since the lowest value of aicc gives us the best model, arima(3,0,1) gives us a better AICc with a value of 978.4925

#9 Examine the residuals of the model that is the best from Step #8 (verify by Acf and Box.test)? Do they behave like white noise? (residuals(fit))
# acf --> all data is within the two blue lines/ critical values and hence it is white noise. 
acf(residuals(arima_fit))
# p-value --> 0.9007 
# Since p-value is large, it means that the residuals are not distinguishable from white noise and hence, it is white noise.
Box.test(residuals(arima_fit))

#10 If the model is proper, then plot the forecast with plot(forecast(fit))
plot(forecast(arima_fit))
