install.packages("fpp", dependencies=TRUE)
require(fpp)
data(package="fpp")

# Loading the dj dataset
data(dj)
# Plotting the data, we see that there is a non linear trend. There is no specific seasonality. 
plot(dj)
# Since the auto-correlation plot represents data that is positive and slowly decreasing that means that the data is not stationary and indicates trend.
acf(dj)
# Box test gives us a p-value less than 2.2e-16, which means that p-value was very small and close to zero. This indicates that the data is distinguishable from white noise and hence isn't white noise.
Box.test(dj, lag=10, fitdf=0, type="Lj")

# Since the data is non-stationary we use ndiffs() to find the order of differencing. It gives us a value of 1, indicating that lag = 1
ns <- ndiffs(dj) # ns --> 1

# we take differencing with lag = 1
diff_dj <- diff(dj, differences = ns)

# plot the new data
plot(diff_dj)

# auto-correlation plot of the new data gives us different results will all values within the critical values. And ACF quickly drops to zero. 
# Which indicates that the data is stationary now. 
acf(diff_dj)

# Box test on the new data gives us a p-value of 0.153 > 0.05 and therefore a large p-value indicates that the data is indistinguishable from white noise and hece can be considered as just white noise.
# New data is stationary and white noise. 
Box.test(diff_dj, lag=10, fitdf = 0, type = "Lj")

