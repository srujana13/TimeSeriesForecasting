require(fpp)
require(fma)
data(package = "fma")
data(package="fpp")

# Dataset -> usnetelec
# Data clearly shows upward trend and hence is not stationary. Data does not have any seasonality.
plot(usnetelec)

# acf of data is slowly decreasing therefore, data is not stationary and indicates trend.
acf(usnetelec)

# transforming data using differencing by lag of ns
ns <- ndiffs(usnetelec) # ns --> 1

# Differencing using lag = 1
diff_usnetelec <- diff(usnetelec, differences = ns)

# acf of transformed data is within critical values and hence indicates stationary data. 
acf(diff_usnetelec)


########################################################################################
# Dataset -> usgdp
# Data shows upward trend and is not stationary. 
plot(usgdp)

# acf of data is slowly decreasing therefore, data is non-stationary. 
acf(usgdp)

# transforming data using differencing by lag of ns_1 
ns_1 <- ndiffs(usgdp) #ns_1 --> 2

# Differencing using lag of ns_1 = 2
diff_usgdp <- diff(usgdp, differences = ns_1)

# checking acf to check if data is stationary
acf(diff_usgdp)
# Since acf from lag 1 onwards is below the critical values, the data is stationary now. 

########################################################################################
# Dataset -> mcopper
# Data shows some trend and a spike towards the end. 
plot(mcopper)

# Data shows a seasonality and a trend component. 
fit <- stl(mcopper, s.window = "periodic")

# acf is decreasing but it is non-uniform 
acf(mcopper)

# applying box-cox transformation to strabilize the variance
lambda <- BoxCox.lambda(mcopper)
new_mcopper <- BoxCox(mcopper,lambda)

# acf is now slowly decreasing, data is non-stationary. Now we can use differencing to stabilize mean. 
acf(new_mcopper)

# finding the number of lags to apply differencing 
ns_3 <- ndiffs(new_mcopper) #ns_3 --> 1
print(ns_3)

# Applying differencing
diff_mcopper <- diff(new_mcopper,differences = ns_3)

# All acf values from 0.5 lag onwards are within the critical values. Therefore, data is stationary. 
acf(diff_mcopper)

########################################################################################
# Dataset -> enplanements
# Data appears to have both trend and seasonality component. 
plot(enplanements)

# Data shows a seasonality and a trend component. 
fit <- stl(enplanements, s.window = "periodic")
plot(fit)

# Removing seasonality using seasadj
seasadj_enplanements <- seasadj(fit)

# acf plot is non uniform, therefore we need to stabilize variance. 
acf(seasadj_enplanements)

# Applying BoxCox transformation
# lambda <- BoxCox.lambda(seasadj_enplanements)
# new_enplanements <- BoxCox(seasadj_enplanements,lambda)
# plot(new_enplanements)
# acf(new_enplanements)

# Box cox transformation did not show any significant improvement in acf values. Using differencing to stabilize mean below. 
ns <- ndiffs(seasadj_enplanements) # ns --> 1

# new data after differencing 
diff_enplanements <- diff(seasadj_enplanements, differences = ns)

# Data is now stationary since all the values dropped to zero and are mostly within the critical values.
acf(diff_enplanements)

# Since the data is not critical, we can test this using kpss test. 
# p-value > 0.05, therefore null hypothesis is not rejected. Data is stationary. 
kpss.test(diff_enplanements)

########################################################################################
# Dataset -> visitors
# Data has both trend and seasonality.
plot(visitors)

# Decomposing data as follows
fit <- stl(visitors, s.window = "periodic")
plot(fit)

# Acf values are not uniform, hence we need to remove seasonality. 
acf(visitors)

# Removing seasonality as follows:
seasadj_visitors <- seasadj(fit)

# Acf shows more uniformly decreasing trend now. 
acf(seasadj_visitors)

# We apply differencing to stabilize mean. 
ns <- ndiffs(seasadj_visitors)

diff_visitors <- diff(seasadj_visitors, differences = ns)

# acf shows a spike at every 1 lag, but values are still close to zero. 
acf(diff_visitors, lag.max = 50)

# p-value is greater than 0.05. Therefore, we do not reject the null hypothesis and confirm that the data is now stationary. 
kpss.test(diff_visitors)


