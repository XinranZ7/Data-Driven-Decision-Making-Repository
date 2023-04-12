# Get the dataset "AirPassengers" from the library. This is a time series dataset
data(AirPassengers)
class(AirPassengers)
# This dataset consists of monthly totals of international airline passengers from 1949 to 1960.
  head(AirPassengers)
# Let us try to understand the data - Let us see for what year and month we have the data
  start(AirPassengers)
  end(AirPassengers)

  # To determine the frequency
  frequency(AirPassengers)

  # The dataset contains 12 entries for each year (12 years) which is (12 * 12 = 144 datapoints)
  summary(AirPassengers)
  cycle(AirPassengers)

  # Let us plot the time series and fit a simple regression line
  plot(AirPassengers)
  abline(reg=lm(AirPassengers~time(AirPassengers)))
  
    # Aggregate the cycles and display a year on year trend
    plot(aggregate(AirPassengers,FUN=mean))
  
    # Let us now see the boxplot to determine if there are any seasonal trends
    boxplot(AirPassengers~cycle(AirPassengers))

    
    #To Convert the time series in to a data frame
    AP<-ts(AirPassengers,frequency = 12, start=c(1949,1))
    data.frame(AP = c(AP), time = c(time(AP)))
    # To convert the data frame into matrix
    AP1 <- as.matrix(AP)
    # Write the time series data in to a CSV file
    ts2csv <- function(x) {
      fname <- paste0(deparse(substitute(x)), ".csv")
      if (NCOL(x) == 1L) {
        # Univariate time series
        readr::write_csv(
          as.data.frame(tsibble::as_tsibble(x)),
          fname)
      } else {
        # Multivariate time series
        readr::write_csv(
          as.data.frame(tsibble::spread(tsibble::as_tsibble(x), key, value)),
          fname)
      }
    }
install.packages("fpp2","tsibble")
library(fpp2)
library(tsibble)
ts2csv(AP) # Univariate monthly data
ts2csv(AP) # Multivariate quarterly data
ts2csv(elecdemand) # Multivariate half-hourly data

# Let us first install and load the package 'tseries'
install.packages("tseries")
library(tseries)
# Since we want to remove the unequal variances in the data take the log of the series
# Also, since the original signal had trends and seasonal effects, so it was not stationary.
# Therefore, we perform a diff of the signal and check if the trends and seasonal effect can be
# removed. But first take the log of the original signal
# Perform Augmented Dickey-Fuller Test
adf.test(diff(log(AirPassengers)), alternative="stationary", k=0)
plot(diff(log(AirPassengers)))
# Let us now determine the value of p and q by plotting the ACF and PACF curves
acf(diff(log(AirPassengers)))
# Let us now determine the value of p and q by plotting the ACF and PACF curves
pacf(diff(log(AirPassengers)))

#Clearly, PACF plot cuts off after the first lag. Hence, p should be 1 i.e. it is AR(1) model and
#we know that d=1. Also, q should be 0 (ACF shows geometric decay). Therefore, we need to
#perform ARMA (1,1,0)
  # Fit ARIMA(1, 1, 0)
  (fit <- arima(log(AirPassengers), c(1, 1, 0),seasonal = list(order = c(1, 1, 0), period = 12)))
  # Let us perform prediction for the next 10 years i.e. from 1960 to 1970 and plot them
  pred <- predict(fit, n.ahead = 10*12)
pred # get the values for each month for the next 10 years
  ts.plot(AirPassengers,2.718^pred$pred, log = "y", lty = c(1,3))

  
  
  
  
