# Simple Forecasting: naive, mean, seasonal-naive, drift

# Set training data from 1992 to 2007
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") +
  guides(colour=guide_legend(title="Forecast"))

autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") +
  guides(colour=guide_legend(title="Forecast"))

# Transformations and Adjustments

# Calendar adjustments

dframe <- cbind(Monthly = milk,
                DailyAverage = milk/monthdays(milk))
autoplot(dframe, facet=TRUE) +
  xlab("Years") + ylab("Pounds") +
  ggtitle("Milk production per cow")

# By looking at the average daily production instead of the average monthly production, 
# we effectively remove the variation due to the different month lengths. 
# Simpler patterns are usually easier to model and lead to more accurate forecasts.

# Box-Cox transformations

(lambda <- BoxCox.lambda(elec))
autoplot(BoxCox(elec,lambda))

# lambda = -1. is a reciprocal transform.
# lambda = -0.5 is a reciprocal square root transform.
# lambda = 0.0 is a log transform.
# lambda = 0.5 is a square root transform.
# lambda = 1.0 is no transform.

# Bias adjustment

fc <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80)
fc2 <- rwf(eggs, drift=TRUE, lambda=0, h=50, level=80,
           biasadj=TRUE)
autoplot(eggs) +
  autolayer(fc, series="Simple back transformation") +
  autolayer(fc2, series="Bias adjusted", PI=FALSE) +
  guides(colour=guide_legend(title="Forecast"))

# Residuals diagnostic

# A good forecasting method will yield residuals with the following properties:
#   
# 1.The residuals are uncorrelated. 
# If there are correlations between residuals, then there is information left 
# in the residuals which should be used in computing forecasts.
# 2.The residuals have zero mean. If the residuals have a mean other than zero, 
# then the forecasts are biased.
# 3.The residuals have constant variance.
# 4. he residuals are normally distributed.

# Example: Google stock price forecast using naive algorithm

autoplot(goog200) +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google Stock (daily ending 6 December 2013)")

res <- residuals(naive(goog200))
autoplot(res) + xlab("Day") + ylab("") +
  ggtitle("Residuals from naïve method")

gghistogram(res) + ggtitle("Histogram of residuals")

ggAcf(res) + ggtitle("ACF of residuals")

checkresiduals(naive(goog200))

# Evaluating forecast accuracy

# extracts all data from 1995 onward.
window(ausbeer, start=1995)

# extracts the last 5 years of observations from ausbeer
subset(ausbeer, start=length(ausbeer)-4*5)

# extracts the first quarters for all years.
subset(ausbeer, quarter = 1)

# last 5 years of ausbeer
tail(ausbeer, 4*5)

# Types of errors:

# 1. Mean Absolute Error (MAE): vertical distance between the prediction and the ground truth
# over the total number of samples

# Example 1
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))

beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

# Example 2
googfc1 <- meanf(goog200, h=40)
googfc2 <- rwf(goog200, h=40)
googfc3 <- rwf(goog200, drift=TRUE, h=40)
autoplot(subset(goog, end = 240)) +
  autolayer(googfc1, PI=FALSE, series="Mean") +
  autolayer(googfc2, PI=FALSE, series="Naïve") +
  autolayer(googfc3, PI=FALSE, series="Drift") +
  xlab("Day") + ylab("Closing Price (US$)") +
  ggtitle("Google stock price (daily ending 6 Dec 13)") +
  guides(colour=guide_legend(title="Forecast"))

googtest <- window(goog, start=201, end=240)
accuracy(googfc1, googtest)
accuracy(googfc2, googtest)
accuracy(googfc3, googtest)

# Predictions Intervals

naive(goog200)
autoplot(naive(goog200))

# with bootstapping
naive(goog200, bootstrap=TRUE)
autoplot(naive(goog200))

### EXERCISES ###

# Exercise 1 -> data: usnetelec, usgdp, mcopper, enplanements

autoplot(usnetelec)
(lambda <- BoxCox.lambda(usnetelec))
autoplot(BoxCox(usnetelec,lambda))

autoplot(usgdp)
(lambda <- BoxCox.lambda(usgdp))
autoplot(BoxCox(usgdp,lambda))

autoplot(mcopper)
(lambda <- BoxCox.lambda(mcopper))
autoplot(BoxCox(mcopper,lambda))

autoplot(enplanements)
(lambda <- BoxCox.lambda(enplanements))
autoplot(BoxCox(enplanements,lambda))

# Exercise 2

autoplot(cangas)
(lambda <- BoxCox.lambda(cangas))
autoplot(BoxCox(cangas,lambda))
# can see that Box-Cox transformation doesn't yield simpler model

# Exercise 3

library(xlsx)
retaildata <-readxl::read_excel("retail.xlsx", skip=1)
myts <- ts(retaildata[,"A3349873A"], frequency=12, start=c(1982,4))

lambda_retail <- BoxCox.lambda(myts)
print(c("selected lambda:", lambda_retail))

fc_retail <- rwf(myts, 
                 drift = TRUE, 
                 lambda = lambda_retail,
                 h = 50,
                 level = 80)

fc_retail_biasadj <- rwf(myts, 
                         drift = TRUE, 
                         lambda = lambda_retail,
                         h = 50,
                         level = 80,
                         biasadj = TRUE)

autoplot(myts) +
  autolayer(fc_retail, series = "Drift method with Box-Cox Transformation") +
  autolayer(fc_retail_biasadj$mean, series = "Bias Adjusted") +
  guides(colour = guide_legend(title = "Forecast"))

# It would be better to choose bias adjusted Box-Cox Transformation with lambda = 0.128

# Exercise 4

autoplot(dole)
(lambda <- BoxCox.lambda(dole))
autoplot(BoxCox(dole,lambda))

autoplot(usdeaths)
(lambda <- BoxCox.lambda(usdeaths))
autoplot(BoxCox(usdeaths,lambda))

autoplot(bricksq)
(lambda <- BoxCox.lambda(bricksq))
autoplot(BoxCox(bricksq,lambda))

# Exercise 5

beer <- window(ausbeer, start=1992)
fc <- snaive(beer)
autoplot(fc)
res <- residuals(fc)
autoplot(res)
checkresiduals(fc)

# Exercise 6

autoplot(WWWusage)
fc <- naive(WWWusage)
autoplot(fc)
res <- residuals(fc)
autoplot(res)
checkresiduals(fc)

autoplot(bricksq)
fc <- snaive(bricksq)
autoplot(fc)
res <- residuals(fc)
autoplot(res)
checkresiduals(fc)

# Exercise 7

# 1 - Not necessarily. 
# 2 - No, they should be uncorrelated.
# 3 - Not necessarily
# 4 - No, I should make it simpler.
# 5 - Not necessarily. 

# Exercise 8

# a. Split the data into two parts using

myts.train <- window(myts, end=c(2010,12))
myts.test <- window(myts, start=2011)

# b. Check that your data have been split appropriately by producing the following plot.

autoplot(myts) + 
  autolayer(myts.train, series="Training") +
  autolayer(myts.test, series="Test")

# c. Calculate forecasts using snaive applied to myts.train.

snaive_myts_train <- snaive(myts.train)

# d. Compare the accuracy of your forecasts against the actual values stored in myts.test.

accuracy(snaive_myts_train, myts.test)

# e. Check the residuals. Do the residuals appear to be uncorrelated and normally distributed?

checkresiduals(snaive_myts_train)
# residuals were correlated with each other and not normally distributed

# f. How sensitive are the accuracy measures to the training/test split?

# I thought of sensitivity as the ratio of the test set error to the train set error. When I used the definition, 
# it looked like Mean Error is highly sensitive, RMSE, MAE, MPE, MASE are sensitive and MAPE and ACF1 aren't much sensitive.
# I don't know whether this method is what the author wanted to solve this question about sensitivity.

# Exercise 9

vn <- visnights
# a. Use window() to create three training sets for vn[,"Melbourne"], omitting the last 1, 2 and 3 years; call these train1, train2, and train3, respectively. For example train1 <- window(vn[, "Melbourne"], end = c(2014, 4)).
vn_Melbourne_train1 <- window(vn[, "QLDMetro"], end = c(2014, 4))
vn_Melbourne_train2 <- window(vn[, "QLDMetro"], end = c(2013, 4))
vn_Melbourne_train3 <- window(vn[, "QLDMetro"], end = c(2012, 4))

# b. Compute one year of forecasts for each training set using the snaive() method. 
snaive_vn_Melbourne_train1 <- snaive(vn_Melbourne_train1, h = 4)
snaive_vn_Melbourne_train2 <- snaive(vn_Melbourne_train2, h = 4)
snaive_vn_Melbourne_train3 <- snaive(vn_Melbourne_train3, h = 4)

# c. Use accuracy() to compare the MAPE over the three test sets. Comment on these.
vn_Melbourne_test1 <- window(vn[, "QLDMetro"], start = c(2015, 1), end = c(2015, 4))
vn_Melbourne_test2 <- window(vn[, "QLDMetro"], start = c(2014, 1), end = c(2014, 4))
vn_Melbourne_test3 <- window(vn[, "QLDMetro"], start = c(2013, 1), end = c(2013, 4))

accuracy(snaive_vn_Melbourne_train1, vn_Melbourne_test1)
writeLines("")
accuracy(snaive_vn_Melbourne_train2, vn_Melbourne_test2)
writeLines("")
accuracy(snaive_vn_Melbourne_train3, vn_Melbourne_test3)

# MAPE was smallest for 2015 prediction and biggest for 2014 prediction. 
# MAPE became smaller in 2013 prediction, but it wan't smaller than the one for 2015.

# Exercise 10

# a. Produce a time plot of the series.
autoplot(dowjones)

# b. Produce forecasts using the drift method and plot them.
drift_dj <- rwf(dowjones, drift = TRUE)
autoplot(drift_dj)

# c. Show that the forecasts are identical to extending the line drawn between the 
# first and last observations.
dj_x <- c(1, 78)
dj_y <- c(dowjones[1], dowjones[78])
lm_dj <- lm(dj_y ~ dj_x)

# It looked like ggplot2 package isn't compatible to graphics package that autoplot function, 
# made of ggplot2 can't be worked with graphics function like abline, etc. 
# When I tried it, 'plot.new has not been called yet' error appeared.

autoplot(drift_dj) +
  geom_abline(intercept = lm_dj$coefficients[1],
              slope = lm_dj$coefficients[2],
              colour = "red")

autoplot(drift_dj) +
  geom_line(aes(x = c(1, 78),
                y = dowjones[c(1, 78)]), 
            colour = "red")

# d. Try using some of the other benchmark functions to forecast the same data set. Which do you think is best? Why?
checkresiduals(drift_dj)

mean_dj <- meanf(dowjones)
autoplot(mean_dj)

naive_dj <- naive(dowjones)
autoplot(naive_dj)
checkresiduals(naive_dj)

snaive_dj <- snaive(dowjones, h = 10)
autoplot(snaive_dj)

# Exercise 11

# a. Produce some plots of the data in order to become familiar with it.
autoplot(ibmclose)

# b. Split the data into a training set of 300 observations and a test set of 69 observations.
ibm_train <- subset(ibmclose, end = 300)
ibm_test <- subset(ibmclose, start = 301)

# c. Try using various benchmark methods to forecast the training set and compare the results on the test set. Which method did best?
snaive_ibm <- snaive(ibm_train, h = 69)
naive_ibm <- naive(ibm_train, h = 69)
drift_ibm <- rwf(ibm_train, drift = TRUE, h = 69)
mean_ibm <- meanf(ibm_train, h = 69)

autoplot(snaive_ibm) +
  autolayer(ibm_test)
autoplot(naive_ibm) +
  autolayer(ibm_test)
autoplot(drift_ibm) +
  autolayer(ibm_test)
autoplot(mean_ibm) +
  autolayer(ibm_test)

writeLines("Snaive method")
accuracy(snaive_ibm, ibm_test)

writeLines("\nNaive method")
accuracy(naive_ibm, ibm_test)

writeLines("\nDrift method")
accuracy(drift_ibm, ibm_test)

writeLines("\nMean method")
accuracy(mean_ibm, ibm_test)

e_snaive_ibm <- ibm_test - snaive_ibm$mean
e_naive_ibm <- ibm_test - naive_ibm$mean
e_drift_ibm <- ibm_test - drift_ibm$mean
e_mean_ibm <- ibm_test - mean_ibm$mean

autoplot(e_snaive_ibm^2, series = "snaive method") +
  autolayer(e_naive_ibm^2, series = "naive method") +
  autolayer(e_drift_ibm^2, series = "drift method") +
  autolayer(e_mean_ibm^2, series = "mean method") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("Errors of the forecast of closing IBM stock price") +
  ylab(expression(paste("erro", r^{2})))
# Drift method did best

# Time series cross-validation method of tsCV function don't use full data unless h = 1. 
# For example, if usable data has 100 points and h = 3, tsCV predicts 101st point with 
# 98 points, 102nd with 99 points and 103rd with 100 points. 
# Therefore error result value of tsCV cannot help differing from the values of accuracy function. 
# Accuracy function always get result from full data.
ibmclose %>% tsCV(forecastfunction = snaive, h = 69) ->  e_snaive_ibm_CV
ibmclose %>% tsCV(forecastfunction = naive, h = 69) ->  e_naive_ibm_CV
ibmclose %>% tsCV(forecastfunction = rwf, drift = TRUE, h = 69) ->  e_drift_ibm_CV
ibmclose %>% tsCV(forecastfunction = meanf, h = 69) ->  e_mean_ibm_CV

autoplot(subset(e_snaive_ibm_CV^2, start = 301), series = "snaive method") +
  autolayer(subset(e_naive_ibm_CV^2, start = 301), series = "naive method") +
  autolayer(subset(e_drift_ibm_CV^2, start = 301), series = "drift method") +
  autolayer(subset(e_mean_ibm_CV^2, start = 301), series = "mean method") +
  guides(colour = guide_legend(title = "Forecast")) +
  ggtitle("Errors of the forecast of closing IBM stock price",
          subtitle = "after using tsCV function") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5)) +
  ylab(expression(paste("erro", r^{2})))
# Based on the returned results of tsCV function, I would've selected naive method because it yielded smallest error.

# d. Check the residuals of your preferred method. Do they resemble white noise?
checkresiduals(naive_ibm)
checkresiduals(drift_ibm)

# No. Even when I checked the residuals of naive method because the error values 
# were similar to result of the drift method, they weren't like white noise, too.

# Exercise 12

# a. Produce some plots of the data in order to become familiar with it.
autoplot(hsales)

# b. Split the hsales data set into a training set and a test set, where the test set is the last two years of data.
hsales_train <- subset(hsales, end = length(hsales) - 24)
hsales_test <- subset(hsales, start = length(hsales) - 23)

# c. Try using various benchmark methods to forecast the training set and compare the results on the test set.  Which method did best?
snaive_hsales <- snaive(hsales_train, h = 24)
naive_hsales <- naive(hsales_train, h = 24)
drift_hsales <- rwf(hsales_train, drift = TRUE, h = 24)
mean_hsales <- meanf(hsales_train, h = 24)

autoplot(snaive_hsales) +
  autolayer(hsales_test)
autoplot(naive_hsales) +
  autolayer(hsales_test)
autoplot(drift_hsales) +
  autolayer(hsales_test)
autoplot(mean_hsales) +
  autolayer(hsales_test)

writeLines("Snaive method")
accuracy(snaive_hsales, hsales_test)

writeLines("\nNaive method")
accuracy(naive_hsales, hsales_test)

writeLines("\nDrift method")
accuracy(drift_hsales, hsales_test)

writeLines("\nMean method")
accuracy(mean_hsales, hsales_test)

# Seasonal naive method did the best.

# d. Check the residuals of your preferred method. Do they resemble white noise?
checkresiduals(snaive_hsales) 
# But the residuals don't resemble white noise