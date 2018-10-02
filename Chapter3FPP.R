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


