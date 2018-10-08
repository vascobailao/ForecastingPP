library(fpp2)

# Simple Forecasting

beer2 <- window(ausbeer, start=1992)
fit.beer <- tslm(beer2 ~ trend + season)
fcast <- forecast(fit.beer)
autoplot(fcast) +
  ggtitle("Forecasts of beer production using regression") +
  xlab("Year") + ylab("megalitres")

# Scenario-based forecasting: the forecaster assumes possible scenarios
# for the predictor variables that are of interest.

fit.consBest <- tslm(
  Consumption ~ Income + Savings + Unemployment,
  data = uschange)

h <- 4
newdata <- data.frame(
  Income = c(1, 1, 1, 1),
  Savings = c(0.5, 0.5, 0.5, 0.5),
  Unemployment = c(0, 0, 0, 0))
fcast.up <- forecast(fit.consBest, newdata = newdata)
newdata <- data.frame(
  Income = rep(-1, h),
  Savings = rep(-0.5, h),
  Unemployment = rep(0, h))
fcast.down <- forecast(fit.consBest, newdata = newdata)

autoplot(uschange[, 1]) +
  ylab("% change in US consumption") +
  autolayer(fcast.up, PI = TRUE, series = "increase") +
  autolayer(fcast.down, PI = TRUE, series = "decrease") +
  guides(colour = guide_legend(title = "Scenario"))

fit.cons <- tslm(Consumption ~ Income, data = uschange)
h <- 4
fcast.ave <- forecast(fit.cons,
                      newdata = data.frame(
                        Income = rep(mean(uschange[,"Income"]), h)))
fcast.up <- forecast(fit.cons,
                     newdata = data.frame(Income = rep(5, h)))
autoplot(uschange[, "Consumption"]) +
  ylab("% change in US consumption") +
  autolayer(fcast.ave, series = "Average increase",
            PI = TRUE) +
  autolayer(fcast.up, series = "Extreme increase",
            PI = TRUE) +
  guides(colour = guide_legend(title = "Scenario"))

# Non-linear regression

h <- 10
fit.lin <- tslm(marathon ~ trend)
fcasts.lin <- forecast(fit.lin, h = h)
fit.exp <- tslm(marathon ~ trend, lambda = 0)
fcasts.exp <- forecast(fit.exp, h = h)

t <- time(marathon)
t.break1 <- 1940
t.break2 <- 1980
tb1 <- ts(pmax(0, t - t.break1), start = 1897)
tb2 <- ts(pmax(0, t - t.break2), start = 1897)

fit.pw <- tslm(marathon ~ t + tb1 + tb2)
t.new <- t[length(t)] + seq(h)
tb1.new <- tb1[length(tb1)] + seq(h)
tb2.new <- tb2[length(tb2)] + seq(h)

newdata <- cbind(t=t.new, tb1=tb1.new, tb2=tb2.new) %>%
  as.data.frame()
fcasts.pw <- forecast(fit.pw, newdata = newdata)

fit.spline <- tslm(marathon ~ t + I(t^2) + I(t^3) +
                     I(tb1^3) + I(tb2^3))
fcasts.spl <- forecast(fit.spline, newdata = newdata)

autoplot(marathon) +
  autolayer(fitted(fit.lin), series = "Linear") +
  autolayer(fitted(fit.exp), series = "Exponential") +
  autolayer(fitted(fit.pw), series = "Piecewise") +
  autolayer(fitted(fit.spline), series = "Cubic Spline") +
  autolayer(fcasts.pw, series="Piecewise") +
  autolayer(fcasts.lin, series="Linear", PI=FALSE) +
  autolayer(fcasts.exp, series="Exponential", PI=FALSE) +
  autolayer(fcasts.spl, series="Cubic Spline", PI=FALSE) +
  xlab("Year") + ylab("Winning times in minutes") +
  ggtitle("Boston Marathon") +
  guides(colour = guide_legend(title = " "))

marathon %>%
  splinef(lambda=0) %>%
  checkresiduals()


# Exercises

# Exercise 1

daily20 <- head(elecdaily, 20)

autoplot(daily20, facets=TRUE)
daily20 %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)
fit <- tslm(Demand ~ Temperature, data=daily20)
checkresiduals(fit)
forecast(fit, newdata=data.frame(Temperature=c(15,35)))

elecdaily %>%
  as.data.frame() %>%
  ggplot(aes(x=Temperature, y=Demand)) +
  ylab("Electricity Demand") +
  xlab("Temperature") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)

# Exercise 2
data("mens400")
autoplot(mens400)

time_mens400 <- time(mens400)
tslm_mens400 <- tslm(mens400 ~ time_mens400, 
                     data = mens400)

mens400 %>%
  as.data.frame() %>%
  ggplot(aes(x=time_mens400, y=mens400)) +
  geom_point()

# Show data with regression line
autoplot(mens400) +
  geom_abline(slope = tslm_mens400$coefficients[2],
              intercept = tslm_mens400$coefficients[1],
              colour = "red")
  
# Show data (scatter) with regression line
autoplot(mens400) +
  geom_point() +
  geom_abline(slope = tslm_mens400$coefficients[2],
            intercept = tslm_mens400$coefficients[1],
            colour = "red")

# Average decrease in times
# Get the winning time decreasing rate
tslm_mens400$coefficients[2]
# The winning times have been decreasing at average rate of 0.06457 second per year.

# c. Plot the residuals against the year. What does this indicate about the suitability of the fitted line?
cbind(Time = time_mens400, 
      Residuals = tslm_mens400$residuals) %>%
  as.data.frame() %>%
  ggplot(aes(x = Time, y = Residuals)) +
  geom_point() +
  ylab("Residuals of Regression Line(Unit:s)")
# The residual plot shows that the regression model generally fitted the data well. I can check it using checkresiduals function, too.
checkresiduals(tslm_mens400)
  
# d. Predict the winning time for the men's 400 meters final in the 2020 Olympics. 
# Give a prediction interval for your forecasts. 
# What assumptions have you made in these calculations?
# I made linear model with na.action argument as na.exclude to exclude missing values.
# And then I used the linear model in forecast function to get prediction interval.
# Forecast function can't calculate prediction interval when there is any missing values 
# in the data that I excluded them fitting linear model.

# FIRST; fit a linear model of the data
lm_mens400 <- lm(
  mens400 ~ time_mens400, 
  data = mens400,
  na.action = na.exclude
)
# SECOND: make a forecast for time_mens400 = 2020.
fc_mens400 <- forecast(
  lm_mens400, 
  newdata = data.frame(time_mens400 = 2020)
)

autoplot(mens400) +
  autolayer(fc_mens400, PI = TRUE)

# Get 80% and 95% prediction intervals
fc_mens400$upper
fc_mens400$lower

# Exercise 3

beer_data <- easter(ausbeer)
autoplot(beer_data)
time(beer_data[0])
time(beer_data[length(time[beer_data])-1])

# Exercise 5

# a. We can see that there's a trend (sales increase with years)
# We can see that the volume of sales from 92 forward increases in an exponencial way
autoplot(fancy)

# b. Explain why it is necessary to take logarithms of these data before fitting a model.
# The size of the seasonal variations should be almost same across the whole series to be fitted well to a model. Fancy data shows that seasonal variations increased exponentially. Therefore it is necessary to take logarithms of the data.

# c.
Time <- time(fancy)
surfing_festival <- c()
for(i in 1:length(Time)){
  month <- round(12*(Time[i] - floor(Time[i]))) + 1
  year <- floor(Time[i])
  if(year >= 1988 & month == 3){
    surfing_festival[i] <- 1
  } else {
    surfing_festival[i] <- 0
  }
}

tslm_log_fancy <- tslm(
  BoxCox(fancy, 0) ~ trend + season + surfing_festival
)

# d.
autoplot(tslm_log_fancy$residuals)
# The residuals have pattern against time. 
# It means that there is correlation between residuals and time.

cbind(Residuals = tslm_log_fancy$residuals,
      Fitted_values = tslm_log_fancy$fitted.values) %>%
  as.data.frame() %>%
  ggplot(aes(x = Fitted_values,
             y = Residuals)) +
  geom_point()

# e.

cbind.data.frame(
  Month = factor(
    month.abb[round(12*(Time - floor(Time)) + 1)],
    labels = month.abb,
    ordered = TRUE
  ),
  Residuals = tslm_log_fancy$residuals
) %>%
  ggplot(aes(x = Month,
             y = Residuals)) +
  geom_boxplot()

checkresiduals(tslm_log_fancy$residuals)

