library(fpp2)

# Exercise 1

data("gold", "woolyrnq", "gas")

# Experimental plots & ACF's
autoplot(gold)
autoplot(woolyrnq)
autoplot(gas)

# Check frequency
frequency(gold)
frequency(woolyrnq)
frequency(gas)

# Find outlier
outlier <- which.max(gold)
print(outlier)

# Exercise 2

tute1 <- read.csv("tute1.csv", header=TRUE)
View(tute1)

# Conver data to ts object
mytimeseries <- ts(tute1[,-1], start=1981, frequency=4)

# Check what facets does
autoplot(mytimeseries, facets=TRUE)
autoplot(mytimeseries, facets=FALSE)

# Exercise 3

retaildata <- readxl::read_excel("retail.xlsx", skip=1)
View(retaildata)
myts <- ts(retaildata[,"A3349399C"], frequency=12, start=c(1982,4))

# autoplot
autoplot(myts)
ggseasonplot(myts)
ggsubseriesplot(myts)
gglagplot(myts)
ggAcf(myts)

# Exercise 4
data("bicoal", "chicken", "dole", "usdeaths", 
     "lynx", "goog", "writing", "fancy", "a10", "h02")
help("bicoal")
help("goog")
frequency(goog)
View(goog)
autoplot(goog) +
  ggtitle("Google Stock Price") +
  xlab("Day") +
  ylab("Stock Price")

# Exercise 5

# writing dataset - check august
ggseasonplot(writing)
ggsubseriesplot(writing)

# fancy dataset
help(fancy)
ggseasonplot(fancy)
ggsubseriesplot(fancy)

# a10
help(a10)
ggseasonplot(a10)
ggsubseriesplot(a10)

# h02
help(h02)
ggseasonplot(h02)
ggsubseriesplot(h02)

# Exercise 6

help(hsales)
ggseasonplot(hsales)
ggsubseriesplot(hsales)
ggsubseriesplot(hsales)
gglagplot(hsales)
ggAcf(hsales)


