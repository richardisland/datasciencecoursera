today <-Sys.Date()
rnorm
require(quantmod)

# Download data
spy = getSymbols("SPY",from="1900-01-01",auto.assign=F)
https://www.coursera.org/learn/data-scientists-tools/exam/hDNPL/week-1-quiz
# Confirm the SMA situation first
spy.sma = runMean(Cl(spy), 200)
mm = merge(Cl(spy), spy.sma, all=F)
ind = mm[,1] >= mm[,2]
print(tail(ind,20))

# 2015-08-18      TRUE
# 2015-08-19      TRUE
# 2015-08-20     FALSE
# 2015-08-21     FALSE
# Yep, sell on the Thu's close

# Compute the returns
rets = ROC(Cl(spy),type="discrete")

# Compute the adjusted returns
adj.rets = rets/sqrt(runSum(rets*rets,10)/9)

# The moving average
sma = runMean(adj.rets,n=200)

ind = sma >= 0
print(tail(ind,20))

# 2015-08-18  TRUE
# 2015-08-19  TRUE
# 2015-08-20 FALSE
# 2015-08-21 FALSE
# Yep, sell on the Thu's close

# The standard deviation
stddev = sqrt(runSum(adj.rets*adj.rets,200)/199)
lower.band = -0.05*stddev

ind = sma < lower.band
print(tail(ind,4))

# Yahoo finance url for S&P 500 data
str <- sprintf("%s?s=^GSPC&d=7&e=4&f=2011&g=d&a=0&b=3&c=1950",
               "http://ichart.finance.yahoo.com/table.csv")
df <- tryCatch(read.csv(url(str)), error = function(e) NA)

names(df) <- tolower(names(df))
df$date <- as.Date(df$date)
df <- df[order(df$date), ]

# Plot prices, daily log returns & running mean of log returns since 1990
start.date <- "1990-01-01"
dev.new(width=12, height=6)
plot(subset(df, date >= start.date)[ , c("date", "close")], type="l",
     main="S&P 500", xlab="", col="tomato")
mtext(sprintf("Closing prices since %s", start.date))
mtext(sprintf("Created on %s", Sys.Date()), side=1, line=3, cex=0.60)
savePlot("s&p_500_prices.png")
# Logarithmic returns: p0 * e^r = p1 ; r = ln(p1) - ln(p0)
df$return <- c(diff(log(df$close)), NA)
dev.new(width=12, height=6)
plot(subset(df, date >= start.date)[ , c("date", "return")], type="p",
     main="S&P 500", xlab="", col=rgb(0, 75, 0, 75, maxColorValue=255))
mtext(sprintf("Logarithmic close-to-close returns since %s", start.date))
mtext(sprintf("Created on %s", Sys.Date()), side=1, line=3, cex=0.6)
savePlot("s&p_500_daily_returns.png")
# Running average of logarithmic returns
lag <- 200
df$mean.return <- c(diff(c(0, cumsum(df$return)), lag=lag),
                    rep(NA, (lag - 1))) * (1 / lag)
dev.new(width=12, height=6)
plot(subset(df, date >= start.date)[ , c("date", "mean.return")],
     type="l", main="S&P 500", xlab="", col="darkred",
     ylab=sprintf("%s-day mean return", lag))
mtext(sprintf("%s-day mean of logarithmic returns since %s",
              lag, start.date))
mtext(sprintf("Created on %s", Sys.Date()), side=1, line=3, cex=0.6)
abline(0, 0, lty=2)
savePlot(sprintf("s&p_500_%s_day_mean_returns.png", lag))

install.packages("quantmod") #Install the quantmod library
library("quantmod")  #Load the quantmod Library
stockData <- new.env() #Make a new environment for quantmod to store data in

startDate = as.Date("2008-01-13") #Specify period of time we are interested in
endDate = as.Date("2018-01-31")

tickers <- c("GLD","SPY") #Define the tickers we are interested in

#Download the stock history (for all tickers)
getSymbols(tickers, env = stockData, src = "yahoo", from = startDate, to = endDate)

#Use head to show first six rows of matrix
head(stockData$SPY)

#Lets look at the just the closing prices
Cl(stockData$SPY)

#Lets plot the data
chartSeries(stockData$SPY)

#Lets add some bollinger bands to the plot (with period 50 & width 2 standard deviations)
?addBBands #Make R display the help documentation so we know what variables to pass to the function
addBBands(n=50, sd=2)

#Lets get the technical indicator values saved into a variable
#Note must give it a single time series (I gave it the close price in this example)
indicatorValuesBBands <- BBands(Cl(stockData$SPY),n=50, sd=2)

#Lets examine only a 1 month period of data
armSubset<-  window(stockData$SPY, start = as.Date("2010-02-15"), end = as.Date("2010-03-15"))
armSubset #Lets see the data

#Lets extract a 1 month period of data for CSR but starting midway through the arm data
csrSubset<-  window(stockData$GLD, start = as.Date("2010-02-25"), end = as.Date("2010-03-25"))
csrSubset #Lets see the data

#Now we want to get the intersection of the two subsets of data
#this will gives us all the sets of data where the dates match
#Its important to match the date series to stop spurious analysis of non-synchronised data
#All=FALSE specifies the intersection as in don't include all dates in the merge
armcsrIntersection <- merge(armSubset, csrSubset, all = FALSE)
subset(armcsrIntersection,select = c("SPY.Open","GLD.Open")) #Select the open columns and display

a <- available.packages()
head(3)
a <- available.packages()