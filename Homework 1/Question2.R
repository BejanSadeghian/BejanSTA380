library(fImport)
library(foreach)
library(mosaic)

set.seed(1)
#Import the last five years worth of data
Stockdata = yahooSeries(c('SPY','TLT','LQD','EEM','VNQ'), to=Sys.timeDate(), nDaysBack = 1825)

YahooPricesToReturns = function(series) { #YahooPricesToReturns Function Code provided by Dr. James Scott
  mycols = grep('Adj.Close', colnames(series))
  closingprice = series[,mycols]
  N = nrow(closingprice)
  percentreturn = as.data.frame(closingprice[2:N,]) / as.data.frame(closingprice[1:(N-1),]) - 1
  mynames = strsplit(colnames(percentreturn), '.', fixed=TRUE)
  mynames = lapply(mynames, function(x) return(paste0(x[1], ".PctReturn")))
  colnames(percentreturn) = mynames
  as.matrix(na.omit(percentreturn))
}

#Assessing risk of each ETF
myreturns = YahooPricesToReturns(Stockdata)
pairs(myreturns)
print('Column STD')
apply(myreturns,2,sd)


#Assigning weights to each portfolio 
WeightsEven = c(0.2, 0.2, 0.2, 0.2, 0.2) #SPY, TLT, LQD, EEM, VNQ

#for a safer portfolio, Id like to invest in stocks that appear to be uncorrelated or 
#unversely correlated  to one another, to avoid multiplicative losses. Also I want to 
#choose a stock that does not have a high STD of percent change from day to day
WeightsSafe = c(0.33, 0.34, 0.33, 0.0, 0.0) #SPY, TLT, LQD, EEM, VNQ

#for higher risk ETFs Id say investing more of my portfolio on ETFs that seem
#correlated to one another will result in higher risk because when one goes up both
#do, but if one goes down, the other tends to go down as well. 
WeightsAggr = c(0.3, 0.0, 0.0, 0.35, 0.35) #SPY, TLT, LQD, EEM, VNQ

IniWealth = 100000 #invest $100,000 initially
ndays=20

#Begin bootstrap for each portfolio
#Even Split Portfolio
EvenSim = foreach(i=1:5000, .combine='rbind') %do% {
  totwealth = IniWealth
  wealthTracker = rep(0,ndays)
  for(i in 1:ndays){
    currentWealth = totwealth * WeightsEven
    dailyChange = resample(myreturns,1, orig.id=FALSE) #bootstrap sample for the day
    newHoldings = currentWealth + currentWealth*dailyChange
    totwealth = sum(newHoldings)
    wealthTracker[i] = totwealth
  }
  wealthTracker
}
plot(wealthTracker, type='l', main='Even Split Portfolio', xlab='Trading Days', ylab='Wealth')

#Safe Portfolio
SafeSim = foreach(i=1:5000, .combine='rbind') %do% {
  totwealth = IniWealth
  wealthTracker = rep(0,ndays)
  for(i in 1:ndays){
    currentWealth = totwealth * WeightsSafe
    dailyChange = resample(myreturns,1, orig.id=FALSE) #bootstrap sample for the day
    newHoldings = currentWealth + currentWealth*dailyChange
    totwealth = sum(newHoldings)
    wealthTracker[i] = totwealth
  }
  wealthTracker
}
plot(wealthTracker, type='l', main='Safe Portfolio', xlab='Trading Days', ylab='Wealth')


#Aggressive Portfolio
AggSim = foreach(i=1:5000, .combine='rbind') %do% {
  totwealth = IniWealth
  wealthTracker = rep(0,ndays)
  for(i in 1:ndays){
    currentWealth = totwealth * WeightsAggr
    dailyChange = resample(myreturns,1, orig.id=FALSE) #bootstrap sample for the day
    newHoldings = currentWealth + currentWealth*dailyChange
    totwealth = sum(newHoldings)
    wealthTracker[i] = totwealth
  }
  wealthTracker
}
plot(wealthTracker, type='l', main='Aggressive Portfolio', xlab='Trading Days', ylab='Wealth')


#Now calculate the 5% value at risk for each of these three portfolios
print('Even Split')
quantile(EvenSim[,ndays], 0.05) - 100000

print('Safe Portfolio')
quantile(SafeSim[,ndays], 0.05) - 100000

print('Aggressive Portfolio')
quantile(AggSim[,ndays], 0.05) - 100000