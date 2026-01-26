library(PerformanceAnalytics)
library(tidyquant)
library(tseries)
library(tidyverse)

symbol_name <<- c("AAPL", "GOOG", "AMZN", "F", "A", "TQQQ") # try to put Your tickers!!
vahy <<- c(0.2,0.2,0.2,0.2,0.1,0.1)
FROM <<- "2020-01-01" # change to Your dates!!!
TO <<- "2022-12-31"

# preparing one table in common for all the downloaded tickers - You can change
for (i in 1:length(symbol_name)) {
  prac <<- Ad(getSymbols(symbol_name[i], from = FROM, to = TO,auto.assign=FALSE))
  if (i==1) {
    price <<-prac
  } else{
    price <<- merge(price,prac)
  }
}
rm(prac) # prac is just temporary variable to remove
colnames(price) <- symbol_name #puting the names of the shares

return_a <<- CalculateReturns(price, method="log")
#hist(return_a$AAPL)
# next cycle imputes the missing data by the variable medians
for(i in 1:dim(return_a)[2]){
  return_a[,i][is.na(return_a[,i])] <- median(return_a[,i],na.rm = TRUE)
}

return2_a <<- return_a[-1,]
