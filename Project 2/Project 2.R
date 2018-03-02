setwd('/Users/brettscroggins/Downloads/')

# read in the data
data = read.csv("N100StkPrices.csv", header = TRUE)

# clean up data
data = na.omit(data)
ticker = data$TICKER

# spun off MDLZ
delete = seq(1, dim(data)[1])[ticker == "MDLZ"]
data = data[-delete, ]

date = apply(as.matrix(data$date), MARGIN = 1, FUN = "toString")
date = as.Date(date, "%Y%m%d")
ticker = data$TICKER
price = data$PRC
shares = data$SHROUT


# Accounting for changes in ticker names

# KFT changed to KRFT in Oct 2012.
ticker[ticker == "KFT"] = "KRFT"

# SXCI changed to CTRX in Jul 2012.
ticker[ticker == "SXCI"] = "CTRX"

# HANS changed to MNST in Jan 2012.
ticker[ticker == "HANS"] = "MNST"

# convert prices to a matrix, arranged by rows of dates and columns of tickers
unique_dates = sort(unique((date)))
unique_tickers = sort(unique(ticker))

priceMat = matrix(NA, length(unique_dates), length(unique_tickers))
sharesMat = matrix(0, length(unique_dates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_dates, date[ticker == tic])
  
  priceMat[idx, i] = price[ticker == tic]
  sharesMat[idx, i] = shares[ticker == tic]
}

rownames(priceMat) = as.character(unique_dates)
rownames(sharesMat) = as.character(unique_dates)

rm(list = c("data", "delete", "i", "idx", "price", "shares", "tic", "ticker", "date"))

# Read Monthly Data -------------------------------------------------------

# read in the data
mdata = read.csv("N100Monthly.csv", header = TRUE, stringsAsFactors = FALSE)

# clean up data
mdate = apply(as.matrix(mdata$date), MARGIN = 1, FUN = "toString")
mdate = as.Date(mdate, "%Y%m%d")

mticker = mdata$TICKER
mprice = mdata$PRC
mshares = mdata$SHROUT
mticker[mticker == "FOXA"] = "NWSA"


unique_mdates = sort(unique((mdate)))
unique_mtickers = sort(unique(mticker))

idx = is.element(unique_mtickers, unique_tickers)

# if (!all(idx)) {
#   print("Warning: Some tickers seem to be missing")
# }

monthlyPriceMat = matrix(NA, length(unique_mdates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_mdates, mdate[mticker == tic])
  monthlyPriceMat[idx, i] = mprice[mticker == tic]
}


## Calculate Daily Returns for Each Stock


### Calculate Daily Returns

returnsMat <- diff(priceMat)/priceMat[-nrow(priceMat),]


## Calculate Correlation Matrix


### Calculate the Correlation Matrix

rho <- cor(returnsMat, use = "complete.obs")


## Code the Integer Program

constructFunds <- function(rh,q,pMat,sMat,ut,ud){
  library("lpSolve")
  #set n to be the number of stocks we're looking at
  n <- length(ut)
  Q <- q
  #Create the y constraint portion of the A matrix
  y_constraint <- c(rep(0,n*n),rep(1,n))
  y_constraint <- matrix(y_constraint, 1, n*n+n)
  #Create the x constraint portion of the A matrix
  x_constraint <- c()
  for (i in 1:n) {
    x_constraint <- append(x_constraint,rep(0, n*(i-1)))
    x_constraint<- append(x_constraint,rep(1, n))
    x_constraint <- append(x_constraint,rep(0, n*(n-i)+n))
    
  }
  x_constraint <- matrix(x_constraint, byrow = TRUE,n, n*n+n)
  #Create the xy constraint portion of the A matrix
  xy_x <- diag(n*n)
  xy_y <- rep(c(diag(x=-1,n)),n)
  xy_constraint <- cbind(xy_x, matrix(xy_y, byrow=TRUE,n*n,n))
  
  
  A <- rbind(y_constraint,x_constraint,xy_constraint)
  
  b <- c(Q,rep(1,n),rep(0,n*n))
  
  dir <- c(rep("=",n+1),rep("<=",n*n))
  
  c <- c(rh,rep(0,n))
  
  sol <- lp("max",c,A,dir,b,binary.vec = 1:((n^2)+n))
  
  # Calculate the weights
  xij_solution <- matrix(sol$solution[1:(n*n)],byrow = TRUE,n,n)
  valueMat <- sMat*pMat
  lastValueDate <- valueMat[nrow(valueMat),]
  wj <- colSums(xij_solution*lastValueDate)
  total <- sum(wj)
  wt <- wj/total
  return(wt)
}


## Use Weights to Construct Index Portfolio at the End of 2012

## Create a function that returns the montly value of our index fund assuming a 1MM investment
getPerformance <- function(pMat,wts, mtpMat){
  lastPriceDate <- pMat[nrow(pMat),]
  portfolio <- wts*1000000
  portfolioShares <- portfolio/lastPriceDate
  portfolioReturn <- c()
  for (i in c(1:12)){
    portfolioReturn<-append(portfolioReturn,sum(portfolioShares*mtpMat[i,]))
  }
  return(portfolioReturn)
}

q = 25
weights <- constructFunds(rho, q, priceMat, sharesMat,unique_tickers, unique_dates)
#Stocks in our portfolio 
subset(data.frame(unique_tickers,weights), weights!=0)
#Overal Return
pReturn <- getPerformance(priceMat, weights, monthlyPriceMat)

## Calculate the monthly value of the NASDAQ assuming a 1MM investment
nasdaq <- c(2731.53, 2738.58, 2818.69, 2887.44, 2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592.00)
# calculate our index's return
total_investment = 1000000
shares <- total_investment/2660.93
nasdaqVal <- nasdaq*shares

## Plot NASDAQ vs Index Fund performance
months <- seq(as.Date("2013/1/1"), by = "month", length.out = 12)
library(ggplot2)
df <- data.frame(nasdaqVal,pReturn)
g <- ggplot(df,aes(months))
g <- g+geom_point(aes(y=nasdaqVal),color = "red")
g <- g+geom_point(aes(y=pReturn),color = "blue")
g <- g+ylab("Portfolio Value in Dollars")
g

##Plot real returns 
monthlyPriceMat = matrix(NA, length(unique_mdates), length(unique_tickers))

for (i in 1:length(unique_tickers)) {
  tic = unique_tickers[i]
  idx = is.element(unique_mdates, mdate[mticker == tic])
  monthlyPriceMat[idx, i] = mprice[mticker == tic]
}

monthlyPriceMat = rbind(priceMat[250,], monthlyPriceMat)


# calculate monthly return
returnMonthlyMat = matrix(NA, length(unique_mdates), length(unique_tickers)) #initialize matrix for daily returns

for (i in 1:length(unique_tickers)) {
  prices = monthlyPriceMat[, i]
  monthly_returns = diff(prices)
  returnMonthlyMat[, i] = monthly_returns
}


investment_vector = weights * total_investment
share_vector = investment_vector / priceMat[250,]
total_value = share_vector * monthlyPriceMat[1,]

real_return = NULL
for (i in 1:dim(returnMonthlyMat)[1]) {
  r = sum(share_vector * returnMonthlyMat[i,])
  real_return = c(real_return, r)
}
real_return

# calculate nasdaq return
nasdaq_2013 = c(2660.93, 2731.53, 2738.58, 2818.69, 2887.44, 2981.76, 2909.60, 3090.19, 3073.81, 3218.20, 3377.73, 3487.82, 3592.00)
num_share_nasdaq = total_investment / nasdaq_2013[1]
nasdaq_monthly_return = NULL
for (i in 1:length(nasdaq_2013)) {
  monthly_return = nasdaq_2013[i+1] - nasdaq_2013[i]
  nasdaq_monthly_return = c(nasdaq_monthly_return, monthly_return)
}

nasdaq_return = nasdaq_monthly_return[1:12] * num_share_nasdaq

par(mfrow=c(1, 1))
plot(nasdaq_return, type='b', xlab='Month', ylab='Return', col='red', ylim=c(-50000,130000))
lines(real_return, type='b', col='blue')
legend('topleft',legend=c('nasdaq', 'our fund'), col=c('red', 'blue'), lty=c(1, 1))


## Calculate New Similarity Matrix Using Correlation Between Stocks Using 20 Day Moving Averages

i <- 20
similarityMat <- function(pMat, sMat, ut,ud){
  
  ### Create New Similarity Matrix
  csCol <- function(stock,i){
    stock[which(is.na(stock))] <- 0 
    cs <- cumsum(stock)
    rsum <- (cs[(i+1):length(stock)] - cs[1:(length(stock) - i)]) / i
  }
  
  maStocks <- apply(pMat,2, csCol,20)
  corVarMat <- cor(maStocks,maStocks)
  return(corVarMat)
}

rho_new <- similarityMat(priceMat,sharesMat,unique_tickers,unique_dates)
weights_new <- constructFunds(rho_new,q,priceMat,sharesMat,unique_tickers,unique_dates)
newpReturn <- getPerformance(priceMat,weights_new, monthlyPriceMat)
df <- data.frame(nasdaqVal,pReturn, newpReturn)
g <- ggplot(df,aes(months))
g <- g+geom_point(aes(y=nasdaqVal),color = "red")
g <- g+geom_point(aes(y=pReturn),color = "blue")
g <- g+geom_point(aes(y=newpReturn),color = "green")
g <- g+ylab("Portfolio Value in Dollars")
g

investment_vector_new = weights_new * total_investment
share_vector_new = investment_vector_new / priceMat[250,]
total_value_new = share_vector_new * monthlyPriceMat[1,]

real_return_new = NULL
for (i in 1:dim(returnMonthlyMat)[1]) {
  r_new = sum(share_vector_new * returnMonthlyMat[i,])
  real_return_new = c(real_return_new, r_new)
}
real_return_new

par(mfrow=c(1, 1))
plot(nasdaq_return, type='b', xlab='Month', ylab='Return', col='red', ylim=c(-50000,130000))
lines(real_return, type='b', col='blue')
lines(real_return_new, type='b', col='green')
legend('topleft',legend=c('nasdaq', 'our fund', 'updated fund'), col=c('red', 'blue','green'), lty=c(1, 1,1))