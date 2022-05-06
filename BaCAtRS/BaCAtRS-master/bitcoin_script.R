# https://www.kaggle.com/hrkzszk/analysis-in-cryptocurrencies-using-r 

dat <- read.csv(file.choose())

# make data 

close.raw <- reshape(dat[c(1,2,6)], timevar= "Currency", idvar = "Date", direction = "wide")
close.raw[,"Close.Currency"] <- NULL
close <- data.frame(sapply(close.raw, function(z){as.numeric(as.character(z))}))
colnames(close) <- sub("Close.", "", colnames(close))
library(lubridate)
dates <- parse_date_time(x = as.character(close.raw$Date), orders ="b/d/Y", locale = "eng")
close$Date<- dates
close <- close[-nrow(close),]
dim(close)

markcap <- dat[c(1,2,8)]
markcap$Market.Cap[markcap$Market.Cap == "-"] <- NA
mean.cap <- data.frame(mean.cap=colMeans(markcap, na.rm = TRUE))
mean.cap.10.name <- rownames(mean.cap[order(mean.cap$mean.cap, decreasing = T),,drop=F])[1:10]

library(xts)
rownames(close) <- close$Date
close.xts <- as.xts(close)

#price10 <- as.xts(close)
#plot.xts(price10, main="Price")

plot(as.ts(close[, 2:6]), main=paste("TS Plot for ", 5, "coins"))

library(PerformanceAnalytics)

CalculateReturns(as.ts(close[, 24]), method = "log")

apply(close[, 2:10], 2, function(x) mean(x, na.rm = TRUE))
apply(close[, 2:10], 2, function(x) var(x, na.rm = TRUE))
apply(close[, 2:10], 2, function(x) sd(x, na.rm = TRUE))

apply(close[, 2:10], 2, function(x) CVaR(x))
CVaR(close[, 24])

# volt charts

library(TTR)
#vol30 <- xts(apply(close[, 24], 2, runSD,n=30), index(close[, 24]))*sqrt(252)
#ohlc <- dat[, c("Open", "High", "Low", "Close")]
#volt <- volatility(as.numeric(ohlc[, "Close"]), calc = "close")

plot(volatility(as.numeric(na.omit(close[, 23])), calc = "close"))


# kernel density plots

plot(density(close[, 24]))

# Portfolio selection - efficient frontier

close.ef <- na.omit(close[, 20:24])
mean_vect <- colMeans(as.matrix(close.ef))
cov_mat <- cov(close.ef)
sd_vect <- sqrt(diag(cov_mat))

M <- length(mean_vect)

library(quadprog)
Amat <- cbind(rep(1,M), mean_vect) # set the constraints matrix
muP <- seq(0.0, 0.3, length=300) # set of 300 possible target values
sdP <- muP # set up storage for std dev's of portfolio returns
weights <- matrix(0, nrow=300, ncol=M) # set up storage for weights
for (i in 1:length(muP)){
  bvec <- c(1, muP[i]) # constraint vector
  result <- solve.QP(Dmat = cov_mat, dvec=rep(0,M), Amat=Amat, bvec = bvec, meq=2) 
  sdP[i] <- sqrt(2*result$value) 
  weights[i,] <- result$solution
}

plot(100*sdP, 100*muP, type="l", xlim=c(0,250), ylim=c(0,35), 
     main="Efficient Frontier", ylab="Expected Return(%)", xlab="Standard deviation(%)")

ind.min <- which.min(sdP)
#Expected return
muP[ind.min]

#Expected standard deviation
sdP[ind.min]

#Proportions
weight <- data.frame(proportion = weights[ind.min,]) #, row.names = colnames(close[, -1]))
weight
