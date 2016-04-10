library(quantmod)
library(TTR)
library(xts)
library(randomForest)
library(nnet)
library(DMwR)
library(e1071)
library(kernlab)
library(earth)

# Choose stock company
Nasdaq100_Symbols <- "AAPL"
getSymbols(Nasdaq100_Symbols)

# The incidicator function
T.ind <- function(quotes, targetMargin = 0.025, n.days = 10) {
        v <- apply(HLC(quotes), 1, mean)
        r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
        for (x in 1:n.days) {
                r[, x] <- Next(Delt(v, k = x), x)}
        x <- apply(r, 1, function(x) sum(x[x > targetMargin | x < -targetMargin]))
        if (is.xts(quotes))
                xts(x, time(quotes))
        else x
}


# A variation of different functions 

myATR <- function(x) ATR(HLC(x))[, "atr"]
mySMI <- function(x) SMI(HLC(x))[, "SMI"]
myADX <- function(x) ADX(HLC(x))[, "ADX"]
myBB <- function(x) BBands(HLC(x))[, "pctB"]
myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
myMACD <- function(x) MACD(Cl(x))[, 2]
myVolat <- function(x) volatility(OHLC(x), calc = "garman")[,1]


# Add in as many different variables as we may think may be uselful in the prediction
data.model <- specifyModel(T.ind(AAPL) ~ Delt(Cl(AAPL),k=1:10) +
                                   myATR(AAPL) + mySMI(AAPL) + myADX(AAPL) +
                                   myBB(AAPL) + myCLV(AAPL) +
                                   CMO(Cl(AAPL)) + EMA(Delt(Cl(AAPL))) +
                                   myVolat(AAPL) + myMACD(AAPL) +
                                   runMean(Cl(AAPL)) + runSD(Cl(AAPL)))


# Set the seed and build random forest model
set.seed(1234)
rf <- buildModel(data.model,method='randomForest',
                 training.per=c(start(AAPL),index(AAPL["2008-12-31"])),
                 ntree=50, importance=T)

# Look at relative importance of the variables
varImpPlot(rf@fitted.model, type = 1)
imp <- importance(rf@fitted.model, type = 1)

# Only take variables with importance > 10
rownames(imp)[which(imp > 10)]
# Only outputs above 10 are myVolat and runMean

# New model with only important variables
data.model <- specifyModel(T.ind(AAPL) ~ Delt(Cl(AAPL), k = 1) + myVolat(AAPL) + runMean(Cl(AAPL)))

# Threshold
Thresh <- 0.1


Tdata.train <- as.data.frame(modelData(data.model, data.window=c('2008-01-01','2014-12-31')))
Tdata.eval <- na.omit(as.data.frame(modelData(data.model, data.window=c('2015-01-01','2015-12-31'))))
Tform <- as.formula('T.ind.AAPL ~ .')


# Using Neural networks to build predictions
set.seed(1234)
 
norm.data <- scale(Tdata.train)
# Decay and size should be changed to achieve best results
nn <- nnet(Tform, norm.data[1:1000, ], size = 5, decay = 0.01, maxit = 1000, linout = T, trace = F)
norm.preds <- predict(nn, norm.data[1001:1250, ])
preds <- unscale(norm.preds, norm.data)

sigs.nn <- trading.signals(preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:1250, "T.ind.AAPL"], 0.1, -0.1)
# Show the precision and recall values
# Low precision scores mean that the model gave wrong signals rather frequently
# Low recall scores mean lost opportunities
sigs.PR(sigs.nn, true.sigs)

set.seed(1234)

signals <- trading.signals(Tdata.train[, "T.ind.AAPL"], 0.1, -0.1)
norm.data <- data.frame(signals = signals, scale(Tdata.train[,-1]))
nn <- nnet(signals ~ ., norm.data[1:1000, ], size = 10, decay = 0.01, maxit = 1000, trace = F)
preds <- predict(nn, norm.data[1001:1250, ], type = "class")

# Using Support Vector Machines to build predictions
sv <- svm(Tform, Tdata.train[1:1000, ], gamma = 0.001, cost = 50)
s.preds <- predict(sv, Tdata.train[1001:1250, ])
sigs.svm <- trading.signals(s.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:1250, "T.ind.AAPL"], 0.1, -0.1)
sigs.PR(sigs.svm, true.sigs)

# Classification using the Kernlab package
data <- cbind(signals = signals, Tdata.train[, -1])
ksv <- ksvm(signals ~ ., data[1:1000, ], C = 10)
ks.preds <- predict(ksv, data[1001:1250, ])
sigs.PR(ks.preds, data[1001:1250, 1])

# Regression using Earth function
e <- earth(Tform, Tdata.train[1:1000, ])
e.preds <- predict(e, Tdata.train[1001:1250, ])
sigs.e <- trading.signals(e.preds, 0.1, -0.1)
true.sigs <- trading.signals(Tdata.train[1001:1250, "T.ind.AAPL"], 0.1, -0.1)
sigs.PR(sigs.e, true.sigs)

# Classification using the Kernlab gave the best results

shortTradePolicy <- function(signals,market,opened.pos,money, bet=0.2,hold.time=10,
                     exp.prof=0.025, max.loss= 0.05)
        {
                d <- NROW(market) # this is the ID of today
                orders <- NULL
                numOrders <- NROW(opened.pos)
                if (!numOrders && signals[d] == 'h') return(orders)
                # Check for long positions
                if (signals[d] == 'b' && !numOrders) {
                        quant <- round(bet*money/market[d,'Close'],0)
                        if (quant > 0)
                                orders <- rbind(orders,
                                        data.frame(order=c(1,-1,-1),order.type=c(1,2,3),
                                        val = c(quant, market[d,'Close']*(1+exp.prof),
                                        market[d,'Close']*(1-max.loss)),
                                        action = c('open','close','close'),
                                        posID = c(NA,NA,NA)))
                }
                # Check for short positions
                else if (signals[d] == 's' && !numOrders) {
                # this is the nr of stocks we already need to buy
                # because of currently opened short positions
                        need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1, 
                                                "N.stocks"])*market[d,'Close']
                        quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
                        if (quant > 0)
                                orders <- rbind(orders, data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                                val = c(quant,market[d,'Close']*(1-exp.prof),
                                                market[d,'Close']*(1+max.loss)),
                                                action = c('open','close','close'),
                                                posID = c(NA,NA,NA)))
                }
                # Check if hold time is up
                if (numOrders)
                        for(i in 1:numOrders) {
                                if (d - opened.pos[i,'Odate'] >= hold.time)
                                        orders <- rbind(orders,
                                        data.frame(order=-opened.pos[i,'pos.type'],
                                        order.type=1,
                                        val = NA,
                                        action = 'close',
                                        posID = rownames(opened.pos)[i]))
                        }
                        orders
}

longTradePolicy <- function(signals,market,opened.pos,money, bet=0.2,exp.prof=0.025, max.loss= 0.05){
                d <- NROW(market) # this is the ID of today
                orders <- NULL
                numOrders <- NROW(opened.pos)
                        if (!numOrders && signals[d] == 'h') return(orders)
                        # Check for long positions
                        if (signals[d] == 'b') {
                                quant <- round(bet*money/market[d,'Close'],0)
                                if (quant > 0)
                                        orders <- rbind(orders,
                                                data.frame(order=c(1,-1,-1),order.type=c(1,2,3),
                                                val = c(quant,
                                                market[d,'Close']*(1+exp.prof),
                                                market[d,'Close']*(1-max.loss)),
                                                action = c('open','close','close'),
                                                posID = c(NA,NA,NA)))
                        # Check for short positions
                        } else if (signals[d] == 's') {
                        # Money already committed to buy stocks from open shorts
                                need2buy <- sum(opened.pos[opened.pos[,'pos.type']==-1, 
                                                           "N.stocks"])*market[d,'Close']
                                quant <- round(bet*(money-need2buy)/market[d,'Close'],0)
                                if (quant > 0)
                                        orders <- rbind(orders,
                                        data.frame(order=c(-1,1,1),order.type=c(1,2,3),
                                        val = c(quant,
                                        market[d,'Close']*(1-exp.prof),
                                        market[d,'Close']*(1+max.loss)),
                                        action = c('open','close','close'),
                                        posID = c(NA,NA,NA)))
                        }
                        orders
}

# Train and test periods
start <- 1
len.train <- 100
len.test <- 50
trainingData <- start:(start+len.train - 1)
testingData <- (start+len.train):(start+len.train+len.test - 1)
# getting the quotes for the testing period
data(GSPC)
date <- rownames(Tdata.train[start+len.train,])
market <- GSPC[paste(date,'/',sep='')][1:len.test]

# learning the model and obtaining its signal predictions using Kernlab classification
data <- cbind(signals = signals, Tdata.train[, -1])
s <- ksvm(signals ~ ., data[trainingData, ], C = 10)
p <- predict(s,Tdata.train[testingData,])
sig <- trading.signals(p,0.1,-0.1)

# now using the simulated trader, first using the first policy, based on short trades
shortTrades <- trading.simulator(market,sig,'shortTradePolicy',list(exp.prof=0.05,bet=0.2,hold.time=30))
summary(shortTrades)
tradingEvaluation(shortTrades)
plot(shortTrades, market, name = "Short Trades - APPL")

# Using the second trading policy based on long trades
t2 <- trading.simulator(market, sig, "longTradePolicy", list(exp.prof = 0.05, bet = 0.3))
summary(t2)
tradingEvaluation(t2)
plot(t2, market, name = "Long Trades - APPL")
