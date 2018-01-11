library(quantmod)
GSPC <- getSymbols("^GSPC",auto.assign=FALSE)



T.ind <- function(quotes, tgt.margin = 0.025, n.days = 10) {
   v <- apply(HLC(quotes), 1, mean)
   v[1] <- Cl(quotes)[1]
   r <- matrix(NA, ncol = n.days, nrow = NROW(quotes))
   for (x in 1:n.days) r[, x] <- Next(Delt(v, k = x), x)
   x <- apply(r, 1, function(x) sum(x[x > tgt.margin | x < -tgt.margin]))
   if (is.xts(quotes)) xts(x, time(quotes)) else x
}


candleChart(last(GSPC,'3 months'),theme='white', TA=NULL)
 avgPrice <- function(p) apply(HLC(p),1,mean)
 addAvgPrice <- newTA(FUN=avgPrice,col=1,legend='AvgPrice')
 addT.ind <- newTA(FUN=T.ind,col='red', legend='tgtRet')
 addAvgPrice(on=1)
 addT.ind()
 
 
 
 ###########
  library(TTR)
  myATR <- function(x) ATR(HLC(x))[,'atr']
  mySMI <- function(x) SMI(HLC(x))[, "SMI"]
  myADX <- function(x) ADX(HLC(x))[,'ADX']
  myAroon <- function(x) aroon(cbind(Hi(x),Lo(x)))$oscillator
  myBB <- function(x) BBands(HLC(x))[, "pctB"]
  myChaikinVol <- function(x) Delt(chaikinVolatility(cbind(Hi(x),Lo(x))))[, 1]
  myCLV <- function(x) EMA(CLV(HLC(x)))[, 1]
  myEMV <- function(x) EMV(cbind(Hi(x),Lo(x)),Vo(x))[,2]
  myMACD <- function(x) MACD(Cl(x))[,2]
  myMFI <- function(x) MFI(HLC(x), Vo(x))
  mySAR <- function(x) SAR(cbind(Hi(x),Cl(x))) [,1]
  myVolat <- function(x) volatility(OHLC(x),calc="garman")[,1]
  
  
  ########## not working
  library(randomForest)
   data.model <- specifyModel(T.ind(GSPC) ~ Delt(Cl(GSPC),k=1:10) +
                                  myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) + myAroon(GSPC) +
                                  myBB(GSPC) + myChaikinVol(GSPC) + myCLV(GSPC) +
                                  CMO(Cl(GSPC)) + EMA(Delt(Cl(GSPC))) + myEMV(GSPC) +
                                  myVolat(GSPC) + myMACD(GSPC) + myMFI(GSPC) + RSI(Cl(GSPC)) +
                                  mySAR(GSPC) + runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
   
   
   
   
   data.model <- specifyModel(T.ind(GSPC) ~ myATR(GSPC) + mySMI(GSPC) + myADX(GSPC) +
                                 myAroon(GSPC) + myVolat(GSPC) +
                                 myMACD(GSPC) + myMFI(GSPC) + mySAR(GSPC) +
                                 runMean(Cl(GSPC)) + runSD(Cl(GSPC)))
   
   set.seed(1234)
   rf <- buildModel(data.model,method='randomForest',
                      training.per=c("2005-01-01","2010-12-30"),
                     ntree=1000, importance=TRUE)