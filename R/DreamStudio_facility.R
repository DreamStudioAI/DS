###############################################################################
#################------------ Dream Studio Facility ----------#################
###############################################################################
#
# Des: This file includes the facilities part
#
# Ref:
#
# Team  : Dream Studio
# Author: Davy Zhu(dreamstudio@vip.163.com)
# Date  : Oct. 21st, 2016
###


### Load library ----
library(RMySQL)
library(tseries)
library(PerformanceAnalytics)
library(pROC)
library(woe)
library(plyr)
library(lubridate)

### ds.roc ----
# Calculate AUC and plot ROC
ds.roc <- function(response,predictor,title="Response"){
  vROC <- roc(response,predictor)
  vAUC <- round(vROC$auc[1],3)
  vTitle <- paste(title," AUC: ",vAUC)
  plot(vROC,col=10,main=vTitle)
}

### ds.age.I ----
# Transform the age into the age indicator
ds.age.I <- function(vAge.age,show=TRUE){
  age.avg <- mean(vAge.age)
  age.std <- sqrt(var(vAge.age))
  age.upper <- round(age.avg + age.std)
  age.low <- round(age.avg - age.std)
  if(show==TRUE){
    plot(vAge.stat,type="l",col=10)
    abline(v=c(age.low,age.avg,age.upper),col=6,lty=5)
  }
  vAge.I <- vAge.age
  vAge.a_idx <- vAge.I<age.low
  vAge.a_b.idx <- vAge.I>=age.low&vAge.I<=age.upper
  vAge.b_idx <- vAge.I>age.upper
  vAge.I[vAge.a_b.idx] <- 0
  vAge.I[vAge.a_idx] <- age.low - vAge.I[vAge.a_idx]
  vAge.I[vAge.b_idx] <- vAge.I[vAge.b_idx] - age.upper
  return(vAge.I)
}
### ds.data.norm ----
# Normalize the data; default is minmax method;
ds.data.norm <- function(vInput){
  vMin <- min(vInput)
  vMax <- max(vInput)
  if(vMin==vMax){
    print("Minimum is equal to maximum!")
    return
  }
  vRes <- (vInput-vMin)/(vMax-vMin)
  return(vRes)
}

### ds.ahp ----
# Calculate AHP rank; default is weighted score
ds.ahp <- function(vDat,vWght=1){
  #vDat <- matrix(1:20,4)
  # vWght <- c(0.1,0.15,0.2,0.25,0.3)
  vSum.wght <- round(sum(vWght))
  if(length(vWght)!=1){
    if(vSum.wght!=1){
      print("The sum of weight array is not equal 1!Return -1!")
      return(-1);
    }
    vRes.ahp <- rowSums(t(t(vDat)*vWght))
    vRes.ahp <- ds.data.norm(vRes.ahp)
    return(vRes.ahp)
  }
  if(vWght!=1){
    print("Please input the right weight array!Return -1!")
    return(-1);
  }
  vRes.ahp <- rowSums(vDat)
  vRes.ahp <- ds.data.norm(vRes.ahp)
  return(vRes.ahp)
}
### ds.accumRate ----
# Calculate the accumation Rate
ds.accumRate <- function(vData,start=1,end=1){
  if(length(which(is.na(vData)))>0||length(which(is.infinite(vData)))>0){
    stop("ERROR:there is NA value!")
    # return(c(accumRate=-1111,STD=-1111))
  }
  if(end>length(vData)||end<start||start<1||length(vData)<=2){
    stop("ERROR:start/end postion is wrong!")
    # return(c(accumRate=-1111,STD=-1111))
  }else
    if(end==1){
      end <- length(vData)
    }
  if(is.element(0,vData)){
    stop("ERROR:there is 0 value!")
    # return(c(accumRate=-1111,STD=-1111))
  }
  vAuccumRate <- as.numeric((vData[end]-vData[start])/vData[start])
  vData.tmp <- vData[start:end]
  vSTD <- sqrt(var(diff(vData.tmp)/vData.tmp[1:(length(vData.tmp)-1)]))
  # vSTD <- sqrt(var(diff(vData[start:end])))
  # vSTD <- sqrt(var(vData[start:end]))
  return(c(accumRate=vAuccumRate,STD=vSTD))
}

### ds.mdd ----
# get the max draw down of a sequence
# this function is as the same as maxdrawdown in package "tseries"
ds.mdd <- function(x){
  if (NCOL(x) > 1)
    stop("x is not a vector or univariate time series")
  if (any(is.na(x)))
    stop("NAs in x")
  cmaxx <- cummax(x) - x
  mdd <- max(cmaxx)
  to <- which(mdd == cmaxx)
  from <- double(NROW(to))
  for (i in 1:NROW(to)) from[i] <- max(which(cmaxx[1:to[i]] == 0))
  return(list(maxdrawdown = mdd, from = from, to = to))
}

### ds.evaluate.seq ----
# calculate the accumlative profit ratio/STD/max std/average AccumRate/Max draw down
# for a numeric sequence
ds.evaluate.seq <- function(vData){
  if (NCOL(vData) > 1)
    stop("this is not a vector!")
  if (any(is.na(vData)))
    stop("there are NAs!")
  vAccumRate <- ds.accumRate(vData)
  vMdd <- ds.mdd(vData)
  vMdd.ratio <- vMdd$maxdrawdown/vData[vMdd$from[1]]
  if(vMdd$maxdrawdown==0)vMdd.ratio <- 0
  if(NROW(vData)<2)
    stop("Value number is less than 2!")
  vAccumRate.arry <- c()
  for(i in 1:NROW(vData)){
    vTmp.accumRate <- ds.accumRate(vData,start=1,end=i)
    vAccumRate.arry <- rbind(vAccumRate.arry,vTmp.accumRate)
  }
  vAccumRate.avg <- mean(vAccumRate.arry[,"accumRate"])
  vAccumRate.min <- min(vAccumRate.arry[,"accumRate"])
  vSTD.max <- max(vAccumRate.arry[,"STD"])
  return(data.frame(accumRate=vAccumRate["accumRate"],accumRate.avg=vAccumRate.avg,
                    accumRate.min=vAccumRate.min,STD=vAccumRate["STD"],STD.max=vSTD.max,
                    mdd=vMdd.ratio))
}
### ds.dbConnect ----
# connect the database by using configuration prameters
# return a connection; depends on RMySQL library
ds.dbConnect <- function(conf,encoding='UTF8') {
  library(RMySQL)
  con <- dbConnect(MySQL(), user=conf$user, password=conf$password,
                   dbname=conf$dbname, host=conf$host)
  print(dbSendQuery(con, paste("SET NAMES '",encoding,"'",sep="")))
  return(con)
}

### ds.getSQL.Data ----
# get the data base data by using RMySQL methods
# depends on RMySQL library
ds.getSQL.Data <- function(conf, sqlBody,encoding='UTF8') {
  library(RMySQL)
  con <- dbConnect(MySQL(), user=conf$user, password=conf$password,
                   dbname=conf$dbname, host=conf$host, client.flag=CLIENT_MULTI_STATEMENTS)
  print(dbSendQuery(con, paste("SET NAMES '",encoding,"'",sep="")))
  # Get the fund dictionary ----
  rs <- dbSendQuery(con, sqlBody)
  data <- fetch(rs,n=-1)
  # save(s1.data,file="RData/s1.data.RData")
  if(dbClearResult(rs))dbDisconnect(con)
  return(data)
}

### ds.writeSQL.Data ----
# connect the database by using configuration prameters
# write the data to a table
ds.writeSQL.Data <- function(conf,vTable,vInsertData,apnd=TRUE,overwrte=FALSE,rownm=FALSE,encoding='UTF8') {
  ### Write the result into the database
  con <- ds.dbConnect(conf,encoding=encoding)
  vSQLRes <- dbWriteTable(con,vTable,vInsertData,append=apnd,overwrite=overwrte,row.names=rownm)
  if(vSQLRes){
    dbDisconnect(con)
    print("Connection is closed!")
  }
}

### ds.getRandomWght ----
# get the random weights array such that the sum of them is 1
ds.getRandomWght <- function(size,scale=100) {
  x <- c()
  vWght.all <- 1:scale / scale
  for (i in 1:(size - 1)) {
    vWght.all <- vWght.all[vWght.all <= 1 - sum(x)]
    if (length(vWght.all) != 0)
      x[i] <- sample(vWght.all, 1)
    else
      x[i] <- 0
  }
  x[size] <- 1 - sum(x)
  vId.x <- sample(1:size,size)
  x <- x[vId.x]
  return(x)
}

### ds.annReturn ----
# type=seq : calculate the annual return for a trading value sequence
# type=single,len=? : calculate the annual return for a value within len years
# type=single,geometric=FALSE,len=? : when the interval is less than 1 year,
# len denotes len days
# e.g.
# ds.annReturn(1.3,type="single",geometric=FALSE,len=100)
ds.annReturn <- function(vData,scale=252,geometric=TRUE,type="seq",len=1) {
  if(type=="seq"){
    R <- diff(vData)/vData[1:(length(vData)-1)]
    vYears <- NROW(vData)/scale
    if(geometric==TRUE){
      vAnn <- (last(vData)/first(vData))^(1/vYears)-1
    }else vAnn <- mean(R)*scale
    return(vAnn)
  }
  if(type=="single"){
    vYears <- len
    if(geometric==TRUE){
      vAnn <- last(vData)^(1/vYears)-1
    }else vAnn <- (last(vData)-1)/len*scale
    return(vAnn)
  }
}

### ds.sharpeRatio ----
# calculate the sharpe ratio for sequence vData with Rf
ds.sharpeRatio <- function(vData,Rf=0,scale=252,geometric = TRUE) {
  R <- diff(vData)/vData[1:(length(vData)-1)]
  R.ann <- Return.annualized(R,scale=scale,geometric = geometric)
  volatility <- sqrt(var(R))*sqrt(scale)
  if(length(which(R<0))>0){
    vDownRisk <- mean(R[R<0])
  }else vDownRisk <- -runif(1,0,.005)
  vSR <- (R.ann-Rf)/volatility
  return(cbind(volatility,vDownRisk,vSR))
}


### ds.TSInterpolate ----
# Interpolate the time series NA value
# especially for trading net/accumlated value
ds.TSInterpolate <- function(vData) {
  if (is.na(vData[1])) {
    # interpolate the first NA vale
    vIdx <- which(!is.na(vData))[1]
    vData[1:vIdx] <- vData[vIdx]
  }
  vIdx <- which(is.na(vData))
  for (j in vIdx) {
    vData[j] <- vData[j - 1]
  }
  return(vData)
}

### ds.workdays ----
# extract the work days of a "Date" type time vector
# vHolidays is a customized holidays vector
ds.workdays <- function(vDate, vHolidays = NULL){
  library(lubridate)
  vDate <- vDate[!wday(vDate) %in% c(1,7)]
  vDate <- as.Date(setdiff(as.character(vDate), as.character(vHolidays)))
  return(vDate)
}

### ds.timeDrill ----
# calculate the object date with a "Date" type time vector vDate + vNum
# by day/month etc.
# e.g. ds.timeDrill(as.Date("2000-01-30"),1,"month")
# e.g. ds.timeDrill(as.Date("2000-02-29"),1,"year")
ds.timeDrill <- function(vDate, vNUM, vType = "day") {
  library(lubridate)
  switch(
    vType,
    day = return(vDate + days(vNUM)),
    week = return(vDate + weeks(vNUM))
  )
  if(vType %in% "month"){
    vDate.ex <- vDate
    vDate.ex <- vDate + months(vNUM)
    vId.na <- which(is.na(vDate.ex))
    for(i in vId.na){
      vTmp <-
        seq.Date(vDate[i]-days(3), vDate[i], by="days")
      vTmp <- vTmp + months(vNUM)
      vTmp.date.ex <-
        vTmp[last(which(!is.na(vTmp)))]
      vDate.ex[i] <- vTmp.date.ex
    }
    return(vDate.ex)
  }
  if(vType %in% "year"){
    vDate.ex <- vDate
    vDate.ex <- vDate + years(vNUM)
    vId.na <- which(is.na(vDate.ex))
    for (i in vId.na) {
      vTmp <-
        seq.Date(vDate[i] - days(3), vDate[i], by = "days")
      vTmp <- vTmp + years(vNUM)
      vTmp.date.ex <-
        vTmp[last(which(!is.na(vTmp)))]
      vDate.ex[i] <- vTmp.date.ex
    }
    return(vDate.ex)
  }
}

### ds.optCombWght ----
# calculate the optimal weights for the combinations
# vCombFund.m is the raw data
# vThreshold.mdd is the threshold of the combination's mdd
# tries is the try count for monte carlo tries, default is 2500
ds.optCombWght <- function(vCombFund.m, vThreshold.mdd, Ifshow=TRUE,tries=2500) {
  library(PerformanceAnalytics)
  vFundId.comb.arry <- colnames(vCombFund.m)
  vNum.funds <- NCOL(vCombFund.m)
  x <- rep(round(1 / vNum.funds, 2),vNum.funds)
  # x <- round(ds.getRandomWght(vNum.funds), 2)
  x.num <- unlist(x / vCombFund.m[1,])
  vComb.value <- colSums(x.num * t(vCombFund.m))
  vEva <- ds.evaluate.seq(vComb.value)

  vEva.all <- c()
  for (i in vFundId.comb.arry) {
    vEva.all <- rbind(vEva.all, cbind(
      ds.evaluate.seq(vCombFund.m[, i]),
      ds.sharpeRatio(vCombFund.m[, i])
    ))
  }
  vMean.avg.STD <- mean(vEva.all$accumRate.avg / vEva.all$STD)
  vAccumRate.summary <- summary(vEva.all$accumRate)
  vDownRisk.mean <- mean(vEva.all$vDownRisk)
  vSR.mean <- mean(vEva.all$vSR)
  vVolatility.mean <- mean(vEva.all$volatility)
  vAcc.threshold <- max(vAccumRate.summary["1st Qu."], 0)

  vEva.best <- vEva
  vX.best <- x
  vSR.best <- ds.sharpeRatio(vComb.value)
  for (i in 1:tries) {
    x <- round(ds.getRandomWght(vNum.funds), 2)
    x.num <- unlist(x / vCombFund.m[1,])
    vComb.value <- colSums(x.num * t(vCombFund.m))
    vEva <- ds.evaluate.seq(vComb.value)
    vSR.res <- ds.sharpeRatio(vComb.value)
    if (vSR.res[, "vSR"] < vSR.best[, "vSR"] ||
        # vEva$accumRate < vEva.best$accumRate ||
        vEva$mdd >= vThreshold.mdd ||
        # vEva$accumRate.avg / vEva$STD < vEva.best$accumRate.avg / vEva.best$STD ||
        #vSR.res[,"vSR"] < vSR.best[,"vSR"]||
        # vSR.res[,"vDownRisk"]<=vDownRisk.mean||
        # vSR.res[,"volatility"]>=vVolatility.mean||
        vEva$accumRate <= 0)
      next()
    else if(vSR.res[, "vSR"]>=1&&vEva$accumRate < vEva.best$accumRate)
      next()
    vSR.best <- vSR.res
    vEva.best <- vEva
    vComb.value.best <- vComb.value
    vX.best <- x
    vReturn.cal.comb <- Return.calculate(vCombFund.m)
    vReturn.cal <- Return.calculate(vComb.value.best)
    vReturn.cal.comb <- cbind(vReturn.cal.comb, vReturn.cal)
    if(Ifshow==TRUE)chart.CumReturns(vReturn.cal.comb, main = "cumulative return")
  }
  names(vX.best) <- vFundId.comb.arry
  return(vX.best)
}

### ds.optCombWght.IR ----
# calculate the optimal weights for the combinations with Information Ratio
# vCombFund.m is the raw data
# vBenchWght is the bench mark weights
# tries is the try count for monte carlo tries, default is 2500
ds.optCombWght.IR <- function(vCombFund.m,vBenchWght, tries=2500) {
  library(PerformanceAnalytics)
  x.Bench.num <- unlist(vBenchWght / vCombFund.m[1,])
  vBench.value <- colSums(x.Bench.num * t(vCombFund.m))
  vBench.return <- CalculateReturns(vBench.value)
  vFundId.comb.arry <- colnames(vCombFund.m)
  vNum.funds <- NCOL(vCombFund.m)
  x <- rep(round(1 / vNum.funds, 2),vNum.funds)
  x <- round(ds.getRandomWght(vNum.funds), 2)
  x.num <- unlist(x / vCombFund.m[1,])
  vComb.value <- colSums(x.num * t(vCombFund.m))
  vComb.return <- CalculateReturns(vComb.value)
  vIR <- InformationRatio(vComb.return,vBench.return,scale=252)

  vIR.best <- vIR
  for (i in 1:tries) {
    x <- round(ds.getRandomWght(vNum.funds), 2)
    x.num <- unlist(x / vCombFund.m[1,])
    vComb.value <- colSums(x.num * t(vCombFund.m))
    vComb.return <- CalculateReturns(vComb.value)
    # vIR <- ActivePremium(vComb.return, vBench.return, scale = 252)
    vIR <- InformationRatio(vComb.return,vBench.return,scale=252)
    if (vIR <= vIR.best)
      next()
    vIR.best <- vIR
    vComb.value.best <- vComb.value
    vX.best <- x
    vReturn.cal.comb <- Return.calculate(vCombFund.m)
    vReturn.cal <- Return.calculate(vComb.value.best)
    vReturn.cal.comb <- cbind(vReturn.cal.comb, vReturn.cal)
    chart.CumReturns(vReturn.cal.comb, main = "cumulative return")
  }
  if(vIR.best<0)vX.best <- vBenchWght
  names(vX.best) <- vFundId.comb.arry
  return(vX.best)
}


### ds.transfer.Fee ----
# calculate buy and redeem fee
ds.transfer.Fee<-function(Money,Selected.fee,Selected.redeemfee){
  #if(Money==0) return (0)
  #last.date<-as.Date(last.data[,"valueDate"])
  #min.add.month=as.integer(month(min(S1_Routine$valueDate)))
  #根据上一次选择的fundId的最后时间来确定基金持有时间
  vMoney.totalfee<-0
  #if(ChangeFlag==1&&vSelected.fundId==s1.last2.record[1,])#如果发生了调仓，只执行调仓这一次。第二次执行时vSelected.fundId!=s1.last2.record[1,1]
  vMoney.buyfee<-Selected.fee/1000000  #考率调仓申购手续费
  vMoney.redeemfee<-Selected.redeemfee/100000000*Money
  vMoney.totalfee<-(Money-vMoney.redeemfee)*vMoney.buyfee
  if(vMoney.totalfee< 0)
    return (0)
  if(Selected.fee==0)
    return (vMoney.redeemfee)  #只赎回，申购费率是0
  else
    return (vMoney.totalfee)
}

### ds.quantEvaList ----
# calculate every evaluation indicators for a strategy's sequence
ds.quantEvaList <- function(vData){
  vSR.arry <- ds.sharpeRatio(vData)
  vES.arry <- ds.evaluate.seq(vData)
  vAnnR <- as.numeric(ds.annReturn(vData)) # clear the names by using as.numeric
  vCalmaR <- vAnnR/vES.arry$mdd
  return(c(accumReturn=vES.arry$accumRate,annReturn=vAnnR,vSR.arry[,c("volatility","vSR")],
           mdd=vES.arry$mdd,CalmaRatio=vCalmaR))
}

### ds.getOccurCount ----
# calculate every element's apperance count for the vector vData
ds.getOccurCount <- function(vData){
  # x <- c(5,4,2,3,2,3,1,2,2,3)
  count_i.all <- c()
  for(i in 1:length(vData)){
    count_i <- which(which(vData==vData[i])==i)
    count_i.all <- c(count_i.all,count_i)
  }
  return(count_i.all)
}

### ds.inv_F ----
# calculate the inverse function of a hist distribution
# x is random value generated by runif() function
# breaks and F_density is the variables from hist function
# F_density is between 0 and 1
ds.inv_F <- function(x,breaks,F_density) {
  F_density <- c(0,F_density)
  for(i in 1:(length(breaks)-1)){
    if(x>=F_density[i] && x<=F_density[i+1])
      F <- runif(1,breaks[i],breaks[i+1])
  }
  return(F)
}

### ds.sample.F ----
# sample from a hist distribution
# n is the count of sample
# breaks and F_density is the variables from hist function
# F_density is between 0 and 1
ds.sample.F <- function(n,breaks,F_density){
  x <- runif(n)
  sam <- sapply(x,ds.inv_F,breaks,F_density)
  return(sam)
}



