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
# Date  : Jun. 13rd, 2015
###


### Load library ----
library(pROC)

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


