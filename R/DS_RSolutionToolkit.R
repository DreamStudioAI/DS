###############################################################################
################--------- Toolkit for create R Solution -------################
###############################################################################
#
# Des: This file includes the facilities part
#
# Ref:
#
# Team  : Dream Studio
# Author: Davy Zhu(dreamstudio@vip.163.com)
# Date  : Oct. 1st, 2016
###

### ds.mkdir ----
# make directories in a certain path, default is current path
ds.mkdir <- function(path="NULL",dirName){
  curPath <- getwd()
  if(path!="NULL") setwd(path)
  tapply(dirName, 1:length(dirName), dir.create)
  setwd(curPath)
}

### ds.rmdir ----
# remove directories in a certain path
ds.rmdir <- function(path="NULL",dirName){
  curPath <- getwd()
  if(path!="NULL") setwd(path)
  file.remove(dirName)
  setwd(curPath)
}

### ds.mkdir.RSol ----
ds.mkdir.RSol <- function(path="NULL"){
  dirName <- c("RCodes","RData","Shell","PyScripts","SQLScripts","Doc")
  ds.mkdir(path = path,dirName = dirName)
}
