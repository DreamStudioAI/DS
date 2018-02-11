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
  dirName <- c("RCodes","RData","Shell","PyScripts","SQLScripts","Doc","Input","Output")
  ds.mkdir(path = path,dirName = dirName)
}


### ds.mkfile.RSol ----
ds.mkfile.RSol <-
  function(vTitle = "DS_Test",
           vAuthor = "Davy Zhu",
           vEmail = "dreamstudio@vip.163.com",
           vTeam = "Dream Studio") {
    vCurTime <- Sys.Date()
    vInput.conf.file <- "Input/configuration.conf"
    vRcodes.main.file <-
      paste("RCodes/", vTitle, "_main.R", sep = "")
    vRcodes.prep.file <-
      paste("RCodes/", vTitle, "_data_prep.R", sep = "")
    vRcodes.facilities.file <-
      paste("RCodes/", vTitle, "_facilities.R", sep = "")
    vTxt.conf <-
      paste(
        "user  password  dbname  host  start.date  end.date  unix_org_date
        User  '@#$%'  Schema  10.10.10.10  '2015-10-01'  '2016-10-01'    '1970-01-01'"
      )
    write(vTxt.conf, file = vInput.conf.file)
    vTxt.main <-
      paste(
        "###############################################################################
################--------------- ",
vTitle,
"_main -------------##################
###############################################################################
#
# Des:
# Ref:
#
# Team  : ",
vTeam,
"
# Author: ",
vAuthor,
"
# Email : ",
vEmail,
"
# Date  : ",
vCurTime,
'
###

### Get facility and data file ----
source("',
vRcodes.facilities.file,
'")
source("',
vRcodes.prep.file,
'")

### Load RData ----',
sep = ""
)
    write(vTxt.main, file = vRcodes.main.file)

    vTxt.facility <-
      paste(
        "###############################################################################
#############--------------- ",
vTitle,
"_facilities ------------################
###############################################################################
#
# Des:
# Ref:
#
# Team  : ",
vTeam,
"
# Author: ",
vAuthor,
"
# Email : ",
vEmail,
"
# Date  : ",
vCurTime,
"
###

### Load library ----
library(DS)
library(RMySQL)",sep=""
)
    write(vTxt.facility, file = vRcodes.facilities.file)

    vTxt.prep <-
      paste(
        '###############################################################################
###############-------------- RA_OptFund_data_prep --------------##############
###############################################################################
#
# Des:
# Ref:
#
# Team  : RA-ML Team @CAIFUPAI Noah
# Author: Davy Zhu(zhudaihui@noahwm.com)
# Date  : Oct. 21st, 2016
###

###
# Load the configuration file
conf <- read.table(file="',
vInput.conf.file,
'",header=TRUE,stringsAsFactors=FALSE)'
,sep="")
    write(vTxt.prep, file = vRcodes.prep.file)
  }
