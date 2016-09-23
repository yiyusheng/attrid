# Data prepare for sc16
rm(list = ls())
source('head.R')
library(ggplot2)

#@@@ CONFIGURE @@@#
load(file.path(dir_dataSource,'load_ftr_attrid.Rda'))

#test
lowerTime <- as.POSIXct('2013-07-01')
upperTime <- as.POSIXct('2013-09-01')
saveName <- 'dataPrepareAFR1307_1308.Rda'

dataPrepare <- function(lowerTime,upperTime,saveName){
  # S1. Failure record prepare
  data.f <- subset(data.flist, f_time < upperTime & f_time > lowerTime)
  data.f <- subset(data.f,ip %in% cmdb$ip & svr_id %in% cmdb$svr_asset_id)
  
  data.f$failShiptime <- as.numeric(difftime(data.f$f_time,data.f$use_time,tz = 'UTC',units = 'days'))/365
  data.f$fsTime <- floor(data.f$failShiptime)
  data.f$fsTimeN <- cut(data.f$failShiptime,c(0,1/2,1:7),include.lowest = T)
  data.f$fsTimeN <- gsub('^\\[|^\\(|,.*$','',data.f$fsTimeN)
  
  # S2. Compute online time for cmdb
  cmdb <- subset(cmdb,use_time <= upperTime)
  cmdb$shiptimeToLeft <- as.numeric(difftime(lowerTime,cmdb$use_time,tz = 'UTC',units = 'days'))/365
  cmdb$shiptimeToRight <- as.numeric(difftime(upperTime,cmdb$use_time,tz = 'UTC',units = 'days'))/365
  
  cmdb$shTime <- floor(cmdb$shiptimeToRight)
  cmdb$shTimeN <- cut(cmdb$shiptimeToRight,c(0,1/2,1:7),include.lowest = T)
  cmdb$shTimeN <- gsub('^\\[|^\\(|,.*$','',cmdb$shTimeN)
  
  # S3. Label dev_class_id for each server
  cmdb$dClass <- ''
  class_C <- 'C1'
  class_B <- c('B5','B6','B1')
  class_TS <- c('TS1','TS3','TS4','TS5','TS6')
  cmdb$dClass[cmdb$dev_class_id %in% class_C] <- 'C'
  cmdb$dClass[cmdb$dev_class_id %in% class_B] <- 'B'
  cmdb$dClass[cmdb$dev_class_id %in% class_TS] <- 'TS'
  
  # S4 Label server with disk model
  cmdb <- mchAttr(cmdb,disk_ip,'svr_asset_id','svr_id',
                  c('numDisk','numModel','numMain','mainModel','capacity'))
  colcmdb <- c('svr_asset_id','ip','dev_class_id','bs1','use_time','shiptimeToLeft','dClass',
               'shiptimeToRight','shTime','numDisk','numModel','numMain','mainModel','capacity')
  cmdbio <- subset(cmdb,svr_asset_id %in% mean_io$svrid & 
                     dev_class_id %in% c(class_C,class_TS) & 
                     shiptimeToRight > 0 & 
                     !is.na(numDisk),colcmdb)
  modelNeed <- c('ST3500514NS','ST31000524NS','ST32000645NS',
                 'ST500NM0011','ST1000NM0011','ST2000NM0011')
  
  # add tag for disk including disk number and disk model
  cmdbio$tagDisk <- 'A'
  cmdbio$tagDisk[cmdbio$numDisk >= 6] <- 'B'
  cmdbio <- factorX(subset(cmdbio,mainModel %in% modelNeed))
  cmdbio$tagDisk <- paste(cmdbio$tagDisk,cmdbio$mainModel,sep='-')
  
  # filter capacity for C and revise capacity for TS
  # C
  cmdbio <- subset(cmdbio,!is.na(capacity) & (dClass != 'C' | capacity %in% c(500,250,1000)))
  # TS
  cmdbio$capacityMerge <- cmdbio$capacity
  cmdbio$capacityMerge[cmdbio$capacityMerge <= 18000] <- 12000
  cmdbio$capacityMerge[cmdbio$capacityMerge > 18000] <- 24000
  cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$capacityMerge == 12000] <- 'TS1T'
  cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$capacityMerge == 24000] <- 'TS2T'
  
  # S5. Add some attributes
  # CMDB
  tmp.cmdb <- factorX(cmdbio)
  
  # failure record
  tmp.f <- subset(data.f,svr_id %in% tmp.cmdb$svr_asset_id)
  tmp.f$ip <- factor(tmp.f$ip)
  tmp.f$svr_id <- factor(tmp.f$svr_id)
  tmp.f <- mchAttr(tmp.f,tmp.cmdb,
                   'svr_id','svr_asset_id',
                   c('capacity','use_time','shiptimeToLeft',
                     'shiptimeToRight','shTime','shTimeN','dClass','tagDisk'))
  tmp.f <- factorX(tmp.f)
  
  # IO statistic
  mean_io <- subset(mean_io,svrid %in% factor(cmdbio$svr_asset_id))
  tmp.io <- mean_io
  tmp.io <- mchAttr(tmp.io,cmdbio,'svrid','svr_asset_id',
                    c('dev_class_id','dClass','use_time','shiptimeToLeft',
                      'shiptimeToRight','shTime','shTimeN','ip','shiptimeToRight','tagDisk'))
  tmp.io <- factorX(tmp.io)
  
  # disk information
  tmp.disk <- disk_ip
  tmp.disk$dClass <- cmdbio$dClass[match(tmp.disk$ip,cmdbio$ip)]
  tmp.disk <- factorX(tmp.disk)
  
  # S5. Save
  save(tmp.cmdb,tmp.f,tmp.io,tmp.disk,cmdb,data.f,file = file.path(dir_data,saveName))
  # list(tmp.cmdb,tmp.f,tmp.io,tmp.disk,cmdb,data.f)
}

# All Data
dataPrepare(as.POSIXct('2010-01-01'),as.POSIXct('2013-10-01'),'dataPrepareAFR10-13.Rda')

# Full Year
dataPrepare(as.POSIXct('2013-01-01'),as.POSIXct('2013-10-01'),'dataPrepareAFR13.Rda')
dataPrepare(as.POSIXct('2014-01-01'),as.POSIXct('2015-01-01'),'dataPrepareAFR14.Rda')

# Half Year
dataPrepare(as.POSIXct('2013-01-01'),as.POSIXct('2013-07-01'),'dataPrepareAFR13A.Rda')
dataPrepare(as.POSIXct('2013-07-01'),as.POSIXct('2014-01-01'),'dataPrepareAFR13B.Rda')
dataPrepare(as.POSIXct('2014-01-01'),as.POSIXct('2014-07-01'),'dataPrepareAFR14A.Rda')
dataPrepare(as.POSIXct('2014-07-01'),as.POSIXct('2015-01-01'),'dataPrepareAFR14B.Rda')

# two month with io and smart
dataPrepare(as.POSIXct('2014-06-01'),as.POSIXct('2014-08-01'),'dataPrepareAFR1406_1407.Rda')

# two month
dataPrepare(as.POSIXct('2013-01-01'),as.POSIXct('2013-03-01'),'dataPrepareAFR1301_1302.Rda')
dataPrepare(as.POSIXct('2013-03-01'),as.POSIXct('2013-05-01'),'dataPrepareAFR1303_1304.Rda')
dataPrepare(as.POSIXct('2013-05-01'),as.POSIXct('2013-07-01'),'dataPrepareAFR1305_1306.Rda')
dataPrepare(as.POSIXct('2013-07-01'),as.POSIXct('2013-09-01'),'dataPrepareAFR1307_1308.Rda')
dataPrepare(as.POSIXct('2013-09-01'),as.POSIXct('2013-11-01'),'dataPrepareAFR1309_1310.Rda')

dataPrepare(as.POSIXct('2014-01-01'),as.POSIXct('2014-03-01'),'dataPrepareAFR1401_1402.Rda')
dataPrepare(as.POSIXct('2014-03-01'),as.POSIXct('2014-05-01'),'dataPrepareAFR1403_1404.Rda')
dataPrepare(as.POSIXct('2014-05-01'),as.POSIXct('2014-07-01'),'dataPrepareAFR1405_1406.Rda')
dataPrepare(as.POSIXct('2014-07-01'),as.POSIXct('2014-09-01'),'dataPrepareAFR1407_1408.Rda')
dataPrepare(as.POSIXct('2014-09-01'),as.POSIXct('2014-11-01'),'dataPrepareAFR1409_1410.Rda')
dataPrepare(as.POSIXct('2014-11-01'),as.POSIXct('2015-01-01'),'dataPrepareAFR1411_1412.Rda')

