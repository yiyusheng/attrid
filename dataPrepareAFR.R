# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
load(file.path(dir_dataSource,'load_ftr_attrid.Rda'))
useOld <- 1

if (useOld == 1){
  lowerTime <- as.POSIXct('2013-01-01')
  upperTime <- as.POSIXct('2013-10-01')
  saveName <- 'dataPrepareAFR13.Rda'
}else{
  lowerTime <- as.POSIXct('2014-06-01')
  upperTime <- as.POSIXct('2014-08-01')
  saveName <- 'dataPrepareAFR1406_1407.Rda'
}

# S1. Failure record prepare

# a wrong failure record makes original result wrong
# data.f <- subset(data.fMore,(f_time > as.POSIXct('2013-02-02') & f_time < as.POSIXct('2013-12-01')) |
#                    f_time > as.POSIXct('2014-01-01')) 

data.f <- subset(data.fMore, f_time < upperTime & f_time > lowerTime)
data.f <- subset(data.f,ip %in% cmdb$ip)
data.f$use_time <- cmdb$use_time[match(data.f$svr_id,cmdb$svr_asset_id)]
data.f$failShiptime <- floor(data.f$f_time - data.f$use_time)
units(data.f$failShiptime) <- 'days'
data.f$failShiptime <- as.numeric(data.f$failShiptime)/365
data.f$fsTime <- floor(data.f$failShiptime)
data.f$fsTimeN <- cut(data.f$failShiptime,c(0,1/2,1:7),include.lowest = T)
data.f$fsTimeN <- gsub('^\\[|^\\(|,.*$','',data.f$fsTimeN)

# S2. Compute online time
cmdb$shiptimeToLeft <- floor(lowerTime - cmdb$use_time)
cmdb$shiptimeToRight <- floor(upperTime - cmdb$use_time)
units(cmdb$shiptimeToLeft) <- 'days'
units(cmdb$shiptimeToRight) <- 'days'
cmdb$shiptimeToLeft <- as.numeric(cmdb$shiptimeToLeft)/365
cmdb$shiptimeToRight <- as.numeric(cmdb$shiptimeToRight)/365

# a wrong setting
# cmdb$shTime <- floor(cmdb$shiptimeToLeft + (1/12))

cmdb$shTime <- floor(cmdb$shiptimeToRight)
cmdb$shTimeN <- cut(cmdb$shiptimeToRight,c(0,1/2,1:7),include.lowest = T)
cmdb$shTimeN <- gsub('^\\[|^\\(|,.*$','',cmdb$shTimeN)
cmdb$diskNum <- disk_ip$disk_c[match(cmdb$ip,disk_ip$ip)]

# S3. Label for each server
cmdb$dClass <- ''
class_C <- 'C1'
class_B <- c('B5','B6','B1')
class_TS <- c('TS1','TS3','TS4','TS5','TS6')
cmdb$dClass[cmdb$dev_class_id %in% class_C] <- 'C'
cmdb$dClass[cmdb$dev_class_id %in% class_B] <- 'B'
cmdb$dClass[cmdb$dev_class_id %in% class_TS] <- 'TS'

# S4. Add capacity and filter data without capacity information
# Filter detail: C1 588, TS3 7, TS6 35
# Containing 13 failed server
cmdbio <- subset(cmdb,svr_asset_id %in% mean_io$svrid & 
                   dev_class_id %in% c(class_C,class_TS) &
                   shiptimeToRight > 0 & !is.na(diskNum))
cmdbio$total <- disk_ip$total[match(cmdbio$ip,disk_ip$ip)]
# C
cmdbio <- subset(cmdbio,!is.na(total) & (dClass != 'C' | total %in% c(500,250,1000)))
# TS
cmdbio$totalMerge <- cmdbio$total
cmdbio$totalMerge[cmdbio$totalMerge <= 18000] <- 12000
cmdbio$totalMerge[cmdbio$totalMerge > 18000] <- 24000
cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 12000] <- 'TS1T'
cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 24000] <- 'TS2T'


# S4. data preparation
# CMDB
tmp.cmdb <- cmdbio

# failure record
tmp.f <- subset(data.f,svr_id %in% tmp.cmdb$svr_asset_id)
tmp.f$total <- tmp.cmdb$total[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]
tmp.f$shTime <- tmp.cmdb$shTime[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]
tmp.f$shTimeN <- tmp.cmdb$shTimeN[match(tmp.f$svr_id,tmp.cmdb$svr_asset_id)]
tmp.f$dClass <- cmdbio$dClass[match(tmp.f$svr_id,cmdbio$svr_asset_id)]
tmp.f$ip <- factor(tmp.f$ip)
tmp.f$svr_id <- factor(tmp.f$svr_id)
tmp.f <- factorX(tmp.f)

# IO statistic
mean_io <- subset(mean_io,svrid %in% factor(cmdbio$svr_asset_id))
tmp.io <- mean_io
tmp.io$dev_class_id <- cmdb$dev_class_id[match(tmp.io$svrid,cmdb$svr_asset_id)]
tmp.io$dClass <- cmdbio$dClass[match(tmp.io$svrid,cmdbio$svr_asset_id)]
tmp.io$shTime <- cmdbio$shTime[match(tmp.io$svrid,cmdbio$svr_asset_id)]
tmp.io$shTimeN <- cmdbio$shTimeN[match(tmp.io$svrid,cmdbio$svr_asset_id)]
tmp.io$ip <- cmdbio$ip[match(tmp.io$svrid,cmdbio$svr_asset_id)]

# disk information
tmp.disk <- disk_ip
tmp.disk$dClass <- cmdbio$dClass[match(tmp.disk$ip,cmdbio$ip)]
tmp.disk$svrid <- cmdbio$svr_asset_id[match(tmp.disk$ip,cmdbio$ip)]

# S5. Save
save(tmp.cmdb,tmp.f,tmp.io,tmp.disk,cmdb,data.f,file = file.path(dir_data,saveName))