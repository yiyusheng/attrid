# S1. Failure record prepare
data.fAllDev <- subset(data.fMore,(f_time > as.POSIXct('2013-02-02') & f_time < as.POSIXct('2013-12-01')) | 
                         f_time > as.POSIXct('2014-01-01'))
data.f <- subset(data.fAllDev,ip %in% cmdb$ip)
data.f$use_time <- cmdb$use_time[match(data.f$svr_id,cmdb$svr_asset_id)]
data.f$failShiptime <- floor(data.f$f_time - data.f$use_time)
units(data.f$failShiptime) <- 'days'
data.f$failShiptime <- as.numeric(data.f$failShiptime)/365
data.f$fsTime <- floor(data.f$failShiptime)
data.f$fsTimeN <- cut(data.f$failShiptime,c(0,1/2,1:7),include.lowest = T)
data.f$fsTimeN <- gsub('^\\[|^\\(|,.*$','',data.f$fsTimeN)

# S2. Compute online time
cmdb$shiptimeToLeft <- floor(as.POSIXct('2014-06-01') - cmdb$use_time)
cmdb$shiptimeToRight <- floor(as.POSIXct('2014-08-01') - cmdb$use_time)
units(cmdb$shiptimeToLeft) <- 'days'
units(cmdb$shiptimeToRight) <- 'days'
cmdb$shiptimeToLeft <- as.numeric(cmdb$shiptimeToLeft)/365
cmdb$shiptimeToRight <- as.numeric(cmdb$shiptimeToRight)/365

cmdb$shTime <- floor(cmdb$shiptimeToLeft + (1/12))
cmdb$shTimeN <- cut(cmdb$shiptimeToLeft,c(0,1/2,1:7),include.lowest = T)
cmdb$shTimeN <- gsub('^\\[|^\\(|,.*$','',cmdb$shTimeN)


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
                   dev_class_id %in% c(class_C,class_TS))
cmdbio$total <- disk_ip$total[match(cmdbio$ip,disk_ip$ip)]
# C
cmdbio <- subset(cmdbio,!is.na(total) & (dClass != 'C' | total %in% c(500,250,1000)))
# TS
cmdbio$totalMerge <- cmdbio$total
cmdbio$totalMerge[cmdbio$totalMerge <= 18000] <- 12000
cmdbio$totalMerge[cmdbio$totalMerge > 18000] <- 24000
cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 12000] <- 'TS1T'
cmdbio$dClass[cmdbio$dClass == 'TS' & cmdbio$totalMerge == 24000] <- 'TS2T'
mean_io <- subset(mean_io,svrid %in% factor(cmdbio$svr_asset_id))

# S4. data preparation
tmp.cmdb <- cmdbio

tmp.f <- subset(data.f,svr_id %in% cmdbio$svr_asset_id)
tmp.f$total <- tmp.cmdb$total[match(tmp.f$ip,tmp.cmdb$ip)]
tmp.f$shTime <- tmp.cmdb$shTime[match(tmp.f$ip,tmp.cmdb$ip)]
tmp.f$shTimeN <- tmp.cmdb$shTimeN[match(tmp.f$ip,tmp.cmdb$ip)]
tmp.f <- factorX(tmp.f)

tmp.io <- mean_io
tmp.io$dev_class_id <- cmdb$dev_class_id[match(tmp.io$svrid,cmdb$svr_asset_id)]

# Add more
disk_ip$dClass <- cmdbio$dClass[match(disk_ip$ip,cmdbio$ip)]
tmp.io$dClass <- cmdbio$dClass[match(tmp.io$svrid,cmdbio$svr_asset_id)]
tmp.f$dClass <- cmdbio$dClass[match(tmp.f$svr_id,cmdbio$svr_asset_id)]
tmp.io$shTime <- cmdbio$shTime[match(tmp.io$svrid,cmdbio$svr_asset_id)]
tmp.io$shTimeN <- cmdbio$shTimeN[match(tmp.io$svrid,cmdbio$svr_asset_id)]
tmp.io$ip <- cmdbio$ip[match(tmp.io$svrid,cmdbio$svr_asset_id)]
