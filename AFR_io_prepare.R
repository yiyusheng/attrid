# data prepare for AFR_io
data.fFewDev <- subset(data.f,ip %in% cmdb$ip)
data.f <- subset(data.fAllDev,ip %in% cmdb$ip)
data.f$use_time <- cmdb$use_time[match(data.f$svr_id,cmdb$svr_asset_id)]
data.f$failShiptime <- floor(data.f$f_time - data.f$use_time)
units(data.f$failShiptime) <- 'days'
data.f$failShiptime <- as.numeric(data.f$failShiptime)/365

cmdb$shiptimeToLeft <- floor(as.POSIXct('2014-06-01') - cmdb$use_time)
cmdb$shiptimeToRight <- floor(as.POSIXct('2014-08-01') - cmdb$use_time)
units(cmdb$shiptimeToLeft) <- 'days'
units(cmdb$shiptimeToRight) <- 'days'
cmdb$shiptimeToLeft <- as.numeric(cmdb$shiptimeToLeft)/365
cmdbio <- subset(cmdb,svr_asset_id %in% mean_io$svrid)
# ggplot(cmdbio,aes(shiptimeToLeft))+geom_bar()
# 数据准备
tmp.cmdb <- cmdbio
tmp.f <- subset(data.f,svr_id %in% cmdbio$svr_asset_id)
tmp.f <- factorX(tmp.f)
tmp.io <- mean_io
tmp.io$dev_class_id <- cmdb$dev_class_id[match(tmp.io$svrid,cmdb$svr_asset_id)]
tmp.io$dClass
lastYears <- 7
# 给每个机器做标记
class_C <- 'C1'
class_B <- c('B5','B6','B1')
class_TS <- c('TS1','TS3','TS4','TS5','TS6')
cmdb$dClass <- ''
cmdb$dClass[cmdb$dev_class_id %in% class_C] <- 'C'
cmdb$dClass[cmdb$dev_class_id %in% class_B] <- 'B'
cmdb$dClass[cmdb$dev_class_id %in% class_TS] <- 'TS'
disk_ip$dClass <- cmdb$dClass[match(disk_ip$ip,cmdb$ip)]
tmp.io$dClass <- cmdb$dClass[match(tmp.io$svrid,cmdb$svr_asset_id)]
tmp.f$dClass <- cmdb$dClass[match(tmp.f$svr_id,cmdb$svr_asset_id)]