 #!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: locate_fail.R
#
# Description: 
# 1.[Solved]Match server id with svrid
# We have two failure records(ykliu and ym). 
# ykliu have svrid and sn. ym does not contain svrid but a server id of integer instead.
# Objective of ykliu's databases is disk instead of server, which is the target of ym's database. 
# But what we are about to analyze needs the svrid to identify servers.
# Thus, we need to find the svrid for each item of ykliu's record. of them are contradictive.

# 2.[Unsolved]locate disk in ym's record
# The ym's record contains only failure of servers. But failure of disks is what we need.
# The ym's record contains a column called eventinfo which may describe the failed disk.
# We are going to extract the disk info from this column and determine whether we use the ym's record.

# 3.[20170414COMMENT]the variable ext_disk contains all parsed disk position from ym's failure record[201401-201412]. It's useful.

# Copyright (c) 2016, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2016-12-19 16:50:32
#
# Last   modified: 2017-01-13 11:29:20
#
#
#
rm(list = ls());setwd('/home/yiyusheng/Code/R/Disk_Workload_201406-201407/');source('~/rhead')
source('IO_statistic/iopsFunc.R')
dir_iops <- dir_data15
dir_smart <- '/home/yiyusheng/Data/SMART'
dir_ten <- "/home/yiyusheng/Data/Load_Data_2015"

load(file.path(dir_smart,'ykliu_smart.Rda'))
load(file.path(dir_smart,'diskInfo.Rda'))
load(file.path(dir_ten,'merge_id_svrid.Rda'))
load(file.path(dir_ten,'failRecord2014.Rda'))

fname <- list.files(dir_iops)

# S1.1. generate relation between sn and svrid for failed disk (smart from ykliu)
pos_disk <- factorX(subset(diskInfo,sn %in% smart$sn))

smart_dup <- smart[!duplicated(smart[,c('sn','failed_time')]),c('sn','failed_time')]
smart_dup <- list2df(tapply(smart_dup$failed_time,smart_dup$sn,max),n = c('failed_time','sn')) #only the last failure
smart_dup$failed_time <- as.POSIXct(smart_dup$failed_time,tz = 'UTC',origin = '1970-01-01')

pos_disk <- merge(pos_disk,smart_dup,all.y = T)
pos_disk$dev_id <- as.numeric(factor(gsub('sd','',pos_disk$device)))

# S1.2. failure record from ym2016-08
pos_disk$sid <- merge_id_svrid$sid[match(pos_disk$svrid,merge_id_svrid$svrid)]
pos_disk$f_time_new <- merge_id_svrid$f_time[match(pos_disk$svrid,merge_id_svrid$svrid)]
pos_disk <- factorX(subset(pos_disk,!is.na(sid)))

# S2.1 extract disk id in merge_id_svrid
ext_disk <- merge_id_svrid[,c('svrid','f_time','eventdescription')]
ext_disk$eventdescription <- gsub('SN: ','SN:',ext_disk$eventdescription)
ext_disk$eventdescription <- gsub('：',':',ext_disk$eventdescription)
ext_disk$eventdescription <- gsub('\\t|\\n',' ',ext_disk$eventdescription)
ext_disk$eventdescription <- gsub('快','块',ext_disk$eventdescription)

ext_disk$sn <- extract_reg('SN:\\w{8}\\s|SN:\\w{15}\\s|SN:\\w{18}\\s|SN:\\w{12}\\s',ext_disk$eventdescription)
ext_disk$logicID <- extract_reg('逻辑盘号:[0-9]{1,2}',ext_disk$eventdescription)
ext_disk$slotID <- extract_reg('槽位:\\w{1,2}\\s',ext_disk$eventdescription)
ext_disk$devID <- extract_reg('SD[a-zA-Z]|sd[a-zA-Z]',ext_disk$eventdescription)
ext_disk$shapeID <- extract_reg('#[0-9]{1,2}|[0-9]{1,2}#',ext_disk$eventdescription)
ext_disk$diskNum <- extract_reg('[0-9]号|[0-9]盘|[0-9]硬盘|[0-9]红灯|\\第.*块|\\硬盘位置.*',ext_disk$eventdescription)
ext_disk$partitionID <- extract_reg('disk[0-9]{1,2}|data[0-9]{1,2}',ext_disk$eventdescription)

save(pos_disk,ext_disk,file = file.path(dir_data,'locate_fail.Rda'))



# ext_disk$check <- apply(ext_disk[,4:(4+6)],1,function(x)sum(x != ''))
# ext_disk_del <- subset(ext_disk,check > 0 & 
#               !grepl('重启ping检查失败重启后无法ping通重启失败',eventdescription) & 
#               !grepl('agent已经5分钟没有上报数据',eventdescription)& 
#               !grepl('Ping.*不可达',eventdescription) & 
#               !grepl('TPC异常处理流程发起故障处理流程',eventdescription) & 
#               !grepl('重启失败',eventdescription))
# 
# table_column <- apply(ext_disk[,5:10],2,table)

