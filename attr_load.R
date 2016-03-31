# LOAD DATA
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
# L1. For ftr_attrid.R
# L1.1 cmdb数据
load(file.path(dir_dataA,'disk_number_label.Rda'))
load(file.path(dir_dataA,'mcf_all_age_rsv2014.Rda'))
dev_need <- c('TS4','TS5','TS6','C1')
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
# data.config <- subset(data.config,dev_class_id %in% dev_need)
data.config$dev_class_id <- factor(data.config$dev_class_id)

# L1.2 读取k131-9**,取提取故障数据
data.flist$dev_class_id <- cmdb$dev_class_id[match(data.flist$svr_id,cmdb$svr_asset_id)]
data.f <- subset(data.flist,f_time > as.POSIXct('2014/06/01') & 
                   f_time < as.POSIXct('2014/08/01') &
                   dev_class_id %in% c('C1','TS1','TS3','TS4','TS5','TS6'))
data.fMore <- subset(data.flist,f_time > as.POSIXct('2013/01/01') & 
                   f_time < as.POSIXct('2014/08/01'))
data.fAllDev <- subset(data.flist,f_time > as.POSIXct('2014/06/01') & 
                         f_time < as.POSIXct('2014/08/01'))
load(file = file.path(dir_data,'attr.Rda'))
k131 <- subset(cmdb,svr_asset_id %in% k131_902$svrid)


# L1.3 硬盘用量与强度: 求每台机器902.mean,903.mean,999.mean的均值,均值/使用时间百分比得强度.
# tmp.902 <- k131_902
# tmp.903 <- k131_903
# tmp.999 <- k131_999
# tmp1 <- data.frame(svrid = levels(tmp.902$svrid),
#                    mean = as.numeric(tapply(tmp.902$mean,tmp.902$svrid,mean)))
# tmp2 <- data.frame(svrid = levels(tmp.903$svrid),
#                    mean = as.numeric(tapply(tmp.903$mean,tmp.903$svrid,mean)))
# tmp3 <- data.frame(svrid = levels(tmp.999$svrid),
#                    mean = as.numeric(tapply(tmp.999$mean,tmp.999$svrid,mean)))
# mean_io <- tmp1
# mean_io$mean_903 <- tmp2$mean[match(mean_io$svrid,tmp2$svrid)]
# mean_io$mean_999 <- tmp3$mean[match(mean_io$svrid,tmp3$svrid)]
# mean_io <- subset(mean_io,svrid %in% tmp.999$svrid)
# names(mean_io) <- c('svrid','mean_902','mean_903','mean_999')
# # mean_io$use_perc <- 100 - use_999$use_perc[match(use_999$svrid,mean_io$svrid)]
# mean_io$class <- 'Normal'
# mean_io$class[mean_io$svrid %in% data.f$svr_id] <- 'Failure'
# mean_io$size <- 1
# mean_io$size[mean_io$class == 'Failure'] <- 10
# save(mean_io,file = file.path(dir_data,'mean_io.Rda'))
load(file = file.path(dir_data,'mean_io.Rda'))

# L1.4 处理disk_ip,生成sata2/sata3数量及有model数小于3时的model及数量，model已排序。
# disk_ip$SATA2 <- 0
# disk_ip$SATA3 <- 0
# disk_ip$modelA <- ''
# disk_ip$modelB <- ''
# disk_ip$modelAC <- 0
# disk_ip$modelBC <- 0
# 
# cur_inter <- strsplit(as.character(disk_ip$disk_inter),'_')
# cur_num <- strsplit(as.character(disk_ip$disk_model_c),'_')
# cur_model <- strsplit(as.character(disk_ip$disk_model),'_')
# 
# for (i in seq(1,nrow(disk_ip))){
#   print(i)
#   if (disk_ip$disk_model_c1[i] == 1){
#     disk_ip[[cur_inter[[i]]]][i] <- disk_ip$disk_c[i]
#     disk_ip$modelA[i] <- cur_model[[i]][1]
#     disk_ip$modelAC[i] <- disk_ip$disk_c[i]
#   }else{
#     for (j in seq(1,length(cur_num[[i]]))){
#       disk_ip[[cur_inter[[i]][j]]][i] <- disk_ip[[cur_inter[[i]][j]]][i] + as.numeric(cur_num[[i]][j])
#       if (disk_ip$disk_model_c1[i] == 2){
#         disk_ip$modelA[i] <- cur_model[[i]][1]
#         disk_ip$modelB[i] <- cur_model[[i]][2]
#         disk_ip$modelAC[i] <- cur_num[[i]][1]
#         disk_ip$modelBC[i] <- cur_num[[i]][2]
#       }
#     }
#   }
# }
save('disk_ip',file = file.path(dir_data,'disk_ip.Rda'))
load(file = file.path(dir_data,'disk_ip.Rda'))

# L1.x SAVE
save(mean_io,cmdb,data.f,data.fMore,data.fAllDev,disk_ip,file = file.path(dir_data,'load_ftr_attrid.Rda'))
################################################################################################################
# L2 for ftr_cpuUse.R
# k131_902 <- read.csv(file.path(dir_data,'attr_902'))
# k131_903 <- read.csv(file.path(dir_data,'attr_903'))
# k131_999 <- read.csv(file.path(dir_data,'attr_999'))
# k131_902$date <- as.Date(k131_902$date)
# k131_903$date <- as.Date(k131_903$date)
# k131_999$date <- as.Date(k131_999$date)
# k131_902more <- read.csv(file.path(dir_data,'attr_902more'))
# k131_903more <- read.csv(file.path(dir_data,'attr_903more'))
# k131_999more <- read.csv(file.path(dir_data,'attr_999more'))
# k131_902more$date <- as.Date(k131_902more$date)
# k131_903more$date <- as.Date(k131_903more$date)
# k131_999more$date <- as.Date(k131_999more$date)
# k131_902more <- k131_902more[!duplicated(k131_902more[,c('svrid','date')]),]
# k131_903more <- k131_903more[!duplicated(k131_903more[,c('svrid','date')]),]
# k131_999more <- k131_999more[!duplicated(k131_999more[,c('svrid','date')]),]
# k131_902 <- rbind(k131_902,k131_902more)
# k131_903 <- rbind(k131_903,k131_903more)
# k131_999 <- rbind(k131_999,k131_999more)
# k131_svrid <- intersect(k131_902$svrid,k131_903$svrid)
# k131_svrid <- intersect(k131_svrid,k131_999$svrid)
# sta_999 <- data.frame(svrid = levels(k131_999$svrid),
#                       count = as.numeric(tapply(k131_999$svrid,k131_999$svrid,length)))
# sta_999$class <- 'Normal'
# sta_999$class[k131$svr_asset_id %in% data.f$svr_id] <- 'Failure'
# save(k131_902,k131_903,k131_999,k131_svrid,sta_999,file = file.path(dir_data,'attr.Rda'))