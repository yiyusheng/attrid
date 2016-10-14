# 统计IO数据
rm(list = ls())
dir_code <- 'D:/Git/attrid'
dir_data <- 'D:/Data/attrid/sta_csv'
dir_data1 <- 'D:/Data/Disk Number'
require('ggplot2')
source('D:/Git/R_Function/Rfun.R')

# 1. 读取故障单
load(file.path(dir_data1,'data_mcf_rsv2014.Rda'))
load(file.path(dir_data1,'disk_number_label.Rda'))
data.mcf$dup <- !duplicated(data.mcf$ip)
flist <- subset(data.mcf, f_time < as.POSIXct('2014-08-01') & f_time >= as.POSIXct('2014-06-01'))

# 2. 读取attrid并统计
# attr_name <- c(26,40,41,50,51,9,902,9020,903,905,907,927,928,929,930,999)
attr_name <- 9020
attr_name <- paste('attr',attr_name,sep='')

sta_attr <- list()
for (x in attr_name) {
  attr <- read.csv(file.path(dir_data,paste(x,'.csv',sep='')))
  if (is.numeric(attr$min_time)) {
    attr$min_time <- as.Date(as.character(attr$min_time),format = '%Y%m%d')
    attr$max_time <- as.Date(as.character(attr$max_time),format = '%Y%m%d')
  }else{
    attr$min_time <- as.Date(attr$min_time)
    attr$max_time <- as.Date(attr$max_time)
  }
  sta_attr[[x]] <- attr
}

# 2.5 对所有902数据进行分块
attr <- attr[order(attr$svrid),]
part_svrid <- attr$svrid[seq(1,nrow(attr),5000)]
write.table(part_svrid,file = file.path(dir_data,'part_svrid'),quote = F,row.names = F,col.names = F)

# 3. 求故障单与attrid的交
its <- matrix(0,2,length(attr_name))
names(its) <- attr_name
for (i in 1:length(attr_name)) {
  x <- attr_name[i]
  its[1,i] <- nrow(sta_attr[[x]])
  its[2,i] <- length(intersect(sta_attr[[x]]$svrid,flist$svr_id))
}
its_svr <- intersect(sta_attr[['attr9020']]$svrid,flist$svr_id)
dif_svr <- setdiff(sta_attr[['attr9020']]$svrid,its_svr)
col_need <- c('svr_asset_id','dev_class_id','type_name','model_name','ip','dept_id','bs1',
              'raid','use_time','svr_version','idc_parent_name')
dev_need <- c('C1','A3','TS1','TS3','TS4','TS5','TS6')
cmdb_its <- subset(cmdb,svr_asset_id %in% its_svr & dev_class_id %in% dev_need,col_need)
cmdb_dif <- subset(cmdb,svr_asset_id %in% dif_svr & dev_class_id %in% dev_need,col_need)
cmdb_all <- subset(cmdb,dev_class_id %in% dev_need,col_need)
tmp <- table(factor(cmdb_its$dev_class_id))
sta_dev <- data.frame(dev_class_id = names(tmp),
                      inter = as.numeric(tmp),
                      diff = as.numeric(table(factor(cmdb_dif$dev_class_id))),
                      all = as.numeric(table(factor(cmdb_all$dev_class_id))))
sta_dev$diff_disk <- sta_dev$diff
sta_dev$all_disk <- sta_dev$all
sta_dev$diff_disk[sta_dev$dev_class_id != 'C1'] <- sta_dev$diff[sta_dev$dev_class_id != 'C1']*12
sta_dev$all_disk[sta_dev$dev_class_id != 'C1'] <- sta_dev$all[sta_dev$dev_class_id != 'C1']*12

# 4. 指定机型中每个机型选取3000台无故障机
num_need <- 1000
good_list <- list()
for (d in dev_need){
  tmp <- cmdb_dif$svr_asset_id[cmdb_dif$dev_class_id == d]
  len <- length(tmp)
  good_list[[d]] <- tmp[ceiling(runif(min(num_need,len),1,len))]
}
good_svr <- as.character(unlist(good_list))
write.csv(good_svr,file = file.path(dir_data,'good_svrid.csv'),row.names = F)

# 5. 所有数据中每个机型选择10台故障机,10台无故障机看数据
bad_svrid <- tapply(cmdb_its$svr_asset_id,factor(cmdb_its$dev_class_id),function(x)x[round(runif(10,1,length(x)))])
bad_svrid <- data.frame(svrid = as.character(unlist(bad_svrid)))
bad_svrid$dev_class_id <- cmdb$dev_class_id[match(bad_svrid$svrid,cmdb$svr_asset_id)]
bad_svrid$class <- 'bad'

good_svrid <- tapply(cmdb_dif$svr_asset_id,factor(cmdb_dif$dev_class_id),function(x)x[round(runif(10,1,length(x)))])
good_svrid <- data.frame(svrid = as.character(unlist(good_svrid)))
good_svrid$dev_class_id <- cmdb$dev_class_id[match(good_svrid$svrid,cmdb$svr_asset_id)]
good_svrid$class <- 'good'

attr_svrid <- rbind(bad_svrid,good_svrid)
row.names(attr_svrid) <- NULL
attr_svrid <- attr_svrid[!duplicated(attr_svrid$svrid),]
write.csv(attr_svrid,file = file.path(dir_data,'attr_svrid.csv'),row.names = F)

# 6. 提取所有故障机svrid
tmp <- as.character(cmdb_its$svr_asset_id)
write.csv(tmp,file = file.path(dir_data,'bad_svrid.csv'),row.names = F)

# 7. 对取出的无故障机进行处理
tmp <- read.csv(file.path(dir_data,'attr_902_bad'))
names(tmp) <- c('date','svrid','attrid','timepoint','value')
tmp1 <- table(tmp$svrid)
tmp1 <- data.frame(svrid = names(tmp1),
                   count = as.numeric(tmp1))


