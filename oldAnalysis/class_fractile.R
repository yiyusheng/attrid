# IO特征: 分位点 + 高中低分类
rm(list = ls())
dir_code <- 'D:/Git/attrid'
dir_data <- 'D:/Data/attrid'
dir_dataA <- 'D:/Data/Disk Number'
source('D:/Git/R_Function/Rfun.R')
require('ggplot2')
require('xlsx')

# feature function
ftr_quan <- function(data){
  data <- factorX(data)
  point.quan <- c(1/3,2/3)
  point.data <- as.numeric(quantile(data$mean,point.quan))
  data$low <- data$mean <= point.data[1]
  data$mid <- data$mean <= point.data[2] & data$mean > point.data[1]
  data$high <- data$mean > point.data[2]
  ftr.data <- data.frame(svrid = levels(data$svrid),
                        low = as.numeric(tapply(data$low,data$svrid,sum)),
                        mid = as.numeric(tapply(data$mid,data$svrid,sum)),
                        high = as.numeric(tapply(data$high,data$svrid,sum)))
  row.names(ftr.data) <- NULL
  return(list(ftr.data,point.data))
}

#@@@ LOAD DATA
# 1. cmdb数据
load(file.path(dir_dataA,'disk_number_label.Rda'))
load(file.path(dir_dataA,'mcf_all_age_rsv2014.Rda'))
dev_need <- c('TS4','TS5','TS6','C1')
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
# data.config <- subset(data.config,dev_class_id %in% dev_need)
data.config$dev_class_id <- factor(data.config$dev_class_id)

# 2. 读取k131-9**
k131_902 <- read.csv(file.path(dir_data,'k131_902'))
k131_903 <- read.csv(file.path(dir_data,'k131_903'))
k131_999 <- read.csv(file.path(dir_data,'k131_999'))
k131_902$dev_class_id <- cmdb$dev_class_id[match(k131_902$svrid,cmdb$svr_asset_id)]
k131_903$dev_class_id <- cmdb$dev_class_id[match(k131_903$svrid,cmdb$svr_asset_id)]
k131_999$dev_class_id <- cmdb$dev_class_id[match(k131_999$svrid,cmdb$svr_asset_id)]

# 3. 求所有数据点的mean值的分位点
dev_need <- c('C1','TS1','TS3','TS4','TS5','TS6')
ftr.902 <- ftr_quan(subset(k131_902,dev_class_id %in% dev_need))
ftr.903 <- ftr_quan(subset(k131_903,dev_class_id %in% dev_need))
ftr.999 <- ftr_quan(subset(k131_999,dev_class_id %in% dev_need))
point.data <- data.frame(rbind(ftr.902[[2]],ftr.903[[2]],ftr.999[[2]]))
names(point.data) <- c('Low-Mid','Mid-High')
row.names(point.data) <- c(902,903,999)
ftr.902 <- ftr.902[[1]]
ftr.903 <- ftr.903[[1]]
ftr.999 <- ftr.999[[1]]
ftr.all <- merge(ftr.902,ftr.903,by = 'svrid')
ftr.all <- merge(ftr.all,ftr.999,by = 'svrid')
names(ftr.all) <- c('svrid','low_902','mid_902','high_902',
                    'low_903','mid_903','high_903',
                    'low_999','mid_999','high_999')
ftr.all$dev_class_id <- cmdb$dev_class_id[match(ftr.all$svrid,cmdb$svr_asset_id)]
ftr.all$bs1 <- cmdb$bs1[match(ftr.all$svrid,cmdb$svr_asset_id)]
ftr.all$db <- paste(ftr.all$dev_class_id,ftr.all$bs1,sep='_')

# 4. 聚类
ftr.data <- ftr.all
se <- seq(3,20,1)
rs <- rep(0,length(se))
cl <- rep(0,nrow(ftr.all))
km <- list()
for (i in 1:length(se)){
  km[i] <- list(kmeans(ftr.data[,2:10],centers = se[i], iter.max = 100,nstart = 100))
  rs[i] <- km[[i]]$tot.withinss/km[[i]]$totss
  cl <- cbind(cl,km[[i]]$cluster)
}

# 5. 求各聚类之间的条件熵
cl <- cl[,2:ncol(cl)]
ce <- list()
for (i in 1:(ncol(cl)-1)){
  tmp <- entropy(cl[,i],cl[,i+1])
  ce[i] <- list(tmp[2])
}

# 6.总结
re <- list()
s <- list()
s1 <- list()
for (i in 1:length(km)){
  tmp1 <- colSums(km[[i]]$centers > 50)
  tmp2 <- c(sum(tmp1[1:3]),sum(tmp1[4:6]),sum(tmp1[7:9]))
  
  tmp3 <- rowSums(km[[i]]$centers > 50)
  
  tmp <- data.frame(cbind(km[[i]]$centers,km[[i]]$size,km[[i]]$withinss,km[[i]]$withinss/km[[i]]$size))
  names(tmp) <- c(colnames(km[[i]]$centers),'size','within','mean dist')
#   tmp <- tmp[order(tmp$within),]
  re[i] <- list(tmp)
  s[i] <- list(tmp2)
  s1[i] <- list(tmp3)
}









# all <- data.frame(matrix(unlist(re),ncol = ncol(re[[1]]),byrow = T))
# cbind(km[[1]]$centers,km[[1]]$size,km[[1]]$withinss)
# cbind(km[[2]]$centers,km[[2]]$size,km[[2]]$withinss)
# cbind(km[[3]]$centers,km[[3]]$size,km[[3]]$withinss)
# rs <- data.frame(seq = se,rsquared = rs)
# ggplot(rs,aes(x = seq,y = rsquared)) + geom_line()
# km <- kmeans(ftr.all[,2:ncol(ftr.all)],centers = 8, iter.max = 100, nstart = 100);
# cbind(km$centers,km$size,km$withinss,km$withinss/km$size)

