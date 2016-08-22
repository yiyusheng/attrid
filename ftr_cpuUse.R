# 求IO空闲时间特征与故障的关系
rm(list = ls())
dir_code <- 'D:/Git/attrid'
dir_data <- 'D:/Data/attrid'
dir_dataA <- 'D:/Data/Disk Number'
source('D:/Git/R_Function/Rfun.R')
require('ggplot2')
require('xlsx')
require('scales')

#@@@ Function
plotDevStime <- function(k131,suffix){
  k131$dc <- paste(k131$dev_class_id,k131$class,sep='_')
  p <- ggplot(subset(k131,!is.na(ut_part)),aes(x = dc)) +
    geom_bar(aes(fill = ut_part),position = 'fill') +
    ggtitle(paste('Ship Date and Device Class_',suffix,sep='')) + 
    xlab('Device Class') + ylab('Percentage of Ship Year') +
    guides(fill = guide_legend(title=NULL),shape = guide_legend(title=NULL)) + 
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0.8,0.7),
          legend.justification = c(0,0),
          legend.background = element_rect(fill = alpha('grey',0.8)),
          axis.text.x = element_text(angle = 30, hjust = 1))
  print(p)
  ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('上架时间与机型_',suffix,'.png',sep='')),
         width = 12, height = 9, dpi = 100)
}

frate <- function(data){
  tmp <- data.frame(perc = as.numeric(levels(data$use_perc_std)),
                    rate = tapply(data$class,data$use_perc_std,function(x)sum(x == 'Failure')/length(x)))
  return(tmp)
}

frate_3 <- function(data){
  tmp <- data.frame(perc = levels(data$use_perc_3),
                    rate = tapply(data$class,data$use_perc_3,function(x)sum(x == 'Failure')/length(x)))
  return(tmp)
}

# 1. LOAD cmdb数据
load(file.path(dir_dataA,'disk_number_label.Rda'))
load(file.path(dir_dataA,'mcf_all_age_rsv2014.Rda'))
cmdb_simple <- cmdb[,c('svr_asset_id','ip','dev_class_id','bs1','use_time')]
dev_need <- c('TS3','TS4','TS5','TS6','C1')
data.config$bs1 <- cmdb_dev$bs1[match(data.config$ip,cmdb_dev$ip)]
data.config$dev_class_id <- factor(data.config$dev_class_id)
data.config <- subset(data.config,use_time > as.POSIXct('2010-01-01'))
data.config <- subset(data.config,dev_class_id %in% dev_need)
data.flist$dev_class_id <- cmdb$dev_class_id[match(data.flist$svr_id,cmdb$svr_asset_id)]
data.f <- subset(data.flist,f_time > as.POSIXct('2014/06/01') & 
                   f_time < as.POSIXct('2014/08/01') &
                   dev_class_id %in% dev_need)

# 2. 读取k131-9**,取提取故障数据,存储
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

# 2.1 读取并过滤数据
load(file = file.path(dir_data,'attr.Rda'))
k131_999 <- subset(k131_999,svrid %in% sta_999$svrid[sta_999$count == 60])
k131 <- subset(cmdb,dev_class_id %in% dev_need & 
                 svr_asset_id %in% k131_svrid & 
                 svr_asset_id %in% k131_999$svrid)

# 2.2 机型,故障,上架时间的关系图统计
k131$class <- 'Normal'
k131$class[k131$svr_asset_id %in% data.f$svr_id] <- 'Failure'
se <- seq(as.Date('2009-01-01'),as.Date('2015-01-01'),'1 year')
k131$ut_part <- cut(as.Date(k131$use_time),se)
k131$fsize <- 1
k131$fsize[k131$class == 'Failure'] <- 2
k131$ut_part <- format(as.Date(k131$ut_part),'%Y')
k131 <- subset(k131,!is.na(ut_part))
k131 <- factorX(k131)
plotDevStime(k131,'All')

# 3. IO使用特征: 求每台机器每天999分位点为0的百分比.并求平均
# tmp <- k131_999
# cn <- paste('p',seq(10,100,10),sep='')
# tmp$use_perc <- apply(tmp[,cn],1,function(x)sum(x == 0))
# tmp1 <- data.frame(svrid = levels(tmp$svrid),
#                    use_perc = as.numeric(tapply(tmp$use_perc,tmp$svrid,mean))*10)
# tmp1$class <- 'Normal'
# tmp1$class[tmp1$svrid %in% data.f$svr_id] <- 'Failure'
# tmp1$use_time <- k131$use_time[match(tmp1$svrid,k131$svr_asset_id)]
# tmp1$fsize <- k131$fsize[match(tmp1$svrid,k131$svr_asset_id)]
# tmp1$ut_part <- k131$ut_part[match(tmp1$svrid,k131$svr_asset_id)]
# tmp1$dev_class_id <- k131$dev_class_id[match(tmp1$svrid,k131$svr_asset_id)]
# tmp1 <- subset(tmp1,!is.na(fsize))
# row.names(tmp1) <- NULL
# use_999 <- tmp1
# save(use_999,sta_999,file = file.path(dir_data,'use_999.Rda'))
load(file = file.path(dir_data,'use_999.Rda'))

p <- ggplot(subset(use_999,use_perc != -1)) + 
  geom_histogram(aes(x = use_perc,fill = class),binwidth = 5) + 
#   ylim(c(0,1000)) + 
  ggtitle('Idle Time of Disk') + xlab('Percentage of Time with Idel Disk(%)') + ylab('Number of Servers') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.8,0.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘空闲时间统计.png'),
       width = 12, height = 9, dpi = 100)

# 3.1 空闲时间与故障率
count_split <- 10
tmp2 <- floor(use_999$use_perc/count_split)*count_split
use_999$use_perc_std <- factor(tmp2,levels = sort(unique(tmp2)))
tmp2A <- subset(use_999)
tmp3 <- data.frame(perc = as.numeric(levels(tmp2A$use_perc_std)),
                   rate = tapply(tmp2A$class,tmp2A$use_perc_std,function(x)sum(x == 'Failure')/length(x)))
p <- ggplot(subset(tmp3),aes(x = factor(perc),y = rate*100)) + 
  geom_bar(stat = 'identity',position = 'dodge') + 
  #   ylim(c(0,1000)) + 
  ggtitle(paste('Idle Time and Failure Rate')) + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.8,0.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与故障率.png',sep = '')),
       width = 12, height = 9, dpi = 100)

# # 3.2 空闲时间对故障的影响,TS类
# count_split <- 10
# tmp2 <- floor(use_999$use_perc/count_split)*count_split
# use_999$use_perc_std <- factor(tmp2,levels = sort(unique(tmp2)))
# d <- 'TS'
# tmp2A <- subset(use_999,dev_class_id != 'C1' & ut_part != '2009')
# # tableX(tmp2A$use_perc_std)
# tmp3 <- data.frame(perc = as.numeric(levels(tmp2A$use_perc_std)),
#                    rate = tapply(tmp2A$class,tmp2A$use_perc_std,function(x)sum(x == 'Failure')/length(x)))
# p <- ggplot(subset(tmp3),aes(x = factor(perc),y = rate*100)) + 
#   geom_bar(stat = 'identity',position = 'dodge') + 
#   #   ylim(c(0,1000)) + 
#   ggtitle(paste('Idle Time and Failure Rate-','TS',sep='')) + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.8),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与故障率-',d,'.png',sep = '')),
#        width = 12, height = 9, dpi = 100)
# 
# # 3.3 空闲时间对故障的影响,C类
# count_split <- 10
# tmp2 <- floor(use_999$use_perc/count_split)*count_split
# use_999$use_perc_std <- factor(tmp2,levels = sort(unique(tmp2)))
# d <- 'C'
# tmp2A <- subset(use_999,dev_class_id == 'C1')
# tmp3 <- data.frame(perc = as.numeric(levels(tmp2A$use_perc_std)),
#                    rate = tapply(tmp2A$class,tmp2A$use_perc_std,function(x)sum(x == 'Failure')/length(x)))
# p <- ggplot(subset(tmp3),aes(x = factor(perc),y = rate*100)) + 
#   geom_bar(stat = 'identity',position = 'dodge') + 
#   #   ylim(c(0,1000)) + 
#   ggtitle(paste('Idle Time and Failure Rate-',d,sep='')) + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.8),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与故障率-',d,'.png',sep = '')),
#        width = 12, height = 9, dpi = 100)

# 3.4 分段空闲时间对故障的影响,C类,加入上架时间
se <- c(0,30,80,100.1)
tmp34 <- subset(use_999,dev_class_id == 'C1')
tmp34$use_perc_3 <- cut(tmp34$use_perc,se,right = F)
# tapply(tmp34$ut_part,tmp34$use_perc_3,tableX)
# tapply(tmp34$ut_part,tmp34$use_perc_std,tableX)
uni <- unique(tmp34$ut_part)
tmp34A <- data.frame()
for (i in 1:length(uni)){
  tmp <- frate(subset(tmp34,ut_part == uni[i]))
  tmp$class <- uni[i]
  tmp34A <- rbind(tmp34A,tmp)
}
tmp34A$rate[is.na(tmp34A$rate)] <- 0
d <- 'C'
p <- ggplot(subset(tmp34A),aes(x = factor(perc),y = rate*100)) + 
  geom_bar(aes(fill = class), stat = 'identity',position = 'dodge') + 
    ylim(c(0,10)) + 
  ggtitle(paste('Idle Time and Ship Time-',d,sep='')) + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.8,0.6),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与上架时间-',d,'.png',sep = '')),
       width = 12, height = 9, dpi = 100)

# 3.5 分段空闲时间对故障的影响,TS类,加入上架时间
se <- c(0,50,70,100.1)
tmp35 <- subset(use_999,dev_class_id != 'C1')
tmp35$use_perc_3 <- cut(tmp35$use_perc,se,right = F)
tmp35 <- subset(tmp35,ut_part != '2009')
# tmp35 <- subset(tmp35,!(use_perc_3 != '[0,50)' & ut_part == 2010))
# tapply(tmp35$ut_part,tmp35$use_perc_std,tableX)
uni <- unique(tmp35$ut_part)
tmp35A <- data.frame()
for (i in 1:length(uni)){
  tmp <- frate(subset(tmp35,ut_part == uni[i]))
  tmp$class <- uni[i]
  tmp35A <- rbind(tmp35A,tmp)
}
tmp35A$rate[is.na(tmp35A$rate)] <- 0
tmp35A$label <- ''
tmp35A$label[tmp35A$rate >= 0.1] <- 
  paste(as.character(round(100*tmp35A$rate[tmp35A$rate>=0.1],2)),'%',sep='')
tmp35A$rate[tmp35A$rate>0.1] <- 0.1
d <- 'TS'
p <- ggplot(subset(tmp35A),aes(x = factor(perc),y = rate*100,label = label)) + 
  geom_bar(aes(fill = class),stat = 'identity',position = 'dodge') + 
  geom_text() + 
  ggtitle(paste('Idle Time and Ship Time-',d,sep='')) + 
  xlab('Percentage of Time with Idel Disk(%)') + 
  ylab('Failure Rate(%)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.85,0.6),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与上架时间-',d,'.png',sep = '')),
       width = 12, height = 9, dpi = 100)

# 3.6 分段空闲时间对故障的影响,C类
se <- c(0,30,80,100.1)
tmp36 <- subset(use_999,dev_class_id == 'C1')
tmp36$use_perc_3 <- cut(tmp36$use_perc,se,right = F)
# tapply(tmp36$ut_part,tmp36$use_perc_3,tableX)
tmp36A <- data.frame(perc = levels(tmp36$use_perc_3),
                     rate = tapply(tmp36$class,tmp36$use_perc_3,function(x)sum(x == 'Failure')/length(x)))
d <- 'C'
p <- ggplot(subset(tmp36A),aes(x = factor(perc),y = rate*100)) + 
  geom_bar(stat = 'identity') + 
  #   ylim(c(0,1000)) + 
  ggtitle(paste('Idle Time and Ship Time-',d,sep='')) + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.8,0.6),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与上架时间(3段)-',d,'.png',sep = '')),
       width = 12, height = 9, dpi = 100)

# 3.7 在空闲时间占比为0-100的机器中挑100个出来看他们的999的值.(因为30档只有1443台机器，所以全部取完，每档取1400台)
# tmp <- tapply(use_999$svrid,use_999$use_perc_std,function(x)as.character(sample(x,1400)))
# svrid_perc <- data.frame()
# for (i in names(tmp)){
#   tmp1 <- data.frame(svrid = tmp[i],use_perc = i)
#   names(tmp1) <- c('svrid','use_perc')
#   svrid_perc <- rbind(svrid_perc,tmp1)
# }
# svrid_perc <- subset(svrid_perc,use_perc != '100')
# svrid_perc$use_perc <- as.numeric(levels(svrid_perc$use_perc)[svrid_perc$use_perc])
# save(svrid_perc,file = file.path(dir_data,'svrid_perc.Rda'))

#每档取100个
load(file.path(dir_data,'svrid_perc.Rda'))
svrid_perc_mysql <- data.frame()
k <- 1
c <- 100
for (i in seq(0,90,10)){
  tmp1 <- subset(svrid_perc,use_perc == i)
  svrid_perc_mysql <- rbind(svrid_perc_mysql,tmp1[seq(((k-1)*c+1),k*c,1),])
}
tmp2 <- sort(svrid_perc_mysql[['svrid']]) #这里写错，导致出了14000个的数据。差不多10G的数据。读不了的。
write.table(tmp2,file = file.path(dir_data,'svrid_percA'),quote = F,row.names = F,col.names = F)

#####################################################################################################################
#读取数据
# d1014_902 <- read.csv(file = file.path(dir_data,'attr_902_d1019A'),colClasses = c('factor','POSIXct','int','int'))
# save(d1014_999,file=file.path(dir_data,'attr_999_dataA'))

load(file = file.path(dir_data,'attr_999_dataA'))
load(file = file.path(dir_data,'attr_902_dataA'))
load(file = file.path(dir_data,'attr_903_dataA'))
d1014_999 <- d1014_999[order(d1014_999$svrid,d1014_999$time,d1014_999$timepoint),]
d1014_902 <- d1014_902[order(d1014_902$svrid,d1014_902$time,d1014_902$timepoint),]
d1014_903 <- d1014_903[order(d1014_903$svrid,d1014_903$time,d1014_903$timepoint),]
row.names(d1014_999) <- NULL
row.names(d1014_902) <- NULL
row.names(d1014_903) <- NULL
merge_io <- d1014_999
merge_io$value <- NULL
merge_io$a902 <- d1014_902$value
merge_io$a903 <- d1014_903$value
merge_io$a999 <- d1014_999$value
merge_io$time <- as.POSIXct(merge_io$time,tzone = 'UTC')
merge_io$timeNew <- merge_io$time + 5*60*merge_io$timepoint
merge_io$time <- merge_io$timeNew
merge_io$timepoint <- NULL
merge_io$timeNew <- NULL
save(merge_io,file = file.path(dir_data,'merge_io.Rda'))
merge_io$time <- factor(merge_io$time)
save(merge_io,file = file.path(dir_data,'merge_io_factor.Rda'))

for (s in svrid_perc$svrid){
  tmp <- subset(d1014_999,svrid == s & timeNew>as.POSIXct('2014-06-30') & timeNew <as.POSIXct('2014-07-07'))
  perc <- tmp$use_perc_std[1]
  t <- paste('999[',perc,']_',s,sep = '')
  p <- ggplot(tmp,aes(x = timeNew,y = value)) +
    geom_line() +
    ggtitle(t) + xlab('time') + ylab('value') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0.8,0.6),
          legend.justification = c(0,0),
          legend.background = element_rect(fill = alpha('grey',0.5)))
#   print(p)
  ggsave(plot = p,file = file.path(dir_data,'data_999',paste(t,'.png',sep = '')),
         width = 12, height = 9, dpi = 100)
}

# 3.9 观察902，903，999的数值关系
load(file.path(dir_data,'merge_io.Rda'))
merge_io <- subset(merge_io,a902 >= 0 & a903 >= 0 & a999 >= 0)
avg_io <- data.frame(svrid = levels(merge_io$svrid),
                 avg_902 = as.numeric(tapply(merge_io$a902,merge_io$svrid,mean)),
                 avg_903 = as.numeric(tapply(merge_io$a903,merge_io$svrid,mean)),
                 avg_999 = as.numeric(tapply(merge_io$a999,merge_io$svrid,mean)))

zero999_io <- subset(merge_io,a999 == 0)
zero999_quan902 <- data.frame(quan = seq(0,1,0.01),value = quantile(zero999_io$a902,seq(0,1,0.01)),attr = '902')
zero999_quan903 <- data.frame(quan = seq(0,1,0.01),value = quantile(zero999_io$a903,seq(0,1,0.01)),attr = '903')
zero999_quan <- rbind(zero999_quan902,zero999_quan903)
p <- ggplot(zero999_quan,aes(x = quan,y = value,group = attr,color = attr)) + 
  geom_line(size = 1) +  geom_point(size = 3) + ylim(c(0,1000)) +
  ggtitle('999为0值时的902/903分布') + xlab('分位点') + ylab('值') +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.1,0.6),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','999为0值时的902903分布.png'),
       width = 12, height = 9, dpi = 100)

full999_io <- subset(merge_io,a999 !=0)
full999_io$wr <- full999_io$a902 + full999_io$a903
full999_quan902 <- data.frame(quan = seq(0,1,0.01),value = quantile(full999_io$a902,seq(0,1,0.01)),attr = '902')
full999_quan903 <- data.frame(quan = seq(0,1,0.01),value = quantile(full999_io$a903,seq(0,1,0.01)),attr = '903')
full999_quan9023 <- data.frame(quan = seq(0,1,0.01),value = quantile(full999_io$wr,seq(0,1,0.01)),attr = 'wr')
full999_quan <- rbind(full999_quan902,full999_quan903)

p <- ggplot(full999_quan,aes(x = quan,y = value,group = attr,color = attr)) + 
  geom_line(size = 1) +  geom_point(size = 3) + ylim(c(0,50000)) +
  ggtitle('999为100时的902/903分布') + xlab('分位点') + ylab('值') +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.1,0.6),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','999为100时的902903分布.png'),
       width = 12, height = 9, dpi = 100)

# 3.10 导出故障机IO并分析
fail_svrid <- data.frame(svrid = intersect(data.f$svr_id,k131$svr_asset_id))
fail_svridA <- data.frame(svrid = setdiff(data.f$svr_id,k131$svr_asset_id))
fail_svridAll <- data.frame(svrid = unique(data.f$svr_id))
write.table(fail_svrid,file = file.path(dir_data,'fail_svrid'),row.names = F,quote = F,col.names = F)
write.table(fail_svridA,file = file.path(dir_data,'fail_svridA'),row.names = F,quote = F,col.names = F)
write.table(fail_svridAll,file = file.path(dir_data,'fail_svridAll'),row.names = F,quote = F,col.names = F)

# 3.11 查看机器的内核版本
cmdb_io <- subset(cmdb,svr_asset_id %in% k131_902$svrid)
cmdb_io$kernelFlag <- '0'
cmdb_io$kernelFlag[grepl('2.6.16.60',cmdb_io$os_kernal)] <- '16'
cmdb_io$kernelFlag[grepl('2.6.32.43',cmdb_io$os_kernal)] <- '32'
tableX(cmdb_io$kernelFlag)
statKernelA <- tableX(cmdb_io$os_kernal)
statKernelB <- tableX(cmdb_io$os_version)
statKernelA$num <- substr(statKernelA$item,1,9)
statKernelNum <- data.frame(tapply(statKernelA$count,statKernelA$num,sum))
# # 3.3 空闲时间对故障的影响,加入上架时间
# frate <- function(data){
#   tmp <- data.frame(perc = as.numeric(levels(data$use_perc_std)),
#                     rate = tapply(data$class,data$use_perc_std,function(x)sum(x == 'Failure')/length(x)))
#   return(tmp)
# }
# count_split <- 10
# tmp2 <- floor(use_999$use_perc/count_split)*count_split
# use_999$use_perc_std <- factor(tmp2,levels = sort(unique(tmp2)))
# d <- '2008'
# tmp2A <- use_999
# tmp2A <- subset(tmp2A,ut_part == d)
# tmp3 <- data.frame(perc = as.numeric(levels(tmp2A$use_perc_std)),
#                    rate = tapply(tmp2A$class,tmp2A$use_perc_std,function(x)sum(x == 'Failure')/length(x)))
# p <- ggplot(subset(tmp3),aes(x = factor(perc),y = rate*100)) + 
#   geom_bar(stat = 'identity',position = 'dodge') + 
#   #   ylim(c(0,1000)) + 
#   ggtitle(paste('Idle Time and Failure Rate-',d,sep='')) + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.8),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io',paste('硬盘空闲时间与故障率-',d,'.png',sep = '')),
#        width = 12, height = 9, dpi = 100)
# 
# # 3.3 机型/上架时间与空闲时间对故障的影响
# frate_3 <- function(data){
#   tmp <- data.frame(perc = levels(data$use_perc_3),
#                     rate = tapply(data$class,data$use_perc_3,function(x)sum(x == 'Failure')/length(x)))
#   return(tmp)
# }
# se <- c(0,30,80,100.1)
# tmp7 <- use_999
# tmp7$use_perc_3 <- cut(tmp7$use_perc,se,right = F)
# 
# # 上架时间分类
# uni <- unique(tmp7$ut_part)
# tmp3 <- data.frame()
# for (i in 1:length(uni)){
#   tmp <- frate_3(subset(tmp7,ut_part == uni[i]))
#   tmp$class <- uni[i]
#   tmp3 <- rbind(tmp3,tmp)
# }
# tmp3$rate[is.na(tmp3$rate)] <- 0
# p <- ggplot(subset(tmp3),aes(x = factor(perc),y = rate*100)) + 
#   geom_bar(aes(fill = class),stat = 'identity',position = 'dodge') + 
#   ggtitle('Idle Time and Ship Time') + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.6),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘空闲时间与上架时间.png'),
#        width = 12, height = 9, dpi = 100)
# 
# # 机型分类
# uni <- unique(tmp7$dev_class_id)
# tmp3 <- data.frame()
# for (i in 1:length(uni)){
#   tmp <- frate_3(subset(tmp7,dev_class_id == uni[i]))
#   tmp$class <- uni[i]
#   tmp3 <- rbind(tmp3,tmp)
# }
# tmp3$rate[is.na(tmp3$rate)] <- 0
# p <- ggplot(subset(tmp3),aes(x = factor(perc),y = rate*100)) + 
#   geom_bar(aes(fill = class),stat = 'identity',position = 'dodge') + 
#   ggtitle('Idle Time and Device Class') + xlab('Percentage of Time with Idel Disk(%)') + ylab('Failure Rate(%)') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.6),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘空闲时间与机型.png'),
#        width = 12, height = 9, dpi = 100)
# 
# # 3.4 上架时间与硬盘空闲时间
# use_999 <- subset(use_999,!is.na(ut_part))
# use_999$uu <- factor(paste(use_999$use_perc_std,use_999$ut_part,sep='_'))
# tmp4 <- data.frame(uu = levels(use_999$uu),
#                    rate = as.numeric(tapply(use_999$class,use_999$uu,function(x)sum(x == 'Failure')/length(x))))
# tmp5 <- strsplit(as.character(tmp4$uu),'_')
# tmp6 <- matrix(unlist(tmp5),byrow = T,nrow = length(tmp5))
# tmp4$user_perc_std <- tmp6[,1]
# tmp4$ut_part <- tmp6[,2]
# p <- ggplot(subset(tmp4),aes(x = user_perc_std,y = ut_part,size = rate)) + 
#   geom_point() + 
#   #   ylim(c(0,1000)) + 
#   ggtitle('Idle Time and Ship Time') + xlab('Percentage of Time with Idel Disk(%)') + ylab('Ship Time(Year)') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.8),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘空闲时间与上架时间.png'),
#        width = 12, height = 9, dpi = 100)

# 3.4 使用特征超过95%的机器的机型,上架时间,与故障作图
# 使用特征没有超过95%的机器的机型上架时间与故障作图
# use_999A <- subset(use_999,use_perc <= 0.05)
# use_999B <- subset(use_999,use_perc >0.05)
# plotDevStime(use_999A,'idle(A)')
# plotDevStime(use_999B,'idle(B)')

# # 4. 统计999中有p100超过100的行.
# tmp <- subset(k131_999,p100 > 100)
# tmp <- factorX(tmp)
# tmp.svrid <- data.frame(svrid = levels(tmp$svrid),
#                         count = as.numeric(tapply(tmp$svrid,tmp$svrid,length)))
# tmp.svrid$dev_class_id <- cmdb$dev_class_id[match(tmp.svrid$svrid,cmdb$svr_asset_id)]
# tmp.svrid <- factorX(tmp.svrid)
# se <- seq(0,60,10)
# tmp.svrid$cutCount <- cut(tmp.svrid$count,se)
# ggplot(tmp.svrid,aes(x = dev_class_id,fill = cutCount)) + geom_histogram()

# # 2.2 取没有IO的故障机数据,这些数据可能因为不全没有取特征
# svrid_more <- subset(data.f,!(svr_id %in% k131$svr_asset_id))
# svrid_more <- factor(svrid_more$svr_id)
# write.table(svrid_more,file = file.path(dir_data,'more_svrid'),row.names = F,col.names = F,quote = F)

# # 3.1 将点限制在0%-10%内
# p <- ggplot(subset(use_999,use_perc != -1)) + 
#   geom_histogram(aes(x = use_perc,fill = class),binwidth = 0.5) + 
#   xlim(c(0,10)) + 
#   ggtitle('Idle Time of Disk') + xlab('Percentage of Time with Idel Disk(%)') + ylab('Number of Servers') +
#   guides(fill = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.8),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘空闲时间统计-10perc.png'),
#        width = 12, height = 9, dpi = 100)
# tableX(cut(use_999$use_perc,seq(0,1,0.01),right = F))

# 2.31 两个月内故障机器机型
# svrid <- read.csv(file = file.path(dir_data,'all_svrid','all_svrid'),header = F)
# names(svrid) <- c('svrid','ip')
# tmp <- subset(data.f,svr_id %in% k131_svrid)
# svrid$dev_class_id <- cmdb$dev_class_id[match(svrid$svrid,cmdb$svr_asset_id)]