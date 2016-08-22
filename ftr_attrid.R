# 求IO用量特征.
# s*表示统计，P*表示画图
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))

#########################################################################################################
# S1. 两两联合划分区间，并给出各区间的故障率
# S1.1 区间测试，得出用10^seq(-5,5)来划分区间比较好
quanSeq <- seq(0,1,0.01)
quan_attr <- data.frame(quan = quanSeq,
                        q902 = as.numeric(quantile(mean_io$mean_902,quanSeq)),
                        q903 = as.numeric(quantile(mean_io$mean_903,quanSeq)),
                        q999 = as.numeric(quantile(mean_io$mean_999,quanSeq)))
quan_melted = melt(quan_attr,id.vars = 'quan')
ggplot(quan_melted,aes(x = quan,y = value,group = variable,color = variable)) +
  geom_line() + scale_y_log10()
serverZeroAttr <- subset(mean_io,mean_902 <= 0 | mean_903 <= 0 | mean_999 <= 0)
# S1.2 区间数量和区间故障数(因为log10不能有0和负数，所有要去掉
# 这样就去掉了一直没有读或写或CPU利用的机器,7637台，其中包含故障机47台)
# 标记：读r,写w,cpuUse c
# 标记：区间中机器数A,区间中故障数F,区间故障率R
mean_io <- subset(mean_io,mean_902 >= 0 & mean_903 >= 0 & mean_999 >= 0)
mean_io$dev_class_id <- cmdb$dev_class_id[match(mean_io$svrid,cmdb$svr_asset_id)]
mean_ioF <- subset(mean_io,class == 'Failure')
mean_io$mean_9023 <- mean_io$mean_902 + mean_io$mean_903
mean_io$ioFreq <- mean_io$mean_9023/mean_io$mean_999
singlePartCount <- 10

# S1.3.1 单字段非平均区间分析
mean_io_TS <- subset(mean_io,dev_class_id != 'C1')
mean_ioF_TS <- subset(mean_io_TS,class == 'Failure')
mean_io_C <- subset(mean_io,dev_class_id == 'C1')
mean_ioF_C <- subset(mean_io_C,class == 'Failure')

# sinAttrFR(mean_io_TS$mean_902,mean_ioF_TS$mean_902,singlePartCount,
#           'Storage Server - Value Partition and Failure Rate (Read)','KB/s',12)
# sinAttrFR(mean_io_TS$mean_903,mean_ioF_TS$mean_903,singlePartCount,
#           'Storage Server - Value Partition and Failure Rate (Write)','KB/s',12)
# sinAttrFR(mean_io_TS$mean_999,mean_ioF_TS$mean_999,singlePartCount,
#           'Storage Server - Value Partition and Failure Rate (Time)','%',12)
# sinAttrFR(mean_io_TS$mean_9023,mean_ioF_TS$mean_9023,singlePartCount,
#           'Storage Server - Value Partition and Failure Rate (Read and Write)','%',12)
# sinAttrFR(mean_io_TS$ioFreq,mean_ioF_TS$ioFreq,singlePartCount,
#           'Storage Server - Value Partition and Failure Rate (IO Frequency)','%',12)
# 
# 
# sinAttrFR(mean_io_C$mean_902,mean_ioF_C$mean_902,singlePartCount,
#           'non-Storage Server - Value Partition and Failure Rate (Read)','KB/s',1)
# sinAttrFR(mean_io_C$mean_903,mean_ioF_C$mean_903,singlePartCount,
#           'non-Storage Server - Value Partition and Failure Rate (Write)','KB/s',1)
# sinAttrFR(mean_io_C$mean_999,mean_ioF_C$mean_999,singlePartCount,
#           'non-Storage Server - Value Partition and Failure Rate (Time)','%',1)
# sinAttrFR(mean_io_C$mean_9023,mean_ioF_C$mean_9023,singlePartCount,
#           'non-Storage Server - Value Partition and Failure Rate (Read and Write)','%',1)
# sinAttrFR(mean_io_C$ioFreq,mean_ioF_C$ioFreq,singlePartCount,
#           'non-Storage Server - Value Partition and Failure Rate (IO Frequency)','%',1)

# S1.3.2 单字段累积故障率分析(横轴为根据字段排序后的机器编号)
# # TS类机器
# sinAttrCumFR(mean_io_TS,'mean_902','singleCumulationTSR.csv')
# sinAttrCumFR(mean_io_TS,'mean_903','singleCumulationTSW.csv')
# sinAttrCumFR(mean_io_TS,'mean_999','singleCumulationTST.csv')
# # C类机器
# sinAttrCumFR(mean_io_C,'mean_902','singleCumulationCR.csv')
# sinAttrCumFR(mean_io_C,'mean_903','singleCumulationCW.csv')
# sinAttrCumFR(mean_io_C,'mean_999','singleCumulationCT.csv')

# S1.3.4 单字段人工分区间划分求故障率
tmp1 <- sinAttrManualFR(mean_io_TS$mean_999,mean_ioF_TS$mean_999,
#                 c(seq(0,20,5),seq(30,70,10)),
                c(seq(0,5,1),seq(10,40,5),seq(50,100,20)),
                'Storage Server Manual uneven - Value Partition and Failure Rate (Time)','%',12)
tmp2 <- sinAttrManualFR(mean_io_C$mean_999,mean_ioF_C$mean_999,
                       c(seq(0,5,1),seq(10,20,5),seq(40,80,20),100),
#                         c(seq(0,20,5),seq(30,100,10)),
                       'non-Storage Server Manual uneven - Value Partition and Failure Rate (Time)','%',1)
tmp3 <- sinAttrManualFR(mean_io_TS$mean_999,mean_ioF_TS$mean_999,
                        seq(0,100,1),
                        'Storage Server Manual- Value Partition and Failure Rate (Time)','%',12)
tmp4 <- sinAttrManualFR(mean_io_C$mean_999,mean_ioF_C$mean_999,
                        seq(0,100,1),
                        'non-Storage Server Manual- Value Partition and Failure Rate (Time)','%',1)

# S1.3.x 总体数值的分布检验
#正态分布
require(MASS)
norm_902 <- shapiro.test(mean_io_C$mean_902)
norm_902 <- wilcox.test(mean_io_C$mean_902)
fit <- fitdistr(mean_io_C$mean_902,'exponential')
attr <- 'mean_903'
tmp <- mean_io_C[mean_io_C[[attr]] > quantile(mean_io_C[[attr]],0.50) & 
                   mean_io_C[[attr]] < quantile(mean_io_C[[attr]],0.90),]
ggplot(tmp,aes(x = mean_902)) + geom_bar(binwidth = 1)
  stat_ecdf(geom = 'smooth')

# S1.4 两字段分析(按单字段进行分割)
# doublePartCount <- 10
# tmp <- dblAttrFR(subset(mean_io_C,,c('svrid','mean_902','mean_903','class')),
#                  10,'Non-Storage Server - Read and Write','Read (KB/s)','Write (KB/s)')
# part_rwA <- tmp[[1]];part_rwF <- tmp[[2]]; part_rwR <- tmp[[3]]
# tmp <- dblAttrFR(subset(mean_io_C,,c('svrid','mean_902','mean_999','class')),
#                  10,'Non-Storage Server - Read and Time','Read (KB/s)','Time (%)')
# part_rcA <- tmp[[1]];part_rcF <- tmp[[2]]; part_rcR <- tmp[[3]]
# tmp <- dblAttrFR(subset(mean_io_C,,c('svrid','mean_903','mean_999','class')),
#                  10,'Non-Storage Server - Write and Time','Write (KB/s)','Time (KB/s)')
# part_wcA <- tmp[[1]];part_wcF <- tmp[[2]]; part_wcR <- tmp[[3]]
# 
# tmp <- dblAttrFR(subset(mean_io_TS,,c('svrid','mean_902','mean_903','class')),
#                  10,'Storage Server - Read and Write','Read (KB/s)','Write (KB/s)')
# part_rwA <- tmp[[1]];part_rwF <- tmp[[2]]; part_rwR <- tmp[[3]]
# tmp <- dblAttrFR(subset(mean_io_TS,,c('svrid','mean_902','mean_999','class')),
#                  10,'Storage Server - Read and Time','Read (KB/s)','Time (%)')
# part_rcA <- tmp[[1]];part_rcF <- tmp[[2]]; part_rcR <- tmp[[3]]
# tmp <- dblAttrFR(subset(mean_io_TS,,c('svrid','mean_903','mean_999','class')),
#                  10,'Storage Server - Write and Time','Write (KB/s)','Time (KB/s)')
# part_wcA <- tmp[[1]];part_wcF <- tmp[[2]]; part_wcR <- tmp[[3]]

# S1.5 两字段分析(读写，按读写各进行一次分割，按读的值进行二次分割)
# 结果并不好，放弃
doublePartCountA <- 10
mean_io_dp <- mean_io_TS
mean_io_dp$rw <- mean_io_dp$mean_902 + mean_io_dp$mean_903
mean_ioF_C <- subset(mean_io_dp,class == 'Failure')

divA <- as.numeric(quantile(mean_io_dp$rw,seq(0,1,1/doublePartCountA)))
divA <- divA + mean(divA)*1e-20*seq(0,(length(divA)-1),1)
mean_io_dp$sumRange <- cut(mean_io_dp$rw,divA,include.lowest = T)
mean_io_dp$sumLeft <- as.numeric(gsub("\\(|\\[|,.*","",mean_io_dp$sumRange))
uniLeft <- sort(unique(mean_io_dp$sumLeft))

mean_io_dp$bRange <- '0'
for (i in seq(1,length(uniLeft))){
#   tmp <- subset(mean_io_dp,sumLeft == uniLeft[i])
  divB <- as.numeric(quantile(mean_io_dp$mean_902[mean_io_dp$sumLeft == uniLeft[i]],
                               seq(0,1,1/doublePartCountA)))
  divB <- divB + mean(divB)*1e-20*seq(0,(length(divB)-1),1)
  mean_io_dp$bRange[mean_io_dp$sumLeft == uniLeft[i]] <- 
    as.character(cut(mean_io_dp$mean_902[mean_io_dp$sumLeft == uniLeft[i]],divB,include.lowest = T))
}
mean_io_dp$bLeft <- as.numeric(gsub("\\(|\\[|,.*","",mean_io_dp$bRange))
mean_io_dp$abLeft <- factor(paste(mean_io_dp$sumLeft,mean_io_dp$bLeft,sep='_'))
part <- data.frame(item = levels(mean_io_dp$abLeft),
                   sum = as.numeric(tapply(mean_io_dp$abLeft,mean_io_dp$abLeft,length)),
                   failure = as.numeric(tapply(mean_io_dp$class,mean_io_dp$abLeft,function(x)sum(x == 'Failure'))))
part$rate <- part$failure/part$sum/12
tmp <- data.frame(matrix(unlist(strsplit(as.character(part$item),'_')),byrow = T,nrow = nrow(part)))
names(tmp) <- c('sumLeft','bLeft')
tmp$sumLeft <- as.numeric(levels(tmp$sumLeft)[tmp$sumLeft])
tmp$bLeft <- as.numeric(levels(tmp$bLeft)[tmp$bLeft])
part <- cbind(part,tmp)
part <- part[order(tmp$sumLeft,tmp$bLeft),]
part$sumLeft <- factor(as.character(part$sumLeft),levels = sort(unique(part$sumLeft)))
part$bLeft <- factor(as.character(part$bLeft),levels = sort(unique(part$bLeft)))
ggplot(subset(part,rate <= 0.0065),aes(x = bLeft, y = sumLeft, size = rate)) + geom_point()

# S2. 两两聚类并求故障率
# 无法均分，放弃
colIO <- list(c('mean_902','mean_903'),
          c('mean_902','mean_999'),
          c('mean_903','mean_999'))
ftr.data <- mean_io
classNum <- 100
km <- kmeans(ftr.data[,colIO[[1]]],centers = classNum, nstart = 3)
mean_io$kmeanClass <- km$cluster

partA <- rep(0,1,classNum)
partF <- rep(0,1,classNum)
partR <- rep(0,1,classNum)
partCenter <- data.frame(km$centers)
for (i in seq(1,classNum)){
  tmp <- subset(mean_io,kmeanClass == i)
  partA[i] <- nrow(tmp)
  partF[i] <- nrow(tmp[tmp$class == 'Failure',])
}
partR <- partF/partA
partCenter$size <- partA
partCenter$rate <- partR
ggplot(partCenter,aes(x = mean_902, y = mean_903, size = rate, colour = size)) + 
  geom_point()

# S3.三字段累积分布计算
tmp_io <- mean_io_C
cdf <- data.frame(io = c(tmp_io$mean_902,tmp_io$mean_903,tmp_io$mean_999),
                  class = rep(c('902','903','999'),each = nrow(tmp_io)))
ggplot(subset(cdf,io>0),aes(log10(io),colour = class)) + stat_ecdf()
# #画图尝试
# melt_rcR <- melt(as.matrix(part_rcR))
# melt_rcA <- melt(as.matrix(part_rcA))
# melt_rcF <- melt(as.matrix(part_rcF))
# melt_rc <- cbind(melt_rcR,melt_rcA$value,melt_rcF$value)
# names(melt_rc) <- c('Var1','Var2','Rate','numAll','numFailure')
# ggplot(subset(melt_rc,Rate != -1),aes(x = as.character(Var1),y = as.character(Var2),size = value)) + geom_point()
#########################################################################################################
# # P1. 999均值与非空闲时间
# p <- ggplot(mean_io,aes(x = use_perc,y = mean_999)) + 
#   geom_point(aes(shape = class,color = class,alpha = size),size = 5) +
#   ggtitle('Disk Util') + xlab('Percentage of Time with Idel Disk(%)') + ylab('Mean Disk Util(%)') +
#   guides(color = guide_legend(title=NULL),shape = guide_legend(title=NULL)) + scale_alpha(guide = F) +
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.1),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘用量(CPU)与非空闲时间.png'),
#        width = 12, height = 9, dpi = 100)

# # P2. 999均值与故障数量
# p <- ggplot(mean_io,aes(x = mean_999)) + 
#   geom_bar(aes(fill = class),binwidth = 0.5) +
#   ggtitle('Disk Usage(Util)') + xlab('Mean Util(%)') + ylab('Number of Servers') +
#   xlim(c(0,30)) + 
#   guides(color = guide_legend(title=NULL),shape = guide_legend(title=NULL)) + scale_alpha(guide = F) +
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.7),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘用量(CPU)-30.png'),
#        width = 12, height = 9, dpi = 100)

# # P3. 作图: 902.mean与繁忙时间
# p <- ggplot(mean_io) + 
#   geom_point(aes(x = mean_902,y = use_perc,color = class,size = size)) +
#   ggtitle('Disk Usage (Read)') + xlab('Mean Read Usage(kB/s)') + ylab('Percentage of Time with Idel Disk(%)') +
#   guides(size=FALSE,color = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.1),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘用量(读).png'),
#        width = 12, height = 9, dpi = 100)

# # P4. 作图: 903.mean与繁忙时间
# p <- ggplot(mean_io) + 
#   geom_point(aes(x = mean_903,y = use_perc,color = class,size = size)) +
#   ggtitle('Disk Usage (Write)') + xlab('Mean Write Usage(kB/s)') + ylab('Percentage of Time with Idel Disk(%)') +
#   guides(size=FALSE,color = guide_legend(title=NULL)) + 
#   theme(axis.text = element_text(size = 18),
#         axis.title = element_text(size = 20),
#         legend.text = element_text(size = 18),
#         legend.title = element_text(size = 20),
#         plot.title = element_text(size = 26, face = 'bold'),
#         legend.position = c(0.8,0.1),
#         legend.justification = c(0,0),
#         legend.background = element_rect(fill = alpha('grey',0.5)))
# print(p)
# ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘用量(写).png'),
#        width = 12, height = 9, dpi = 100)

# P5. 作图: 902.mean与903.mean
p <- ggplot(mean_io,aes(x = mean_902,y = mean_903)) + 
  geom_point(aes(shape = class,color = class,alpha = size),size = 5) +
  ggtitle('Disk Usage and Failure') + xlab('Mean Read Usage(kB/s)') + ylab('Mean Write Usage(kB/s)') +
  xlim(c(1,75000)) + ylim(c(1,75000)) +
  guides(color = guide_legend(title=NULL),shape = guide_legend(title=NULL)) + scale_alpha(guide = F) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.1,0.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘读写与故障-散点图.png'),
       width = 12, height = 9, dpi = 100)

# P6. 作图: 902.mean与903.mean(限制5000)
p <- ggplot(mean_io,aes(x = mean_902,y = mean_903)) + 
  geom_point(aes(shape = class,color = class,alpha = size),size = 5) +
  ggtitle('Disk Usage and Failure') + xlab('Mean Read Usage(kB/s)') + ylab('Mean Write Usage(kB/s)') +
  xlim(c(1,5000)) + ylim(c(1,5000)) +
  guides(color = guide_legend(title=NULL),shape = guide_legend(title=NULL)) + scale_alpha(guide = F) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.1,0.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘读写与故障-散点图-5000.png'),
       width = 12, height = 9, dpi = 100)

# P7. 作图: 902.mean与故障
p <- ggplot(mean_io) + 
  geom_bar(aes(x = mean_902,fill = class)) +
  ggtitle('Disk usage(Read) and Failure') + xlab('Mean Read Usage(kB/s)') + ylab('Number of Servers') + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.8,0.1),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘用量(读)-直方图.png'),
       width = 12, height = 9, dpi = 100)

# P8. 作图: 903.mean与故障
p <- ggplot(mean_io) + 
  geom_bar(aes(x = mean_903,fill = class)) +
  ggtitle('Disk usage(Write) and Failure') + xlab('Mean Write Usage(kB/s)') + ylab('Number of Servers') + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0.8,0.1),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(plot = p,file = file.path(dir_data,'ftr_io','硬盘用量(写)-直方图.png'),
       width = 12, height = 9, dpi = 100)
