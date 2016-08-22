# 读与写的不同特性对故障的影响
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
source(file.path(dir_code,'AFR_io_prepare.R'))

#@@@ LOCAL FUNCTION @@@#

############################################################################################
# C1.为所有机器生成读写总量与读写比例
ioC1 <- subset(tmp.io,mean_902 != 0 & mean_903 != 0)
ioC1$mean_902 <- log2(ioC1$mean_902*86400*365)
ioC1$mean_903 <- log2(ioC1$mean_903*86400*365)
ioC1$mean_9023 <- log2(2^ioC1$mean_902 + 2^ioC1$mean_903)
ioC1$rwRate <- (2^ioC1$mean_903)/(2^ioC1$mean_9023)*100

# C1.1 分字段处理 + 生成故障单字段
div902 <- seq(10,44,1)
ioC1$cut902 <- cut(ioC1$mean_902,div902)
ioC1$sep902 <- as.numeric(gsub('^\\(|,.*$','',ioC1$cut902))
div903 <- seq(26,43,0.5)
ioC1$cut903 <- cut(ioC1$mean_903,div903)
ioC1$sep903 <- as.numeric(gsub('^\\(|,.*$','',ioC1$cut903))
div9023 <- seq(26,44,1)
ioC1$cut9023 <- cut(ioC1$mean_9023,div9023)
ioC1$sep9023 <- as.numeric(gsub('^\\(|,.*$','',ioC1$cut9023))
divRate <- c(seq(0,100,5))
ioC1$cutRate <- cut(ioC1$rwRate,divRate)
ioC1$sepRate <- as.numeric(gsub('^\\(|,.*$','',ioC1$cutRate))

#将分段数据交给故障机
fC1 <- subset(tmp.f, svr_id %in% ioC1$svrid)
fC1$sep902 <- ioC1$sep902[match(fC1$svr_id,ioC1$svrid)]
fC1$sep903 <- ioC1$sep903[match(fC1$svr_id,ioC1$svrid)]
fC1$sep9023 <- ioC1$sep9023[match(fC1$svr_id,ioC1$svrid)]
fC1$sepRate <- ioC1$sepRate[match(fC1$svr_id,ioC1$svrid)]
# ggplot(subset(ioC1,dClass == 'TS2T'),aes(x = mean_9023,fill = factor(shTime))) + geom_bar(binwidth = 1,position = 'fill')

# C1.2 单字段分机型处理
ioC1C <- subset(ioC1,dClass == 'C')
ioC1TS1 <- subset(ioC1,dClass == 'TS1T')
ioC1TS2 <- subset(ioC1,dClass == 'TS2T')
ioC1TS <- subset(ioC1,grepl('TS',dClass))
fC1C <- subset(fC1,dClass == 'C')
fC1TS1 <- subset(fC1,dClass == 'TS1T')
fC1TS2 <- subset(fC1,dClass == 'TS2T')
fC1TS <- subset(fC1,grepl('TS',dClass))

ioAFR <- function(div,ioC,fC,attr,diskCount = 1){
  AFR <- data.frame(sep = div,
                    count = sapply(div,function(x)sum(ioC[[attr]] == x)),
                    fcount = sapply(div,function(x)sum(fC[[attr]] == x)))
  AFR$AFR <- AFR$fcount/AFR$count*6*100/diskCount
  AFR <- subset(AFR,count > 100)
  AFR
}

#分机型
AFR9023C <- ioAFR(div9023,ioC1C,fC1C,'sep9023')
AFR9023TS1 <- ioAFR(div9023,ioC1TS1,fC1TS1,'sep9023',12)
AFR9023TS2 <- ioAFR(div9023,ioC1TS2,fC1TS2,'sep9023',12)
AFR9023TS <- ioAFR(div9023,ioC1TS,fC1TS,'sep9023',12)

AFRRateC <- ioAFR(divRate,ioC1C,fC1C,'sepRate')
AFRRateTS1 <- ioAFR(divRate,ioC1TS1,fC1TS1,'sepRate',12)
AFRRateTS2 <- ioAFR(divRate,ioC1TS2,fC1TS2,'sepRate',12)
AFRRateTS <- ioAFR(divRate,ioC1TS,fC1TS,'sepRate',12)

AFR9023C$class <- 'Non-Storage Servers'
AFR9023TS1$class <- 'Storage Servers[1T]'
AFR9023TS2$class <- 'Storage Servers[2T]'
AFR9023Both <- rbind(subset(AFR9023C,sep>=29 & sep<37),
                     subset(AFR9023TS1,sep > 30 & sep < 41),
                     subset(AFR9023TS2,sep > 30 & sep < 41))

#写占比低于10%
AFRRateTS_10 <- ioAFR(div9023,subset(ioC1TS,sepRate <= 10),subset(fC1TS,sepRate <= 10),'sep9023',12)
ggplot(AFRRateTS_10,aes(x = sep,y = AFR)) + geom_bar(stat = 'identity')
#写占比为[40,60]
AFRRateTS_40_60 <- ioAFR(div9023,subset(ioC1TS,sepRate >= 40 & sepRate <= 60),
                         subset(fC1TS,sepRate >= 40 & sepRate <= 60),'sep9023',12)
ggplot(AFRRateTS_40_60,aes(x = sep,y = AFR)) + geom_bar(stat = 'identity')

# 读写比例画图
Ra1 <- ggplot(subset(AFRRateC,sep>= 35),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Percentage of Write Workload in I/O Workload (%)') + ylab('Annual Failure Rate (%)') + 
  ggtitle('I/O Workload Distribution and AFR (Non-Storage Servers)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
Ra2 <- ggplot(subset(AFRRateTS1),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Percentage of Write Workload in I/O Workload (%)') + ylab('Annual Failure Rate (%)') + 
  ggtitle('I/O Workload Distribution and AFR (Storage Servers[1T])') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
Ra3 <- ggplot(subset(AFRRateTS2),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Percentage of Write Workload in I/O Workload (%)') + ylab('Annual Failure Rate (%)') + 
  ggtitle('I/O Workload Distribution and AFR (Storage Servers[2T])') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
Ra4 <- ggplot(subset(AFRRateTS),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Percentage of Write Workload in I/O Workload (%)') + ylab('Annual Failure Rate (%)') + 
  ggtitle('I/O Workload Distribution and AFR (Storage Servers)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))

# 读写总量（log2）画图
A1 <- ggplot(subset(AFR9023C,sep >= 29 & sep < 37),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Annual I/O Workload (2^x KB)') + ylab('Annual Failure Rate (%)') + 
  ggtitle('Annual I/O Workload and AFR (Non-Storage Servers)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
A2 <- ggplot(subset(AFR9023TS1,sep >= 29 & sep <= 40),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Annual I/O Workload (2^x KB)') + ylab('Annual Failure Rate (%)') + xlim(c(28,41)) +
  ggtitle('Annual I/O Workload and AFR (Storage Servers[1T])') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
A3 <- ggplot(subset(AFR9023TS2,sep >= 29 & sep <= 40),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Annual I/O Workload (2^x KB)') + ylab('Annual Failure Rate (%)') + xlim(c(28,41)) +
  ggtitle('Annual I/O Workload and AFR (Storage Servers[2T])') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
A4 <- ggplot(subset(AFR9023TS,sep >= 29 & sep <= 40),aes(x=sep,y=AFR)) + geom_bar(stat='identity') + 
  xlab('Annual I/O Workload (2^x KB)') + ylab('Annual Failure Rate (%)') + xlim(c(28,41)) +
  ggtitle('Annual I/O Workload and AFR (Storage Servers)') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
ABoth <- ggplot(subset(AFR9023Both),aes(x=sep,y=AFR,fill=class)) + geom_bar(stat='identity',position = 'dodge') + 
  xlab('Annual I/O Workload (2^x KB)') + ylab('Annual Failure Rate (%)') +
  ggtitle('Annual I/O Workload and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
#存储
ggsave(file = file.path(dir_data,'ReadWrite','C_Rate.png'), plot=Ra1, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','TS1_Rate.png'), plot=Ra2, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','TS2_Rate.png'), plot=Ra3, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','TS_Rate.png'), plot=Ra4, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','C_All.png'), plot=A1, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','TS1_All.png'), plot=A2, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','TS2_All.png'), plot=A3, width = 16, height = 12, dpi = 100)
ggsave(file = file.path(dir_data,'ReadWrite','TS_All.png'), plot=A4, width = 16, height = 12, dpi = 100)

# C1.3 读写两字段处理
# rwTable <- colTableX(ioC1TS1,c('sep902','sep903'))
# rwTable <- cbind(rwTable,splitToDF(rwTable$item))
# rwTable$rate <- NULL
# names(rwTable) <- c('item','count','Read','Write')
# rwTable$Read <- as.numeric(levels(rwTable$Read)[rwTable$Read])
# rwTable$Write <- as.numeric(levels(rwTable$Write)[rwTable$Write])
# 
# rwTablef <- colTableX(fC1TS1,c('sep902','sep903'))
# rwTablef <- cbind(rwTablef,splitToDF(rwTablef$item))
# rwTablef$rate <- NULL
# names(rwTablef) <- c('item','count','Read','Write')
# rwTablef$Read <- as.numeric(levels(rwTablef$Read)[rwTablef$Read])
# rwTablef$Write <- as.numeric(levels(rwTablef$Write)[rwTablef$Write])
# 
# rwTable$fcount <- 0
# rwTable$fcount <- rwTablef$count[match(rwTable$item,rwTablef$item)]
# # rwTable$fcount[is.na(rwTable$fcount)] <- 0
# rwTable$AFR <- rwTable$fcount/rwTable$count*6
# 
# rwTable <- rwTable[,c('Read','Write','fcount','count','AFR')]
# ggplot(subset(rwTable,count > 100),aes(x = Read,y = Write, fill = AFR)) + geom_tile()
# 
# AFR9023 <- ioAFR(div9023,ioC1,fC1,'sep9023')
# tmp <- subset(AFR9023TS,count > 10)
# ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
# ggplot(ioC1TS1,aes(x = sep9023,fill = dev_class_id)) + geom_bar()
# ggplot(subset(tmp),aes(x = sep,y = AFR*6)) + geom_line()
# 
# # C2 存储型机器根据总量分类，以年读写总量8T分界
# ioC2.TS.L <- subset(ioC1,dClass == 'TS' & sep9023 >= 33)
# ioC2.TS.S <- subset(ioC1,dClass == 'TS' & sep9023 < 33)
# fC2.TS.L <- subset(fC1,dClass == 'TS' & sep9023 >= 33)
# fC2.TS.S <- subset(fC1,dClass == 'TS' & sep9023 < 33)
# write.table(AFR902C,file.path(dir_data,'AFR902C.csv'),quote = F,row.names = F,sep = ',')
# write.table(AFR903C,file.path(dir_data,'AFR903C.csv'),quote = F,row.names = F,sep = ',')
# write.table(AFR902TS,file.path(dir_data,'AFR902TS.csv'),quote = F,row.names = F,sep = ',')
# write.table(AFR903TS,file.path(dir_data,'AFR903TS.csv'),quote = F,row.names = F,sep = ',')
# 读写占比计算
# ioC1$rwRateRnd <- round(ioC1$rwRate*100)/100
# rwRateTable <- colTableX(ioC1,c('rwRateRnd','sep9023'))
# rwRateTable <- cbind(rwRateTable,splitToDF(rwRateTable$item))
# rwRateTable$item <- NULL
# rwRateTable$rate <- NULL
# names(rwRateTable) <- c('count','rate','total')
# rwRateTable$rate <- as.numeric(levels(rwRateTable$rate)[rwRateTable$rate])
# rwRateTable$total <- as.numeric(levels(rwRateTable$total)[rwRateTable$total])
# ggplot(subset(rwRateTable,count > 100),aes(x = rate,y = total,fill = log2(count))) + geom_tile()

