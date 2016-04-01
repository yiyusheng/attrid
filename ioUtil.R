# IO利用率分析，对每台机器的999的值与故障之间的关系进行分析
# 并对9023/999的值进行分析

rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'AFR_io_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attridN.Rda'))
source(file.path(dir_code,'AFR_io_prepareN.R'))
#speed + util + Dense特征
load(file.path(dir_data,'ioFeature.Rda'))
#100%util持续时间
load(file.path(dir_data,'ioUtilMax.Rda'))

#@@@ FUNCTION @@@#

# F1.将df的item以括号后第一个数字的顺序排序,用于方便的画图[已加入attr_function]
item_order <- function(df,attr = 'item'){
  od <- as.numeric(gsub('^\\[|^\\(|,.*$','',df[[attr]]))
  df[attr] <- factor(df[[attr]],levels = df[[attr]][order(od)])
  df <- df[order(od),] 
  row.names(df) <- NULL
  df
}

#########################################################################################################
# A1.999值每日均值，每日最值，top10最值与故障率的关系
ioFtr$cvUtil <- ioFtr$sdUtil/ioFtr$meanUtil
# ioFtr <- ioUtilMax
x <- 'max';attr <- paste(x,'Util',sep='')
attr <- 'maxUtil'

ioF <- factorX(subset(ioFtr,svrid %in% tmp.cmdb$svr_asset_id,c('svrid',attr)))
names(ioF) <- c('svrid','attr')
meanU <- tapply(ioF$attr,ioF$svrid,mean)
maxU <- tapply(ioF$attr,ioF$svrid,max)
maxtopU <- tapply(ioF$attr,ioF$svrid,function(x){x <- sort(x,decreasing = T);mean(x[1:10])})

#   divU <- c(0:10,50,100)
divU <- c(-1,0,1,2,3,6,12,60,120,288)
staU <- data.frame(svrid = names(meanU),
                   meanoU = as.numeric(meanU),
                   maxoU = as.numeric(maxU),
                   maxtopoU = as.numeric(maxtopU),
                   meanU = cut(as.numeric(meanU),divU,include.lowest = T),
                   maxU = cut(as.numeric(maxU),divU,include.lowest = T),
                   maxtopU = cut(as.numeric(maxtopU),divU,include.lowest = T))
  
tf <- merge(tmp.f,staU,by.x = 'svr_id',by.y = 'svrid')
tcmdb <- merge(tmp.cmdb,staU,by.x = 'svr_asset_id',by.y = 'svrid')
tfC <- subset(tf,grepl('C',dClass));tfTS <- subset(tf,grepl('TS',dClass))
tcmdbC <- subset(tcmdb,grepl('C',dClass)); tcmdbTS <- subset(tcmdb,grepl('TS',dClass))
ggplot(tfTS,aes(maxU)) + geom_histogram()
ggplot(tfC,aes(maxU)) + geom_histogram()


AFR_meanUC <- item_order(AFR_attr_notime(tf,tcmdb,'meanU','meanU',1,dev = 'C'))
AFR_maxUC <- item_order(AFR_attr_notime(tf,tcmdb,'maxU','maxU',1,dev = 'C'))
AFR_maxtopUC <- item_order(AFR_attr_notime(tf,tcmdb,'maxtopU','maxtopU',1,dev = 'C'))
AFR_meanUTS <- item_order(AFR_attr_notime(tf,tcmdb,'meanU','meanU',12,dev = 'TS'))
AFR_maxUTS <- item_order(AFR_attr_notime(tf,tcmdb,'maxU','maxU',12,dev = 'TS'))
AFR_maxtopUTS <- item_order(AFR_attr_notime(tf,tcmdb,'maxtopU','maxtopU',12,dev = 'TS'))

AFR_meanU <- rbind(AFR_meanUC,AFR_meanUTS)
AFR_maxU <- rbind(AFR_maxUC,AFR_maxUTS)
AFR_maxtopU <- rbind(AFR_maxtopUC,AFR_maxtopUTS)

p1 <- ggplot(subset(AFR_meanU,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') + ggtitle(paste(attr,'Mean'))
p2 <- ggplot(subset(AFR_maxU,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') + ggtitle(paste(attr,'Max'))
p3 <- ggplot(subset(AFR_maxtopU,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') + ggtitle(paste(attr,'Maxtop'))

ggsave(file=file.path(dir_data,'ioUtil',paste(attr,'Mean','.png',sep='')), 
       plot=p1, width = 16, height = 12, dpi = 100)
ggsave(file=file.path(dir_data,'ioUtil',paste(attr,'Max','.png',sep='')), 
       plot=p2, width = 16, height = 12, dpi = 100)
ggsave(file=file.path(dir_data,'ioUtil',paste(attr,'Maxtop','.png',sep='')), 
       plot=p3, width = 16, height = 12, dpi = 100)
#   return(list(AFR_meanU,AFR_maxU,AFR_maxtopU))

#计算持续繁忙时间没有超过5分钟的机器的故障率
busyBound <- 98
busyPerc <- data.frame(lowerC = nrow(tfC[tfC$maxoU < busyBound,])/nrow(tcmdbC[tcmdbC$maxoU < busyBound,])*6*100,
                       higherC = nrow(tfC[tfC$maxoU >= busyBound,])/nrow(tcmdbC[tcmdbC$maxoU >= busyBound,])*6*100,
                       lowerTS = nrow(tfTS[tfTS$maxoU < busyBound,])/nrow(tcmdbTS[tcmdbTS$maxoU < busyBound,])/12*6*100,
                       higherTS = nrow(tfTS[tfTS$maxoU >= busyBound,])/nrow(tcmdbTS[tcmdbTS$maxoU >= busyBound,])/12*6*100)

# A2.结合最长持续时间和max次数进行分析
# [结果并没有什么用，本来希望存储机最大满载时间少时故障率高的原因是满载次数多，结果并不是]
ioU <- factorX(subset(ioUtilMax,svrid %in% tmp.cmdb$svr_asset_id))
ioU$A1 <- ioU$maxLastCount
ioU$A2 <- ioU$maxCount

meanA1 <- tapply(ioU$A1,ioU$svrid,mean)
maxA1 <- tapply(ioU$A1,ioU$svrid,max)
maxtopA1 <- tapply(ioU$A1,ioU$svrid,function(x){x <- sort(x,decreasing = T);mean(x[1:10])})

meanA2 <- tapply(ioU$A2,ioU$svrid,mean)
maxA2 <- tapply(ioU$A2,ioU$svrid,max)
maxtopA2 <- tapply(ioU$A2,ioU$svrid,function(x){x <- sort(x,decreasing = T);mean(x[1:10])})
maxsumA2 <- tapply(ioU$A2,ioU$svrid,sum)
maxsumA2[maxsumA2 == 0] <- 0.5
maxsumA2 <- log2(maxsumA2)

divU1 <- c(-1,0,1,2,3,6,12,60,120,288)
divU2 <- c(seq(0,24,2),144,288)
divU3 <- c(-1,seq(0,9,3),15)

staU <- data.frame(svrid = names(meanU),
                   meanoA1 = as.numeric(meanA1),
                   maxoA1 = as.numeric(maxA1),
                   maxtopoA1 = as.numeric(maxtopA1),
                   meanA1 = cut(as.numeric(meanA1),divU1,include.lowest = T),
                   maxA1 = cut(as.numeric(maxA1),divU1,include.lowest = T),
                   maxtopA1 = cut(as.numeric(maxtopA1),divU1,include.lowest = T),
                   meanoA2 = as.numeric(meanA2),
                   maxoA2 = as.numeric(maxA2),
                   maxtopoA2 = as.numeric(maxtopA2),
                   maxsumoA2 = as.numeric(maxsumA2),
                   meanA2 = cut(as.numeric(meanA2),divU2,include.lowest = T),
                   maxA2 = cut(as.numeric(maxA2),divU2,include.lowest = T),
                   maxtopA2 = cut(as.numeric(maxtopA2),divU2,include.lowest = T),
                   maxsumA2 = cut(as.numeric(maxsumA2),divU3,include.lowest = T))

tf <- merge(tmp.f,staU,by.x = 'svr_id',by.y = 'svrid')
tcmdb <- merge(tmp.cmdb,staU,by.x = 'svr_asset_id',by.y = 'svrid')

tfC <- subset(tf,grepl('C',dClass));tfTS <- subset(tf,grepl('TS',dClass))
tcmdbC <- subset(tcmdb,grepl('C',dClass)); tcmdbTS <- subset(tcmdb,grepl('TS',dClass))

ggplot(tfTS,aes(meanA2)) + geom_histogram()
ggplot(tfC,aes(meanA2)) + geom_histogram()

# 将不同持续满载时长的故障率根据满载次数分成几份再画图
attrAdd <- 'shTime'
# TS类
tbl.cmdbTS <- melt(table(tcmdbTS[,c('maxA1',attrAdd)]));tbl.fTS <- melt(table(tfTS[,c('maxA1',attrAdd)]))
AFR_muTS <- merge(tbl.cmdbTS,tbl.fTS,by = c('maxA1',attrAdd))
AFR_muTS$AFR <- AFR_muTS$value.y/AFR_muTS$value.x
AFR_muTS <- AFR_muTS[!is.na(AFR_muTS$AFR),]
tbl.cmdb1TS <- melt(table(tcmdbTS[,c('maxA1')]));tbl.f1TS <- melt(table(tfTS$maxA1))
AFR_suTS <- merge(tbl.cmdb1TS,tbl.f1TS,by = 'Var1')
AFR_suTS$AFR <- AFR_suTS$value.y/AFR_suTS$value.x
AFR_muTS$AFRs <- AFR_suTS$AFR[match(AFR_muTS$maxA1,AFR_suTS$Var1)]
AFR_muTS$AFRst <- 0
for (x in levels(AFR_muTS$maxA1)){
  idx <- which(AFR_muTS$maxA1 == x)
  s <- sum(AFR_muTS$AFR[idx])
  AFR_muTS$AFRst[idx] <- AFR_muTS$AFRs[idx] * AFR_muTS$AFR[idx]/s
}
eval(parse(text = sprintf("ggplot(subset(AFR_muTS,value.x > 1),aes(maxA1,AFRst,fill = factor(%s))) + 
                          geom_bar(stat = 'identity',position = 'stack')",attrAdd)))

# C类
tbl.cmdbC <- melt(table(tcmdbC[,c('maxA1',attrAdd)]));tbl.fC <- melt(table(tfC[,c('maxA1',attrAdd)]))
AFR_muC <- merge(tbl.cmdbC,tbl.fC,by = c('maxA1',attrAdd))
AFR_muC$AFR <- AFR_muC$value.y/AFR_muC$value.x
AFR_muC <- AFR_muC[!is.na(AFR_muC$AFR),]
tbl.cmdb1C <- melt(table(tcmdbC[,c('maxA1')]));tbl.f1C <- melt(table(tfC$maxA1))
AFR_suC <- merge(tbl.cmdb1C,tbl.f1C,by = 'Var1')
AFR_suC$AFR <- AFR_suC$value.y/AFR_suC$value.x
AFR_muC$AFRs <- AFR_suC$AFR[match(AFR_muC$maxA1,AFR_suC$Var1)]
AFR_muC$AFRst <- 0
for (x in levels(AFR_muC$maxA1)){
  idx <- which(AFR_muC$maxA1 == x)
  s <- sum(AFR_muC$AFR[idx])
  AFR_muC$AFRst[idx] <- AFR_muC$AFRs[idx] * AFR_muC$AFR[idx]/s
}
eval(parse(text = sprintf("ggplot(subset(AFR_muC,value.x > 1),aes(maxA1,AFRst,fill = factor(%s))) + 
geom_bar(stat = 'identity',position = 'stack')",attrAdd)))


# P1.结论画图
AFR_maxU$item <- gsub('^\\[.*,|^\\(.*,|\\]','',AFR_maxU$item)
AFR_maxU$item <- factor(as.character(as.numeric(AFR_maxU$item)/12))
od <- order(as.numeric(levels(AFR_maxU$item)))
AFR_maxU$item <- factor(AFR_maxU$item,levels = levels(AFR_maxU$item)[od])
levels(AFR_maxU$item) <- c('0','(0,5]','(5,10]','(10,15]','(15,30]','(30,60]','(60,300]','(300,600]','(600,1440]')

pp <- ggplot(subset(AFR_maxU,count_io > 1),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  xlab('Max Duration of Full Load (Minutes)') + ylab('Annual Failure Rate (%)') + 
  ggtitle('Duration of Full Load and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0,1),
        legend.justification = c(-0.2,1.5),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(pp)
ggsave(file=file.path(dir_data,'ioUtil',paste('Duration of Full Load and AFR','.png',sep='')), 
       plot=pp, width = 16, height = 12, dpi = 100)


# cairo_ps(file=file.path(dir_data,'ioUtil',paste('Duration of Full Load and AFR','.eps',sep=''))
#          , width=16, height=12)
# print(pp)
# dev.off()

