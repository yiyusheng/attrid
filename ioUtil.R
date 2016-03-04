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
load(file.path(dir_data,'load_ftr_attrid.Rda'))
source(file.path(dir_code,'AFR_io_prepare.R'))
load(file.path(dir_data,'ioFeature.Rda'))
load(file.path(dir_data,'ioUtilLast.Rda'))

#@@@ FUNCTION @@@#

# F1.将df的item以括号后第一个数字的顺序排序,用于方便的画图
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
ioFtr <- merge(ioFtr,ioUtilLast,by = c('svrid','time'))
x <- 'max'
# r <- sapply(c('min','q25','q50','mean','q75','max','sd','cv'),function(x){
  attr <- paste(x,'Util',sep='')
  attr <- 'lastCount'
  ioF <- factorX(subset(ioFtr,svrid %in% tmp.cmdb$svr_asset_id,c('svrid',attr)))
  names(ioF) <- c('svrid','attr')
  meanU <- tapply(ioF$attr,ioF$svrid,mean)
  maxU <- tapply(ioF$attr,ioF$svrid,max)
  maxtopU <- tapply(ioF$attr,ioF$svrid,function(x){x <- sort(x,decreasing = T);mean(x[1:10])})
  
#   divU <- c(0:10,50,100)
  divU <- c(-1,0,1,2,12,60,288)
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
  return(list(AFR_meanU,AFR_maxU,AFR_maxtopU))
# })

#计算持续繁忙时间没有超过5分钟的机器的故障率
busyPerc <- data.frame(lowerC = nrow(tfC[tfC$maxoU < 100,])/nrow(tcmdbC[tcmdbC$maxoU < 100,])*6*100,
                       higherC = nrow(tfC[tfC$maxoU == 100,])/nrow(tcmdbC[tcmdbC$maxoU == 100,])*6*100,
                       lowerTS = nrow(tfTS[tfTS$maxoU < 100,])/nrow(tcmdbTS[tcmdbTS$maxoU < 100,])/12*6*100,
                       higherTS = nrow(tfTS[tfTS$maxoU == 100,])/nrow(tcmdbTS[tcmdbTS$maxoU == 100,])/12*6*100)

# P1.结论画图
AFR_maxU$item <- gsub('^\\[.*,|^\\(.*,|\\]','',AFR_maxU$item)
AFR_maxU$item <- factor(as.character(as.numeric(AFR_maxU$item)/12))
od <- order(as.numeric(levels(AFR_maxU$item)))
AFR_maxU$item <- factor(AFR_maxU$item,levels = levels(AFR_maxU$item)[od])
levels(AFR_maxU$item) <- c('0','(0,0.5]','(0.5,1]','(1,5]','(5,24]')

pp <- ggplot(subset(AFR_maxU,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  xlab('Max Duration of Full Load (Hours)') + ylab('Annual Failure Rate (%)') + 
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

cairo_ps(file=file.path(dir_data,'ioUtil',paste('Duration of Full Load and AFR','.eps',sep=''))
         , width=16, height=12)
print(pp)
dev.off()
ggsave(file=file.path(dir_data,'ioUtil',paste('Duration of Full Load and AFR','.eps',sep='')), 
       plot=pp, width = 16, height = 12, dpi = 100)
