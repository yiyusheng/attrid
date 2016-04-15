# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'AFR_io_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attridOld.Rda'))
source(file.path(dir_code,'AFR_io_prepareOld.R'))
#speed + util + Dense特征
load(file.path(dir_data,'ioFeature.Rda'))
#100%util持续时间
load(file.path(dir_data,'ioUtilMax.Rda'))

#####################################################################################################
#@@@ FUNCTION @@@#
# F1.plot
AFR_plot <- function(cm,title,ylimL,ylimR){ 
  cm1 <- cm
  p1 <- ggplot(cm1,aes(x = factor(maxA1),y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Duration of full workload (minutes)') + ylab('Annual Failure Rate (%)') + 
    scale_y_continuous(limits = c(ylimL,ylimR),oob = rescale_none,breaks = seq(ylimL,ylimR,5)) +
    # scale_x_continuous(breaks = floor(min(cm1$maxCVd)):ceiling(max(cm1$maxCVd))) +
    # scale_y_continuous(breaks = seq(0,8,1)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
#           panel.grid.major.x = element_blank(),
          
          plot.title = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(0.05,0.95),legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), plot=p1, width = 8, height = 6, dpi = 100)
}

#F2. AFR calculate
ioAFR <- function(io,f,attr,diskCount = 1){
  t1 <- melt(table(io[,attr]))
  t2 <- melt(table(f[,attr]))
  if(length(attr) == 1){
    names(t1)[1] <- attr
    names(t2)[1] <- attr
  }
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*600
  tMerge <- subset(tMerge,!is.na(AFR))
}

#F3. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}
#####################################################################################################
ioFtr$cvUtil <- ioFtr$sdUtil/ioFtr$meanUtil
attr <- 'maxUtil'

ioF <- factorX(subset(ioFtr,svrid %in% tmp.cmdb$svr_asset_id,c('svrid',attr)))
names(ioF) <- c('svrid','attr')

maxU <- tapply(ioF$attr,ioF$svrid,max)

divU <- seq(0,100,10)
staU <- data.frame(svrid = names(maxU),
                   maxoU = as.numeric(maxU),
                   maxU = cut(as.numeric(maxU),divU,include.lowest = T))

tf <- merge(tmp.f,staU,by.x = 'svr_id',by.y = 'svrid')
tcmdb <- merge(tmp.cmdb,staU,by.x = 'svr_asset_id',by.y = 'svrid')

tfC <- subset(tf,grepl('C',dClass));tfTS <- subset(tf,grepl('TS',dClass))
tcmdbC <- subset(tcmdb,grepl('C',dClass)); tcmdbTS <- subset(tcmdb,grepl('TS',dClass))

# tab1
busyBound <- 100
busyPerc <- data.frame(lowerC = nrow(tfC[tfC$maxoU < busyBound,])/nrow(tcmdbC[tcmdbC$maxoU < busyBound,])*6*100/2.26*1.48,
                       higherC = nrow(tfC[tfC$maxoU >= busyBound,])/nrow(tcmdbC[tcmdbC$maxoU >= busyBound,])*6*100/2.26*1.48,
                       lowerTS = nrow(tfTS[tfTS$maxoU < busyBound,])/nrow(tcmdbTS[tcmdbTS$maxoU < busyBound,])/12*6*100/1.96*1.61,
                       higherTS = nrow(tfTS[tfTS$maxoU >= busyBound,])/nrow(tcmdbTS[tcmdbTS$maxoU >= busyBound,])/12*6*100/1.96*1.61)

staU0 <- staU
staU0$dClass <- tcmdb$dClass[match(staU0$svrid,tcmdb$svr_asset_id)]
staU0$dClass[grepl('TS',staU0$dClass)] <- 'Sserv'
staU0$dClass[grepl('C',staU0$dClass)] <- 'Nserv'
staU0$fClass <- 'Normal'
staU0$fClass[staU0$svrid %in% tf$svr_id] <- 'Failed'
# staU0$class <- paste(staU0$dClass,'(',staU0$fClass,')',sep='')
staU0$class <- paste(staU0$fClass,staU0$dClass,sep=' ')

#画图
p1 <- ggplot(staU0,aes(x = maxoU,color = class,linetype = class)) + stat_ecdf(size = 1.5) +
  xlab('Util') + ylab('') +
  coord_cartesian(xlim = c(-3,103),ylim = c(-0.05,1.05),expand = F) + 
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  guides(color = guide_legend(title=NULL),linetype = guide_legend(title = NULL)) + 
  theme_bw() +
  theme(panel.background = element_rect(color = 'black'),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
        #           panel.grid.major.x = element_blank(),
        
        plot.title = element_text(size = 26,vjust = 1),
        axis.line = element_line(color = 'black'),
        axis.text = element_text(size = 22),
        # axis.text.x = element_text(angle = 40,margin = margin(15)),
        axis.title = element_text(size = 24),
        
        legend.key.width = unit(4,units = 'line'),
        legend.key.height = unit(2,units = 'line'),
        legend.text = element_text(size = 26),
        legend.position = c(0,1),legend.justification = c(0,1),
        legend.background = element_rect(fill = alpha('grey',0.5))
  )
print(p1)
ggsave(file=file.path(dir_data,'sc16','fig6A.eps'), plot=p1, width = 8, height = 6, dpi = 100)



######################################################################################################
#持续时间与故障率
ioU <- factorX(subset(ioUtilMax,svrid %in% tmp.cmdb$svr_asset_id))
ioU$A1 <- ioU$maxLastCount
ioU$A2 <- ioU$maxCount

meanA1 <- tapply(ioU$A1,ioU$svrid,mean)
maxA1 <- tapply(ioU$A1,ioU$svrid,max)
maxtopA1 <- tapply(ioU$A1,ioU$svrid,function(x){x <- sort(x,decreasing = T);mean(x[1:10])})

divU1 <- c(-1,0,1,2,3,6,12,60,120,288)

staU1 <- data.frame(svrid = names(meanA1),
                   meanoA1 = as.numeric(meanA1),
                   maxoA1 = as.numeric(maxA1),
                   maxtopoA1 = as.numeric(maxtopA1),
                   
                   meanA1 = cut(as.numeric(meanA1),divU1,include.lowest = T),
                   maxA1 = cut(as.numeric(maxA1),divU1,include.lowest = T),
                   maxtopA1 = cut(as.numeric(maxtopA1),divU1,include.lowest = T))

tf <- merge(tmp.f,staU1,by.x = 'svr_id',by.y = 'svrid')
tcmdb <- merge(tmp.cmdb,staU1,by.x = 'svr_asset_id',by.y = 'svrid')

tfC <- subset(tf,grepl('C',dClass));tfTS <- subset(tf,grepl('TS',dClass))
tcmdbC <- subset(tcmdb,grepl('C',dClass)); tcmdbTS <- subset(tcmdb,grepl('TS',dClass))

attrNeed <- 'maxA1'
a <- ioAFR(tcmdbC,tfC,attrNeed)
a$class <- 'Nserv'
b <- ioAFR(tcmdbTS,tfTS,attrNeed,diskCount = 12)
b$class <- 'Sserv'
a$AFR <- a$AFR/2.26*1.48
b$AFR <- b$AFR/1.96*1.61
Dura <- rbind(item_order(a,attrNeed),item_order(b,attrNeed))
Dura$maxA1 <- as.numeric(gsub('\\]$|^.*,','',Dura$maxA1))*5

AFR_plot(Dura,'fig6B',0,30)
# ggplot(Dura,aes(x = maxA1,y = AFR,fill = class)) + 
#   geom_bar(stat = 'identity',position = 'dodge')

