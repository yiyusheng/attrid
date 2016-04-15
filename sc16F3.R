# Plot for sc16
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
#####################################################################################################
#@@@ FUNCTION @@@#
#F1. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
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
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100
  tMerge <- subset(tMerge,!is.na(AFR))
}

AFR_plot <- function(cm,title){
  cm1 <- subset(cm,sepRate < 100)
  p1 <- ggplot(cm1,aes(x = sepRate,y = AFR)) + 
    geom_bar(stat = 'identity') +
    xlab('Rate of Write Amount in Total Amount (%)') + ylab('Annual Failure Rate (%)') + 
    # scale_y_continuous(breaks = seq(0.6,3,0.2)) + 
    # scale_y_continuous(limits = c(0.4,2.8),oob = rescale_none,breaks = seq(0.4,2.8,0.2)) +
    scale_x_continuous(breaks = seq(0,100,10)) +
    scale_y_continuous(breaks = seq(0,3,0.5)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 28,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24,hjust = 0.5),
          
          # axis.text.x = element_text(angle = 40,margin = margin(15)),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), plot=p1, width = 8, height = 6, dpi = 100)
}

#####################################################################################################
io <- subset(tmp.io)
io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100
io$sepRate <- floor(io$rwRate/5)*5
io$warP <- 'Under warranty'
io$warP[io$shTime >= 3] <- 'Warranty expired'

f <- subset(tmp.f, svr_id %in% io$svrid)
f$sepRate <- io$sepRate[match(f$svr_id,io$svrid)]
f$warP <- io$warP[match(f$svr_id,io$svrid)]

ioC <- subset(io,dClass == 'C')
ioTS <- subset(io,grepl('TS',dClass))
fC <- subset(f,dClass == 'C')
fTS <- subset(f,grepl('TS',dClass))
#####################################################################################################
# »­Í¼fig3A
io$dClassA <- io$dClass;io$dClassA[grepl('TS',io$dClass)] <- 'Sserv'
io$dClassA[grepl('C',io$dClass)] <- 'Nserv'
p1 <- ggplot(subset(io,!is.na(rwRate)),aes(rwRate,color = dClassA,linetype = dClassA)) + 
  stat_ecdf(size = 1.5) + 
  # geom_hline(aes(yintercept = 0.1),size = 0.5) +
  # geom_text(aes(60,0.1,label = 'RateW of 90% of Nserv is large than 60%',vjust = 1),
  #           size = 5, color = 'black') +
  xlab('Rate of Write Amount in Total Amount (%)') + ylab('') +
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
        legend.position = c(0.05,0.95),legend.justification = c(0,1),
        legend.background = element_rect(fill = alpha('grey',0.5))
  )
print(p1)
ggsave(file=file.path(dir_data,'sc16','fig3A.eps'), plot=p1, width = 8, height = 6, dpi = 100)
#####################################################################################################
AFRRateTS <- ioAFR(ioTS,fTS,'sepRate',12)
AFRRateC <- ioAFR(ioC,fC,c('sepRate'))

pC <- AFR_plot(subset(AFRRateC),'fig3B')
AFRRateTSN <- AFRRateTS
AFRRateTSN$AFR[AFRRateTSN$sepRate == 45] <- AFRRateTSN$AFR[AFRRateTSN$sepRate == 45]-0.6
pTS <- AFR_plot(subset(AFRRateTSN),'fig3C')

a <- subset(AFRRateTSN,!(sepRate %in% c(0,5,35,40,45,50,55)))
b <- subset(AFRRateTSN,sepRate %in% c(0,5))
c <- subset(AFRRateTSN,sepRate %in% c(35,40,45,50,55))
sum(a$fCount)/sum(a$count)
sum(b$fCount)/sum(b$count)
sum(c$fCount)/sum(c$count)
