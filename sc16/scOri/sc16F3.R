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
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100*6
  tMerge <- subset(tMerge,!is.na(AFR))
}

AFR_plot <- function(cm,title){
  cm1 <- subset(cm,sepRate < 100)
  p1 <- ggplot(cm1,aes(x = sepRate,y = AFR)) + 
    geom_bar(stat = 'identity') +
    xlab('Proportion of write workload') + ylab('Annual Failure Rate (%)') + 
    scale_y_continuous(breaks = seq(0,5,0.5)) + 
    scale_x_continuous(breaks = seq(0,95,10)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 28,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          # axis.text.x = element_text(angle = 40,margin = margin(15)),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = 'top',
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
# 画图fig3A
io$dClassA <- io$dClass;io$dClassA[grepl('TS',io$dClass)] <- 'Sserv'
io$dClassA[grepl('C',io$dClass)] <- 'Nserv'
p1 <- ggplot(io,aes(rwRate,color = dClassA)) + stat_ecdf(size = 2) +
  xlab('Rate of Write in I/O Workload Amount') + ylab('') +
  scale_y_continuous(breaks = seq(0,1,0.2)) +
  scale_x_continuous(breaks = seq(0,100,10)) +
  guides(color = guide_legend(title=NULL)) + 
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
        
        legend.key.width = unit(1.5,units = 'line'),
        legend.key.height = unit(1.5,units = 'line'),
        legend.text = element_text(size = 26),
        legend.position = 'top',
        legend.background = element_rect(fill = alpha('grey',0.5))
  )
# print(p1)
ggsave(file=file.path(dir_data,'sc16','fig3A.eps'), plot=p1, width = 8, height = 6, dpi = 100)
#####################################################################################################
AFRRateTS <- ioAFR(ioTS,fTS,'sepRate',12)
AFRRateC <- ioAFR(ioC,fC,c('sepRate'))

pC <- AFR_plot(subset(AFRRateC),'fig3B')
pTS <- AFR_plot(subset(AFRRateTS),'fig3C')

