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
# F1.plot
AFR_plot <- function(cm,title){ 
  cm1 <- subset(cm,warP == 'Under warranty')
  cm1$sep9023 <- factor((2^cm1$sep9023)/(2^30))
  p1 <- ggplot(cm1,aes(x = sep9023,y = AFR)) + 
    geom_bar(stat = 'identity') + 
    # ggtitle('Under warranty') +
    xlab('Amount of I/O Workload (TeraBytes)') + ylab('Annual Failure Rate (%)') + 
    # scale_y_continuous(breaks = seq(0,1.75,0.25),limits = c(0,1.75)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
#           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 28,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.text.x = element_text(angle = 40,margin = margin(15)),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = 'top',
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)

  cm2 <- subset(cm,warP == 'Warranty expired')
  cm2$sep9023 <- factor((2^cm2$sep9023)/(2^30))
  p2 <- ggplot(cm2,aes(x = sep9023,y = AFR)) + 
    geom_bar(stat = 'identity') + 
    # ggtitle('Warranty expired') +
    xlab('Amount of I/O Workload (TeraBytes)') + ylab('Annual Failure Rate (%)') + 
    # ylim(c(0,8)) +
    # scale_y_continuous(breaks = seq(0,8,1),limits = c(0,8)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 28,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.text.x = element_text(angle = 40,margin = margin(15)),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = 'top',
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p2)
  # p3 <- multiplot(p1,p2,cols = 2)
  # print(p3)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'1.eps',sep='')), plot=p1, width = 8, height = 6, dpi = 100)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'2.eps',sep='')), plot=p2, width = 8, height = 6, dpi = 100)
  list(p1,p2)
}

#F2. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}

#F3. CDF plot
CDF_plot <- function(data,tt,xl){
  data$shTime <- paste(data$shTime + 1,'years',sep=' ')
  data$shTime[data$shTime == '1 years'] <- '1 year'
  
  p <- ggplot(data,aes(acct_9023O,color = factor(shTime),linetype = factor(shTime))) + 
    stat_ecdf(size = 1) + xlab('Amount of I/O Workload (Terabytes)') + ylab('') + 
    coord_cartesian(xlim = xl) +
    scale_y_continuous(breaks = seq(0,1,0.1)) +
    scale_x_continuous(breaks = seq(xl[1],xl[2],1),labels = round(2^(seq(xl[1],xl[2],1)-30),2)) +
    guides(color = guide_legend(title = NULL), linetype = guide_legend((title = NULL))) +
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_text(size = 26,vjust = 1),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 22),
          axis.text.x = element_text(angle = 40,margin = margin(10)),
          axis.title = element_text(size = 24),
          
          legend.key.width = unit(4,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(1,0),legend.justification = c(1,0),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p)
  ggsave(file=file.path(dir_data,'sc16',paste(tt,'.eps',sep='')), plot=p, width = 8, height = 6, dpi = 300)
} 
#####################################################################################################
# Fig1
io <- subset(tmp.io,mean_902 != 0 & mean_903 != 0)
io$acct_9023 <- (io$mean_902 + io$mean_903)*86400*365
io$acct_9023[grepl('TS',io$dClass)] <- io$acct_9023[grepl('TS',io$dClass)]/12
io$acct_9023O <- log2(io$acct_9023)
io$acct_9023N <- io$acct_9023O
io$acct_9023N[io$acct_9023N < 29] <- 28
io$acct_9023N[io$acct_9023N >= 37] <- 37
io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100

io$sep9023 <- floor(io$acct_9023N)
io$warP <- 'Under warranty'
io$warP[io$shTime >= 3] <- 'Warranty expired'

f <- subset(tmp.f, svr_id %in% io$svrid)
f$sep9023 <- io$sep9023[match(f$svr_id,io$svrid)]
f$warP <- 'Under warranty'
f$warP[f$fsTime >= 3] <- 'Warranty expired'

ioC <- subset(io,dClass == 'C')
ioTS <- subset(io,grepl('TS',dClass))
fC <- subset(f,dClass == 'C')
fTS <- subset(f,grepl('TS',dClass))

ioAFR <- function(io,f,attr,diskCount = 1){
  t1 <- melt(table(io[,attr]))
  t2 <- melt(table(f[,attr]))
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100
  tMerge <- subset(tMerge,!is.na(AFR))
}

AFR9023TS <- ioAFR(ioTS,fTS,c('sep9023','warP'),12)
AFR9023C <- ioAFR(ioC,fC,c('sep9023','warP'))
pC <- AFR_plot(subset(AFR9023C),'fig2A')
pTS <- AFR_plot(subset(AFR9023TS),'fig2B')

ioTSN <- subset(ioTS,shTime < 6)
CDF_plot(ioC,'fig2A3',c(25,38))
CDF_plot(ioTSN,'fig2B3',c(25,38))

mean(ioTS$acct_9023[ioTS$shTime <= 1])
mean(ioTS$acct_9023[ioTS$shTime > 1 & ioTS$shTime <= 3])
mean(ioTS$acct_9023[ioTS$shTime > 3])

mean(ioC$acct_9023[ioC$shTime <= 1])
mean(ioC$acct_9023[ioC$shTime > 1 & ioC$shTime <= 3])
mean(ioC$acct_9023[ioC$shTime > 3])