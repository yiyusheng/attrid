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
    geom_bar(stat = 'identity') + ggtitle('Under warranty') +
    xlab('Amount of I/O workload (TB)') + ylab('Annual Failure Rate (%)') + 
    scale_y_continuous(breaks = seq(0,1.75,0.25),limits = c(0,1.75)) +
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

  cm2 <- subset(cm,warP == 'Warranty expired')
  cm2$sep9023 <- factor((2^cm2$sep9023)/(2^30))
  p2 <- ggplot(cm2,aes(x = sep9023,y = AFR)) + 
    geom_bar(stat = 'identity') + ggtitle('Warranty expired') +
    xlab('Amount of I/O workload (TB)') + ylab('Annual Failure Rate (%)') + ylim(c(0,8)) +
    scale_y_continuous(breaks = seq(0,8,1),limits = c(0,8)) +
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
  p3 <- multiplot(p1,p2,cols = 2)
  print(p3)
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
#####################################################################################################
# Fig1
io <- subset(tmp.io,mean_902 != 0 & mean_903 != 0)
io$acct_9023 <- (io$mean_902 + io$mean_903)*86400*365
io$acct_9023[grepl('TS',io$dClass)] <- io$acct_9023[grepl('TS',io$dClass)]/12
io$acct_9023 <- log2(io$acct_9023)
io$acct_9023[io$acct_9023 < 30] <- 29
io$acct_9023[io$acct_9023 >= 37] <- 37
io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100
# io$mean_902 <- log2(io$mean_902*86400*365)
# io$mean_903 <- log2(io$mean_903*86400*365)

# div9023 <- seq(26,44,1)
# io$cut9023 <- cut(io$acct_9023,div9023)
# io$sep9023 <- as.numeric(gsub('^\\(|,.*$','',io$cut9023))
io$sep9023 <- floor(io$acct_9023)
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
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100*6
  tMerge <- subset(tMerge,!is.na(AFR))
}

AFR9023TS <- ioAFR(ioTS,fTS,c('sep9023','warP'),12)
AFR9023C <- ioAFR(ioC,fC,c('sep9023','warP'))
pC <- AFR_plot(subset(AFR9023C),'fig2A')
pTS <- AFR_plot(subset(AFR9023TS),'fig2B')

