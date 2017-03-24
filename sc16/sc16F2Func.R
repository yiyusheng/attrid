#@@@ FUNCTION @@@#
# F1.plot
AFR_plot <- function(cm,title){ 
  maxY <- 9
  
  cm1 <- subset(cm,warP == 'Under warranty')
  # cm1$sep9023 <- factor((2^cm1$sep9023)/(2^30))
  p1 <- ggplot(cm1,aes(x = sep9023,y = AFR)) + 
    geom_bar(stat = 'identity') + 
    xlab('Total Number of Bytes (TeraBytes)') + ylab('Failure Rate (%)') + 
    scale_y_continuous(breaks = seq(0,maxY,1),limits = c(0,maxY)) +
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
  # cm2$sep9023 <- factor((2^cm2$sep9023)/(2^30))
  p2 <- ggplot(cm2,aes(x = sep9023,y = AFR)) + 
    geom_bar(stat = 'identity') + 
    xlab('Total Number of Bytes (TeraBytes)') + ylab('Failure Rate (%)') + 
    scale_y_continuous(breaks = seq(0,maxY,1),limits = c(0,maxY)) +
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

ioAFR <- function(io,f,attr,diskCount = 1){
  t1 <- melt(table(io[,attr]))
  t2 <- melt(table(f[,attr]))
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100
  tMerge <- subset(tMerge,!is.na(AFR))
}