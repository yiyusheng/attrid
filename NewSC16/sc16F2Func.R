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

ioAFR <- function(io,f,attr,diskCount = 1){
  t1 <- melt(table(io[,attr]))
  t2 <- melt(table(f[,attr]))
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100
  tMerge <- subset(tMerge,!is.na(AFR))
}
# F3. Pearson's coefficient
corFunc <- function(df,down,up,class,mth){
  cor(df$sep9023[df$warP == class & df$sep9023 < up & df$sep9023 >= down],
      df$AFR[df$warP == class & df$sep9023 < up & df$sep9023 >= down],method = mth)
}