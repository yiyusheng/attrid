# F1.plot
AFR_plot <- function(cm,title){ 
  cm1 <- cm
  cm1$maxCVd <- factor(cm1$maxCVd)
  p1 <- ggplot(cm1,aes(x = factor(maxCVd),y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Coefficient of Variable') + ylab('Annual Failure Rate (%)') + 
    # scale_x_continuous(breaks = floor(min(cm1$maxCVd)):ceiling(max(cm1$maxCVd))) +
    scale_y_continuous(breaks = seq(0,6,1)) +
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
          legend.position = c(0.05,0.95),
          legend.justification = c(0,1),
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