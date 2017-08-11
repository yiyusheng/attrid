#@@@ FUNCTION @@@#

#F3.AFR plot
AFR_plot <- function(cm,title){
  cm1 <- subset(cm,sepRate < 100)
  p1 <- ggplot(cm1,aes(x = sepRate,y = AFR)) + 
    geom_bar(stat = 'identity') +
    xlab('Rate of Write Amount in Total Amount (%)') + ylab('Failure Rate (%)') + 
    # scale_y_continuous(breaks = seq(0.6,3,0.2)) + 
    # scale_y_continuous(limits = c(0.4,2.8),oob = rescale_none,breaks = seq(0.4,2.8,0.2)) +
    scale_x_continuous(breaks = seq(0,90,10)) +
    scale_y_continuous(breaks = seq(0,10,1)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          #           panel.grid.major.x = element_blank(),
          
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 22,hjust = 0.5),
          
          # axis.text.x = element_text(angle = 40,margin = margin(15)),
          axis.title = element_text(size = 18),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 18),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), 
         plot=p1, width = 8, height = 6, dpi = 100)
  p1
}

#F4.io plot
io_plot <- function(io,title){
  # 画图fig3A
  io$dClassA <- io$dClass;
  io$dClassA[grepl('TS',io$dClass)] <- 'Sserv'
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
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')),
         plot=p1, width = 8, height = 6, dpi = 100)
  p1
}

# F5. plot correlation between disk age and rwRate
AFR_AgeRate_plot <- function(cm,title){
  cm1 <- subset(cm,sepRate < 100)
  p1 <- ggplot(cm1,aes(x = sepRate,y = AFR,group = shTime,linetype = shTime)) + 
    geom_line() +
    # stat_smooth(aes(x = sepRate), se = F, method = "lm", formula = y ~ poly(x, 5))+
    xlab('Rate of Write Amount in Total Amount (%)') + ylab('Failure Rate (%)') + 
    scale_x_continuous(breaks = seq(0,100,10)) +
    scale_y_continuous(breaks = seq(0,10,1)) +
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
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), 
         plot=p1, width = 8, height = 6, dpi = 100)
  p1
}

# F6.AFR for TBN and rate
AFR_TBNrate_plot <- function(cm,title){
  cm1 <- subset(cm,sepRate < 100)
  p1 <- ggplot(cm1,aes(x = sepRate,y = AFR,group = TBNcut,linetype = TBNcut)) + 
    # geom_line() + 
    stat_smooth(aes(x = sepRate), se = F, method = "lm", formula = y ~ poly(x, 5))+
    xlab('Rate of Write Amount in Total Amount (%)') + ylab('Failure Rate (%)') + 
    scale_x_continuous(breaks = seq(0,100,10)) +
    scale_y_continuous(breaks = seq(0,10,1)) +
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
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), 
         plot=p1, width = 8, height = 6, dpi = 100)
  p1
}

# F7.read/write rate and disk age
rwR_plot <- function(cm,title,sepId){
  cmBar <- melt(table(cm$shTime,cm[[sepId]]))
  names(cmBar) <- c('shTime','Ratio','count')
  cm$shTimeCut <- as.numeric(gsub('\\(.*,|]','',cut(cm$shiptimeToRight,seq(0,5,0.01))))

  
  p1 <- ggplot() + geom_bar(data = cmBar,aes(x = shTime,y = count,fill = Ratio),
                            position = 'fill',stat = 'identity') +
    # stat_ecdf(data = subset(cm,shiptimeToRight <= 5.5),aes(x = shiptimeToRight-0.5,linetype = 'CDF')) +
    xlab('Age(years)') + ylab('Percentage (%)') + 
    scale_x_continuous(breaks = seq(0,5,1)) +
    # scale_y_continuous(breaks = seq(0,100,10)) +
    guides(fill = guide_legend(title=NULL)) + guides(linetype = guide_legend(title=NULL))+
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 22,hjust = 0.5),
          axis.title = element_text(size = 18),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 18),
          legend.position = 'top',
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), 
         plot=p1, width = 8, height = 6, dpi = 100)
  p1
}

# F8. Age - failure rate and rwRate
Age_Fr_RwRate_plot <- function(dt,title,diskcount = 1){
  p <-ggplot(dt,aes(x = shTime,y = AFR)) + 
    geom_bar(aes(fill = factor(sepRateCutFit)),stat = 'identity',position = 'dodge') +
    guides(fill = guide_legend(title = NULL)) +
    xlab('Age(years)') + ylab('Failure Rate(%)') + 
    scale_x_continuous(breaks = seq(0,6,1)) + scale_y_continuous(breaks = seq(0,15,1)) +
    theme_bw() +
    theme(axis.text = element_text(size = 22),
          legend.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          legend.position = 'top',
          
          panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'))
  print(p)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), 
         plot=p, width = 8, height = 6, dpi = 100)
  
}