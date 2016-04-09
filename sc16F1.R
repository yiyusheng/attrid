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
  p1 <- ggplot(cm,aes(x = item,y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Disk Age (years)') + ylab('Annual Failure Rate (%)') + 
    scale_x_continuous(breaks = floor(min(cm$item)):ceiling(max(cm$item))) +
    scale_y_continuous(breaks = seq(floor(min(cm$AFR)),4,1)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_line(size = 0.4),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
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

#F2. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}
#####################################################################################################
# Fig1
cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'fsTime','shTime',1,dev = 'C')
cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'fsTime','shTime',12,dev = 'TS')
cm <- rbind(cm1,cm2)
cm <- factorX(subset(cm,!is.na(AFR) & item != '6'))
cm <- classExchg(cm)
cm$item <- cm$item + 1
plotCol <- c('item','class','AFR')
# naFill <- cbind(expand.grid(item = levels(factor(cm$item)),class = levels(factor(cm$class))),AFR = 0)
# cm <- rbind(cm[,plotCol],naFill)
# cm$item <- as.numeric(cm$item)
title <- 'fig1'
AFR_plot(cm,'fig1')


sum(cm$count_f[cm$item < 4 & cm$class == 'Nserv'])/sum(cm$count_io[cm$item < 4 & cm$class == 'Nserv'])*100
sum(cm$count_f[cm$item >= 4 & cm$class == 'Nserv'])/sum(cm$count_io[cm$item >= 4 & cm$class == 'Nserv'])*100
sum(cm$count_f[cm$item < 4 & cm$class == 'Sserv'])/sum(cm$count_io[cm$item < 4 & cm$class == 'Sserv'])/12*100
sum(cm$count_f[cm$item >= 4 & cm$class == 'Sserv'])/sum(cm$count_io[cm$item >= 4 & cm$class == 'Sserv'])/12*100
sum(cm$count_f[cm$class == 'Nserv'])/sum(cm$count_io[cm$class == 'Nserv'])*100
sum(cm$count_f[cm$class == 'Sserv'])/sum(cm$count_io[cm$class == 'Sserv'])/12*100

