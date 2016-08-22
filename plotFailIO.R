# 将故障机的IO数据作图
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))



#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
load(file.path(dir_data,'failIO.Rda'))

#@@@ LOCAL FUNCTION @@@#
# F1.给IO画图
plotIO <- function(data){
  svrid <- as.character(data$svrid[1])
  data$fPoint <- 'Before Failure'
  data$fPoint[data$timeNew > data.f$f_time[data.f$svr_id == svrid]] <- 'After Failure'
  title902 <- paste(svrid,'-Read',sep = '')
  title903 <- paste(svrid,'-Write',sep = '')
  title999 <- paste(svrid,'-Time',sep = '')
  p902 <- ggplot(data,aes(x = timeNew,y = a902,color = fPoint)) + geom_line() +
    xlab('Time (days)') + ylab('Read (kb/s)') + 
    ggtitle(title902) + guides(color = guide_legend(title=NULL)) + 
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  ggsave(file=file.path(dir_data,'FailIO',paste(title902,'.png',sep='')), 
         plot=p902, width = 16, height = 12, dpi = 100)
  
  p903 <- ggplot(data,aes(x = timeNew,y = a903,color = fPoint)) + geom_line() +
    xlab('Time (days)') + ylab('Write (kb/s)') + 
    ggtitle(title903) + guides(color = guide_legend(title=NULL)) + 
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  ggsave(file=file.path(dir_data,'FailIO',paste(title903,'.png',sep='')), 
         plot=p903, width = 16, height = 12, dpi = 100)
  
  p999 <- ggplot(data,aes(x = timeNew,y = a999,color = fPoint)) + geom_line() +
    xlab('Time (days)') + ylab('CPU cycles used by IO (%)') + 
    ggtitle(title999) + guides(color = guide_legend(title=NULL)) + 
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  ggsave(file=file.path(dir_data,'FailIO',paste(title999,'.png',sep='')), 
         plot=p999, width = 16, height = 12, dpi = 100)
}

# P1.数据预处理
failIO$time <- NULL
failIO$timepoint <- NULL
failIO <- factorX(failIO)
lapply(levels(failIO$svrid),function(x)plotIO(subset(failIO,svrid == x)))
# library('doParallel')
# ck <- makeCluster(3)
# registerDoParallel(ck)
# foreach(i = levels(failIO$svrid)[1:12]) %dopar% plotIO(subset(failIO,svrid == i))
# stopCluster(ck)
