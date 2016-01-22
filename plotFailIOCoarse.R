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

plotOneAttr <- function(data,svrid,predix){
  title <- paste(svrid,'-',predix,sep = '')
  data <- data.frame(time = 1:length(data),attr = data)
  p <- ggplot(data,aes(x = time,y = attr)) + geom_line() +
    xlab('Time (5 Min)') + ylab('unit') + 
    ggtitle(title) + guides(color = guide_legend(title=NULL)) + 
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  ggsave(file=file.path(dir_data,'FailIOCoarse',paste(title,'.png',sep='')), 
         plot=p, width = 16, height = 12, dpi = 100)
}

# P1.数据预处理
failIO <- failIO[order(failIO$svrid,failIO$time,failIO$timepoint),]
failIO <- factorX(failIO)
tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,,'Read'))
tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,,'Write'))
tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,,'Util'))



lapply(levels(failIO$svrid),function(x)plotIO(subset(failIO,svrid == x)))
# library('doParallel')
# ck <- makeCluster(3)
# registerDoParallel(ck)
# foreach(i = levels(failIO$svrid)[1:12]) %dopar% plotIO(subset(failIO,svrid == i))
# stopCluster(ck)
