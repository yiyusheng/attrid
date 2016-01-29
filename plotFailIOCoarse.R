# 将故障机的IO数据作图。并用SVM来学习人工标记的周期结果。
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
load(file.path(dir_data,'failIO.Rda'))
load(file.path(dir_data,'Perio_smp500Svrid.Rda'))
failIO <- subset(failIO,svrid %in% gsub('.png','',nm$name[nm$smp != 0]))
failIO <- failIO[order(failIO$svrid,failIO$time,failIO$timepoint),]
failIO <- factorX(failIO)
#@@@ LOCAL FUNCTION @@@#
# F1.给IO画图

plotOneAttr <- function(data,uv,dv,week = F){
  if (week == T){
    data <- data[1:max(288*7,nrow(data))]
  }
  data <- data.frame(time = 1:length(data),attr = data)
  up <- quantile(data,uv/100,na.rm = T)
  down <- quantile(data,dv/100,na.rm = T)
  data <- subset(data,attr <= up & attr >= down)
  p <- ggplot(data,aes(x = time,y = attr)) + geom_line() +
    xlab('Time (5 Min)') + guides(color = guide_legend(title=NULL)) + 
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  p
}

# P1.数据预处理(用全量，去头尾1%，3%，5%的结果，及只用一周的数据的结果)
p902all <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,100,0)})
p9021 <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,99,1)})
p9023 <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,97,3)})
p9025 <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,95,5)})
p902allW <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,100,0,week = T)})
p9021W <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,99,1,week = T)})
p9023W <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,97,3,week = T)})
p9025W <- tapply(failIO$a902,failIO$svrid,function(x){plotOneAttr(x,95,5,week = T)})

p903all <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,100,0)})
p9031 <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,99,1)})
p9033 <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,97,3)})
p9035 <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,95,5)})
p903allW <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,100,0,week = T)})
p9031W <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,99,1,week = T)})
p9033W <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,97,3,week = T)})
p9035W <- tapply(failIO$a903,failIO$svrid,function(x){plotOneAttr(x,95,5,week = T)})

p999all <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,100,0)})
p9991 <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,99,1)})
p9993 <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,97,3)})
p9995 <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,95,5)})
p999allW <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,100,0,week = T)})
p9991W <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,99,1,week = T)})
p9993W <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,97,3,week = T)})
p9995W <- tapply(failIO$a999,failIO$svrid,function(x){plotOneAttr(x,95,5,week = T)})


# library('doParallel')
# ck <- makeCluster(3)
# registerDoParallel(ck)
# foreach(x = sort(names(p902all))) %dopar% function(x){
lapply(sort(names(p902all)),function(x){
  jpeg(filename = file.path(dir_data,'FailIOCoarse','8in1','smp902',paste(x,'.png',sep='')),
       width = 1440, height = 900, bg = "white", res = NA, restoreConsole = TRUE)
  multiplot(p902all[[x]] + ylab('Read(kB/s)') + ggtitle(x),
            p9021[[x]] + ylab('Read(kB/s)'),p9023[[x]] + ylab('Read(kB/s)'),
            p9025[[x]] + ylab('Read(kB/s)'),p902allW[[x]] + ylab('Read(kB/s)'),
            p9021W[[x]] + ylab('Read(kB/s)'),p9023W[[x]] + ylab('Read(kB/s)'),
            p9025W[[x]] + ylab('Read(kB/s)'),cols = 4)
  dev.off()
  
  jpeg(filename = file.path(dir_data,'FailIOCoarse','8in1','smp903',paste(x,'.png',sep='')),
       width = 1440, height = 900, bg = "white", res = NA, restoreConsole = TRUE)
  multiplot(p903all[[x]] + ylab('Write(kB/s)') + ggtitle(x),
            p9031[[x]] + ylab('Write(kB/s)'),p9033[[x]] + ylab('Write(kB/s)'),
            p9035[[x]] + ylab('Write(kB/s)'),p903allW[[x]] + ylab('Write(kB/s)'),
            p9031W[[x]] + ylab('Write(kB/s)'),p9033W[[x]] + ylab('Write(kB/s)'),
            p9035W[[x]] + ylab('Write(kB/s)'),cols = 4)
  dev.off()
  
  jpeg(filename = file.path(dir_data,'FailIOCoarse','8in1','smp999',paste(x,'.png',sep='')),
       width = 1440, height = 900, bg = "white", res = NA, restoreConsole = TRUE)
  multiplot(p999all[[x]] + ylab('Util(%)') + ggtitle(x),
            p9991[[x]] + ylab('Util(%)'),p9993[[x]] + ylab('Util(%)'),
            p9995[[x]] + ylab('Util(%)'),p999allW[[x]] + ylab('Util(%)'),
            p9991W[[x]] + ylab('Util(%)'),p9993W[[x]] + ylab('Util(%)'),
            p9995W[[x]] + ylab('Util(%)'),cols = 4)
  dev.off()
})

# lapply(sort(names(p902All)),function(x){
#   jpeg(filename = file.path(dir_data,'FailIOCoarse','FailIOa902',paste(x,'.jpg',sep = '')),
#        width = 800, height = 600, bg = "white", res = NA, restoreConsole = TRUE)
#   print(plot902List[[x]] + ylab('Read(kB/s)') + ggtitle(names(plot902List[x])))
#   dev.off()
# 
#   jpeg(filename = file.path(dir_data,'FailIOCoarse','FailIOa903',paste(x,'.jpg',sep = '')),
#        width = 800, height = 600, bg = "white", res = NA, restoreConsole = TRUE)
#   print(plot903List[[x]] + ylab('Write(kB/s)') + ggtitle(names(plot902List[x])))
#   dev.off()
# 
#   jpeg(filename = file.path(dir_data,'FailIOCoarse','FailIOa999',paste(x,'.jpg',sep = '')),
#        width = 800, height = 600, bg = "white", res = NA, restoreConsole = TRUE)
#   print(plot999List[[x]] + ylab('Utile(%)') + ggtitle(names(plot902List[x])))
#   dev.off()
# })

# P2.文件重命名
# nm <- data.frame(name = paste(sort(names(plot902List)),'.png',sep = ''))
# nm$newName <- paste(1:nrow(nm),nm$name,sep='_')
# file.rename(file.path(dir_data,'FailIOCoarse','FailIOa902',nm$name),
#             file.path(dir_data,'FailIOCoarse','FailIOa902',nm$newName))
# file.rename(file.path(dir_data,'FailIOCoarse','FailIOa903',nm$name),
#             file.path(dir_data,'FailIOCoarse','FailIOa903',nm$newName))
# file.rename(file.path(dir_data,'FailIOCoarse','FailIOa999',nm$name),
#             file.path(dir_data,'FailIOCoarse','FailIOa999',nm$newName))

# P3.随机取500个机器，精确的标记周期性
# idx<- sort(sample(1:nrow(nm),500))
# nm$smp <- 0
# nm$smp[idx] <- 1:500
# nm$smpName <- ''
# nm$smpName[idx] <- paste(1:500,nm$newName[idx],sep='_')
# save(nm,file = file.path(dir_data,'Perio_smp500Svrid.Rda'))
# file.copy(file.path(dir_data,'FailIOCoarse','FailIOa902',nm$newName[idx]),
#           file.path(dir_data,'FailIOCoarse','smp902',nm$smpName[idx]))
# file.copy(file.path(dir_data,'FailIOCoarse','FailIOa903',nm$newName[idx]),
#           file.path(dir_data,'FailIOCoarse','smp903',nm$smpName[idx]))
# file.copy(file.path(dir_data,'FailIOCoarse','FailIOa999',nm$newName[idx]),
#           file.path(dir_data,'FailIOCoarse','smp999',nm$smpName[idx]))