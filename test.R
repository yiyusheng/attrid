# 测试用
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
############################################################################################
# T1. data1中三字段分位点
load(file.path(dir_data,'data1Quan.Rda'))
q902 <- data.frame(quan = seq(0,100,0.1),value = q902,cl = 'Read')
q903 <- data.frame(quan = seq(0,100,0.1),value = q903,cl = 'write')
q999 <- data.frame(quan = seq(0,100,0.1),value = q999,cl = 'Util')
q <- rbind(q902,q903,q999)
row.names(q) <- NULL
ggplot(q,aes(x = quan,y = log2(value),group = cl,color = cl)) + geom_line()

# T2.
