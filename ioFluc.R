# IO波动分析，对服务器上跑出来的IO波动信息与故障率之间的联系进行分析。
# 波动信息包含每台服务器每天的信息
# 包含记录数量，coefficient variable,每天最大变化的索引，最大变化的值，最大变化时的低值
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
load(file.path(dir_data,'ioFluc9023Simp.Rda'))

#@@@ FUNCTION @@@#
# F1.将小于1的值设为1以进行log2的处理
logStand <- function(v){
  v[v < 1] <- 1
  v
}

#@@@ DATA PRE @@@#
# ioFluc$cv902 <- abs(ioFluc$cv902)
# ioFluc$cv903 <- abs(ioFluc$cv903)
# ioFluc <- subset(ioFluc,svrid %in% cmdbio$svr_asset_id)
# ioFluc <- merge(ioFluc,cmdbio[,c('svr_asset_id','dev_class_id','dClass','ip')],
#                 by.x = 'svrid',by.y = 'svr_asset_id',all.x = T)
# ioFluc <-factorX(ioFluc)

#########################################################################################################
# A1.cv与故障的关系
# colNeed <- c('svrid','time','cv902','maxF902','cv903','maxF903')
# ioFlucF <- factorX(subset(ioFluc,svrid %in% tmp.f$svr_id,colNeed))
# ioFlucN <- factorX(subset(ioFluc,!(svrid %in% tmp.f$svr_id),colNeed))
# 
# x <- ioFlucF[ioFlucF$svrid == levels(ioFlucF$svrid)[6],];ggplot(x,aes(cv902)) + geom_histogram(binwidth = 0.01)
# 
# plotF <- tapply(ioFlucF$cv902,ioFlucF$svrid,function(x){
#   ggplot(data.frame(x = x),aes(x)) + geom_histogram()
# })
# 
# load(file.path(dir_data,'freqFieldWatch200.Rda'))
# a <- subset(ioFlucF,cv902 <= -15)
# b <- factorX(subset(data,svrid %in% a$svrid))
# c <- factorX(subset(a,svrid %in% b$svrid))

#A2.每日最大跳变的均值
# me902 <- tapply(ioFluc$maxF902,ioFluc$svrid,function(x){mean(abs(x))})
# me903 <- tapply(ioFluc$maxF903,ioFluc$svrid,function(x){mean(abs(x))})
# max902 <- tapply(ioFluc$maxF902,ioFluc$svrid,function(x){max(abs(x))})
# max903 <- tapply(ioFluc$maxF903,ioFluc$svrid,function(x){max(abs(x))})
# meantop902 <- tapply(ioFluc$maxF902,ioFluc$svrid,function(x){a <- sort(x,decreasing = T);mean(a[1:10])})
# meantop903 <- tapply(ioFluc$maxF903,ioFluc$svrid,function(x){a <- sort(x,decreasing = T);mean(a[1:10])})
# 
# avgMaxF <- data.frame(svrid = names(me902),
#                       mean902 = as.numeric(me902),
#                       max902 = as.numeric(max902),
#                       mean903 = as.numeric(me903),
#                       max903 = as.numeric(max903),
#                       meantop902 = as.numeric(meantop902),
#                       meantop903 = as.numeric(meantop903))
# avgMaxF[,2:7] <- apply(avgMaxF[,2:7],2,logStand)
# avgMaxF[,8:13] <- apply(avgMaxF[,2:7],2,function(x)round(log2(x)))
# names(avgMaxF)[8:13] <- paste(names(avgMaxF[,2:7]),'l',sep='')
# tmp.f <- merge(tmp.f,avgMaxF,by.x = 'svr_id',by.y = 'svrid')
# tmp.cmdb <- merge(tmp.cmdb,avgMaxF,by.x = 'svr_asset_id',by.y = 'svrid')
# #计算故障率
# # C类
# AFR_mean902C <- AFR_attr_notime(tmp.f,tmp.cmdb,'mean902l','mean902l',1,dev = 'C')
# AFR_mean903C <- AFR_attr_notime(tmp.f,tmp.cmdb,'mean903l','mean903l',1,dev = 'C')
# AFR_max902C <- AFR_attr_notime(tmp.f,tmp.cmdb,'max902l','max902l',1,dev = 'C')
# AFR_max903C <- AFR_attr_notime(tmp.f,tmp.cmdb,'max903l','max903l',1,dev = 'C')
# AFR_meantop902C <- AFR_attr_notime(tmp.f,tmp.cmdb,'meantop902l','meantop902l',1,dev = 'C')
# AFR_meantop903C <- AFR_attr_notime(tmp.f,tmp.cmdb,'meantop903l','meantop903l',1,dev = 'C')
# # TS类
# AFR_mean902TS <- AFR_attr_notime(tmp.f,tmp.cmdb,'mean902l','mean902l',12,dev = 'TS')
# AFR_mean903TS <- AFR_attr_notime(tmp.f,tmp.cmdb,'mean903l','mean903l',12,dev = 'TS')
# AFR_max902TS <- AFR_attr_notime(tmp.f,tmp.cmdb,'max902l','max902l',12,dev = 'TS')
# AFR_max903TS <- AFR_attr_notime(tmp.f,tmp.cmdb,'max903l','max903l',12,dev = 'TS')
# AFR_meantop902TS <- AFR_attr_notime(tmp.f,tmp.cmdb,'meantop902l','meantop902l',12,dev = 'TS')
# AFR_meantop903TS <- AFR_attr_notime(tmp.f,tmp.cmdb,'meantop903l','meantop903l',12,dev = 'TS')
# 
# #画图
# # ggplot(subset(AFR_mean902C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# # ggplot(subset(AFR_mean903C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_max902C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_max903C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meantop902C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# # ggplot(subset(AFR_meantop903C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# 
# ggplot(subset(AFR_mean902TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# # ggplot(subset(AFR_mean903TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_max902TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_max903TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meantop902TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meantop903TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')

# A3.计算读写和的数据
ioFluc <- ioFluc9023
ioFluc$cv9023 <- abs(ioFluc$cv902)
ioFluc <- subset(ioFluc,svrid %in% cmdbio$svr_asset_id)
ioFluc <-factorX(ioFluc)

me9023 <- tapply(ioFluc$maxF9023,ioFluc$svrid,function(x){mean(abs(x))})
max9023 <- tapply(ioFluc$maxF9023,ioFluc$svrid,function(x){max(abs(x))})
meSD <- tapply(ioFluc$sd9023,ioFluc$svrid,mean)
maxSD <- tapply(ioFluc$sd9023,ioFluc$svrid,max)
meCV <- tapply(ioFluc$cv9023,ioFluc$svrid,mean)
maxCV <- tapply(ioFluc$cv9023,ioFluc$svrid,max)

avgMaxF <- data.frame(svrid = names(me9023),
                      mean9023 = as.numeric(me9023),
                      max9023 = as.numeric(max9023),
                      meanSD = as.numeric(meSD),
                      maxSD = as.numeric(maxSD),
                      meanCV = as.numeric(meCV),
                      maxCV = as.numeric(maxCV))
l <- ncol(avgMaxF)
avgMaxF[,2:l] <- apply(avgMaxF[,2:l],2,logStand)
avgMaxF[,(l+1):(2*l-1)] <- apply(avgMaxF[,2:l],2,function(x)round(log2(x)))
names(avgMaxF)[(l+1):(2*l-1)] <- paste(names(avgMaxF[,2:l]),'l',sep='')
#处理maxCV
avgMaxF$maxCV <- round(avgMaxF$maxCV)
divCV <- c(0,1,2,4,8,12,max(avgMaxF$maxCV,na.rm = T))
avgMaxF$maxCVd <- cut(avgMaxF$maxCV,divCV,include.lowest = T)
avgMaxF$maxCVd <- gsub('^\\[|^\\(|,.*$','',avgMaxF$maxCVd)
#处理maxCVl
avgMaxF$maxCVl[avgMaxF$maxCVl > 4] <- Inf 

tf <- merge(tmp.f,avgMaxF,by.x = 'svr_id',by.y = 'svrid')
tcmdb <- merge(tmp.cmdb,avgMaxF,by.x = 'svr_asset_id',by.y = 'svrid')


# #画图
# AFR_mean9023C <- AFR_attr_notime(tf,tcmdb,'mean9023l','mean9023l',1,dev = 'C')
# AFR_max9023C <- AFR_attr_notime(tf,tcmdb,'max9023l','max9023l',1,dev = 'C')
# AFR_meanSDC <- AFR_attr_notime(tf,tcmdb,'meanSDl','meanSDl',1,dev = 'C')
# AFR_maxSDC <- AFR_attr_notime(tf,tcmdb,'maxSDl','maxSDl',1,dev = 'C')
# AFR_meanCVC <- AFR_attr_notime(tf,tcmdb,'meanCVl','meanCVl',1,dev = 'C')
# AFR_maxCVC <- AFR_attr_notime(tf,tcmdb,'maxCVl','maxCVl',1,dev = 'C')
# 
# ggplot(subset(AFR_mean9023C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_max9023C,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meanSDC,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_maxSDC,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meanCVC,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_maxCVC,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# 
# 
# AFR_mean9023TS <- AFR_attr_notime(tf,tcmdb,'mean9023l','mean9023l',12,dev = 'TS')
# AFR_max9023TS <- AFR_attr_notime(tf,tcmdb,'max9023l','max9023l',12,dev = 'TS')
# AFR_meanSDTS <- AFR_attr_notime(tf,tcmdb,'meanSDl','meanSDl',12,dev = 'TS')
# AFR_maxSDTS <- AFR_attr_notime(tf,tcmdb,'maxSDl','maxSDl',12,dev = 'TS')
# AFR_meanCVTS <- AFR_attr_notime(tf,tcmdb,'meanCVl','meanCVl',12,dev = 'TS')
# AFR_maxCVTS <- AFR_attr_notime(tf,tcmdb,'maxCVl','maxCVl',12,dev = 'TS')
# 
# ggplot(subset(AFR_mean9023TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_max9023TS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meanSDTS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_maxSDTS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_meanCVTS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')
# ggplot(subset(AFR_maxCVTS,count_io > 100),aes(item,AFR)) + geom_bar(stat = 'identity')

#结论画图
AFR_maxCVC <- AFR_attr_notime(tf,tcmdb,'maxCVl','maxCVl',1,dev = 'C')
AFR_maxCVTS <- AFR_attr_notime(tf,tcmdb,'maxCVl','maxCVl',12,dev = 'TS')
AFR_maxCVCp <- AFR_maxCVC[1:5,]
AFR_maxCVCp$item <- c('[0,1]','[1,2]','[2,4]','[4,8]','[8,16]')
AFR_maxCVTSp <- AFR_maxCVTS[1:5,]
AFR_maxCVTSp$item <- c('[0,1]','[1,2]','[2,4]','[4,8]','[8,16]')
AFR_maxCVp <- rbind(AFR_maxCVCp,AFR_maxCVTSp)
t <- 'Coefficient Of Variation and AFR'
p <- ggplot(subset(AFR_maxCVp,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  xlab('Coefficient Of Variation') + ylab('Annual Failure Rate (%)') + 
  ggtitle(t) + guides(fill = guide_legend(title=NULL)) +
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 25),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0,1),
        legend.justification = c(0,1),
        legend.background = element_rect(fill = alpha('grey',0.5)))
ggsave(file=file.path(dir_data,'ioFluc',paste(t,'.png',sep='')), 
       plot=p, width = 16, height = 12, dpi = 100)