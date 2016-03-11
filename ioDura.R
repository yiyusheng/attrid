# IO利用率分析，对每台机器的999的值与故障之间的关系进行分析
# 并对9023/999的值进行分析

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
load(file.path(dir_data,'freqFieldWatch200.Rda'))

# load(file.path(dir_data,'ioDura1.Rda'));ioDura1 <- ioDura
# load(file.path(dir_data,'ioDura2.Rda'));ioDura2 <- ioDura
# load(file.path(dir_data,'ioDura3.Rda'));ioDura3 <- ioDura
# names(ioDura1)[12:15] <- c('diff902A','diff903A','diff999A','diff9023A')
# names(ioDura2)[12:15] <- c('diff902B','diff903B','diff999B','diff9023B')
# names(ioDura3)[12:15] <- c('diff902C','diff903C','diff999C','diff9023C')
# ioDura <- cbind(ioDura1[,c('svrid','time','count')],ioDura1[,12:15],ioDura2[,12:15],ioDura3[,12:15])
# rm(ioDura1);rm(ioDura2);rm(ioDura3)
#@@@ FUNCTION @@@#

#########################################################################################################
# A1.统计diff以判断哪样的diff可以作为周期性的判断标准.计算完之后存储
# ioDura <- factorX(subset(ioDura,count == 288 & 
#                            !is.na(diff902A) & !is.na(diff903A) & !is.na(diff9023A) & !is.na(diff999A) & 
#                            svrid %in% cmdbio$svr_asset_id))
# ioDura$diff902Al <- log4neg(ioDura$diff902A)
# ioDura$diff903Al <- log4neg(ioDura$diff903A)
# ioDura$diff9023Al <- log4neg(ioDura$diff9023A)
# ioDura$diff999Al <- log4neg(ioDura$diff999A)
# 
# ioDura$diff902Bl <- log4neg(ioDura$diff902B)
# ioDura$diff903Bl <- log4neg(ioDura$diff903B)
# ioDura$diff9023Bl <- log4neg(ioDura$diff9023B)
# ioDura$diff999Bl <- log4neg(ioDura$diff999B)
# 
# ioDura$diff902Cl <- log4neg(ioDura$diff902C)
# ioDura$diff903Cl <- log4neg(ioDura$diff903C)
# ioDura$diff9023Cl <- log4neg(ioDura$diff9023C)
# ioDura$diff999Cl <- log4neg(ioDura$diff999C)
# 
# #计算diff902的特征
# meanD902A <- tapply(ioDura$diff902Al,ioDura$svrid,mean)
# maxD902A <- tapply(ioDura$diff902Al,ioDura$svrid,max)
# sdD902A <- tapply(ioDura$diff902Al,ioDura$svrid,sd)
# cvD902A <- tapply(ioDura$diff902Al,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD902B <- tapply(ioDura$diff902Bl,ioDura$svrid,mean)
# maxD902B <- tapply(ioDura$diff902Bl,ioDura$svrid,max)
# sdD902B <- tapply(ioDura$diff902Bl,ioDura$svrid,sd)
# cvD902B <- tapply(ioDura$diff902Bl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD902C <- tapply(ioDura$diff902Cl,ioDura$svrid,mean)
# maxD902C <- tapply(ioDura$diff902Cl,ioDura$svrid,max)
# sdD902C <- tapply(ioDura$diff902Cl,ioDura$svrid,sd)
# cvD902C <- tapply(ioDura$diff902Cl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# dura902 <- data.frame(svrid = names(meanD902A),
#                       meanDA = as.numeric(meanD902A),
#                       maxDA = as.numeric(maxD902A),
#                       sdDA = as.numeric(sdD902A),
#                       cvDA = as.numeric(cvD902A),
#                       meanDB = as.numeric(meanD902B),
#                       maxDB = as.numeric(maxD902B),
#                       sdDB = as.numeric(sdD902B),
#                       cvDB = as.numeric(cvD902B),
#                       meanDC = as.numeric(meanD902C),
#                       maxDC = as.numeric(maxD902C),
#                       sdDC = as.numeric(sdD902C),
#                       cvDC = as.numeric(cvD902C))
# dura902$cv <- (abs(dura902$cvDA) + abs(dura902$cvDB) + abs(dura902$cvDC))/3
# dura902smp <- subset(dura902,!is.na(cvDC),c('svrid','cv','cvDA','cvDB','cvDC'))
# 
# #计算diff903的特征
# meanD903A <- tapply(ioDura$diff903Al,ioDura$svrid,mean)
# maxD903A <- tapply(ioDura$diff903Al,ioDura$svrid,max)
# sdD903A <- tapply(ioDura$diff903Al,ioDura$svrid,sd)
# cvD903A <- tapply(ioDura$diff903Al,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD903B <- tapply(ioDura$diff903Bl,ioDura$svrid,mean)
# maxD903B <- tapply(ioDura$diff903Bl,ioDura$svrid,max)
# sdD903B <- tapply(ioDura$diff903Bl,ioDura$svrid,sd)
# cvD903B <- tapply(ioDura$diff903Bl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD903C <- tapply(ioDura$diff903Cl,ioDura$svrid,mean)
# maxD903C <- tapply(ioDura$diff903Cl,ioDura$svrid,max)
# sdD903C <- tapply(ioDura$diff903Cl,ioDura$svrid,sd)
# cvD903C <- tapply(ioDura$diff903Cl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# dura903 <- data.frame(svrid = names(meanD903A),
#                       meanDA = as.numeric(meanD903A),
#                       maxDA = as.numeric(maxD903A),
#                       sdDA = as.numeric(sdD903A),
#                       cvDA = as.numeric(cvD903A),
#                       meanDB = as.numeric(meanD903B),
#                       maxDB = as.numeric(maxD903B),
#                       sdDB = as.numeric(sdD903B),
#                       cvDB = as.numeric(cvD903B),
#                       meanDC = as.numeric(meanD903C),
#                       maxDC = as.numeric(maxD903C),
#                       sdDC = as.numeric(sdD903C),
#                       cvDC = as.numeric(cvD903C))
# dura903$cv <- (abs(dura903$cvDA) + abs(dura903$cvDB) + abs(dura903$cvDC))/3
# dura903smp <- subset(dura903,!is.na(cvDC),c('svrid','cv','cvDA','cvDB','cvDC'))
# 
# #计算diff999的特征
# meanD999A <- tapply(ioDura$diff999Al,ioDura$svrid,mean)
# maxD999A <- tapply(ioDura$diff999Al,ioDura$svrid,max)
# sdD999A <- tapply(ioDura$diff999Al,ioDura$svrid,sd)
# cvD999A <- tapply(ioDura$diff999Al,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD999B <- tapply(ioDura$diff999Bl,ioDura$svrid,mean)
# maxD999B <- tapply(ioDura$diff999Bl,ioDura$svrid,max)
# sdD999B <- tapply(ioDura$diff999Bl,ioDura$svrid,sd)
# cvD999B <- tapply(ioDura$diff999Bl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD999C <- tapply(ioDura$diff999Cl,ioDura$svrid,mean)
# maxD999C <- tapply(ioDura$diff999Cl,ioDura$svrid,max)
# sdD999C <- tapply(ioDura$diff999Cl,ioDura$svrid,sd)
# cvD999C <- tapply(ioDura$diff999Cl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# dura999 <- data.frame(svrid = names(meanD999A),
#                       meanDA = as.numeric(meanD999A),
#                       maxDA = as.numeric(maxD999A),
#                       sdDA = as.numeric(sdD999A),
#                       cvDA = as.numeric(cvD999A),
#                       meanDB = as.numeric(meanD999B),
#                       maxDB = as.numeric(maxD999B),
#                       sdDB = as.numeric(sdD999B),
#                       cvDB = as.numeric(cvD999B),
#                       meanDC = as.numeric(meanD999C),
#                       maxDC = as.numeric(maxD999C),
#                       sdDC = as.numeric(sdD999C),
#                       cvDC = as.numeric(cvD999C))
# dura999$cv <- (abs(dura999$cvDA) + abs(dura999$cvDB) + abs(dura999$cvDC))/3
# dura999smp <- subset(dura999,!is.na(cvDC),c('svrid','cv','cvDA','cvDB','cvDC'))
# 
# #计算diff9023的特征
# meanD9023A <- tapply(ioDura$diff9023Al,ioDura$svrid,mean)
# maxD9023A <- tapply(ioDura$diff9023Al,ioDura$svrid,max)
# sdD9023A <- tapply(ioDura$diff9023Al,ioDura$svrid,sd)
# cvD9023A <- tapply(ioDura$diff9023Al,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD9023B <- tapply(ioDura$diff9023Bl,ioDura$svrid,mean)
# maxD9023B <- tapply(ioDura$diff9023Bl,ioDura$svrid,max)
# sdD9023B <- tapply(ioDura$diff9023Bl,ioDura$svrid,sd)
# cvD9023B <- tapply(ioDura$diff9023Bl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# meanD9023C <- tapply(ioDura$diff9023Cl,ioDura$svrid,mean)
# maxD9023C <- tapply(ioDura$diff9023Cl,ioDura$svrid,max)
# sdD9023C <- tapply(ioDura$diff9023Cl,ioDura$svrid,sd)
# cvD9023C <- tapply(ioDura$diff9023Cl,ioDura$svrid,function(x)sd(x)/mean(x))
# 
# dura9023 <- data.frame(svrid = names(meanD9023A),
#                       meanDA = as.numeric(meanD9023A),
#                       maxDA = as.numeric(maxD9023A),
#                       sdDA = as.numeric(sdD9023A),
#                       cvDA = as.numeric(cvD9023A),
#                       meanDB = as.numeric(meanD9023B),
#                       maxDB = as.numeric(maxD9023B),
#                       sdDB = as.numeric(sdD9023B),
#                       cvDB = as.numeric(cvD9023B),
#                       meanDC = as.numeric(meanD9023C),
#                       maxDC = as.numeric(maxD9023C),
#                       sdDC = as.numeric(sdD9023C),
#                       cvDC = as.numeric(cvD9023C))
# dura9023$cv <- (abs(dura9023$cvDA) + abs(dura9023$cvDB) + abs(dura9023$cvDC))/3
# dura9023smp <- subset(dura9023,!is.na(cvDC),c('svrid','cv','cvDA','cvDB','cvDC'))
# 
# save(dura902,dura903,dura999,dura9023,dura902smp,dura903smp,dura999smp,dura9023smp,
#      file = file.path(dir_data,'ioDura_result.Rda'))
load(file.path(dir_data,'ioDura_result.Rda'))
divCV <- c(0,0.01*2^(seq(1:8)),Inf)

ioDuraCV <- subset(dura902,,c('svrid','cv'))
names(ioDuraCV) <- c('svrid','cvD902')
ioDuraCV$cvD903 <- dura903$cv[match(ioDuraCV$svrid,dura903$svrid)]
ioDuraCV$cvD999 <- dura999$cv[match(ioDuraCV$svrid,dura999$svrid)]
ioDuraCV$cvD9023 <- dura9023$cv[match(ioDuraCV$svrid,dura9023$svrid)]
ioDuraCV$cut902 <- cut(ioDuraCV$cvD902,divCV,include.lowest = T)
ioDuraCV$cut903 <- cut(ioDuraCV$cvD903,divCV,include.lowest = T)
ioDuraCV$cut9023 <- cut(ioDuraCV$cvD9023,divCV,include.lowest = T)
ioDuraCV$cut999 <- cut(ioDuraCV$cvD999,divCV,include.lowest = T)
ioDuraCV <- factorX(subset(ioDuraCV,!is.na(cvD903) & !is.na(cvD902) & !is.na(cvD999) & !is.na(cvD9023)))

tf <- merge(tmp.f,ioDuraCV,by.x = 'svr_id',by.y = 'svrid')
tcmdb <- merge(tmp.cmdb,ioDuraCV,by.x = 'svr_asset_id',by.y = 'svrid')

tfC <- subset(tf,grepl('C',dClass));tfTS <- subset(tf,grepl('TS',dClass))
tcmdbC <- subset(tcmdb,grepl('C',dClass)); tcmdbTS <- subset(tcmdb,grepl('TS',dClass))

ggplot(tcmdbTS,aes(cut903)) + geom_histogram()

AFR_cvD902C <- item_order(AFR_attr_notime(tf,tcmdb,'cut902','cut902',1,dev = 'C'))
AFR_cvD903C <- item_order(AFR_attr_notime(tf,tcmdb,'cut903','cut903',1,dev = 'C'))
AFR_cvD999C <- item_order(AFR_attr_notime(tf,tcmdb,'cut999','cut999',1,dev = 'C'))
AFR_cvD9023C <- item_order(AFR_attr_notime(tf,tcmdb,'cut9023','cut9023',1,dev = 'C'))

AFR_cvD902TS <- item_order(AFR_attr_notime(tf,tcmdb,'cut902','cut902',12,dev = 'TS'))
AFR_cvD903TS <- item_order(AFR_attr_notime(tf,tcmdb,'cut903','cut903',12,dev = 'TS'))
AFR_cvD999TS <- item_order(AFR_attr_notime(tf,tcmdb,'cut999','cut999',12,dev = 'TS'))
AFR_cvD9023TS <- item_order(AFR_attr_notime(tf,tcmdb,'cut9023','cut9023',12,dev = 'TS'))


AFR_cvD902 <- rbind(AFR_cvD902C,AFR_cvD902TS)
AFR_cvD903 <- rbind(AFR_cvD903C,AFR_cvD903TS)
AFR_cvD999 <- rbind(AFR_cvD999C,AFR_cvD999TS)
AFR_cvD9023 <- rbind(AFR_cvD9023C,AFR_cvD9023TS)

p1 <- ggplot(subset(AFR_cvD902,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') + 
  xlab('Coefficient of Variation of Mean Read Speed Diff') + ylab('Annual Failure Rate (%)') + 
  ggtitle('Coefficient of Variation of Mean Read Speed Diff and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0,1),
        legend.justification = c(-0.2,1.5),
        legend.background = element_rect(fill = alpha('grey',0.5)))
p2 <- ggplot(subset(AFR_cvD903,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge') +
  xlab('Coefficient of Variation of Mean Write Speed Diff') + ylab('Annual Failure Rate (%)') + 
  ggtitle('Coefficient of Variation of Mean Write Speed Diff and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0,1),
        legend.justification = c(-0.2,1.5),
        legend.background = element_rect(fill = alpha('grey',0.5)))
p3 <- ggplot(subset(AFR_cvD999,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge')+
  xlab('Coefficient of Variation of Mean Util Diff') + ylab('Annual Failure Rate (%)') + 
  ggtitle('Coefficient of Variation of Mean Util Diff and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0,1),
        legend.justification = c(-0.2,1.5),
        legend.background = element_rect(fill = alpha('grey',0.5)))
p4 <- ggplot(subset(AFR_cvD9023,count_io > 100),aes(item,AFR,fill = class)) + 
  geom_bar(stat = 'identity',position = 'dodge')+
  xlab('Coefficient of Variation of Mean Read and Write speed Diff') + ylab('Annual Failure Rate (%)') + 
  ggtitle('Coefficient of Variation of Mean Read and Write speed Diff and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(0,1),
        legend.justification = c(-0.2,1.5),
        legend.background = element_rect(fill = alpha('grey',0.5)))

ggsave(file=file.path(dir_data,'ioDura',paste('Coefficient of Variation of Mean Read Speed Diff and AFR','.png',sep='')), 
       plot=p1, width = 16, height = 12, dpi = 100)
ggsave(file=file.path(dir_data,'ioDura',paste('Coefficient of Variation of Mean Write Speed Diff and AFR','.png',sep='')), 
       plot=p2, width = 16, height = 12, dpi = 100)
ggsave(file=file.path(dir_data,'ioDura',paste('Coefficient of Variation of Mean Util Diff and AFR','.png',sep='')), 
       plot=p3, width = 16, height = 12, dpi = 100)
ggsave(file=file.path(dir_data,'ioDura',paste('Coefficient of Variation of Mean Read and Write speed Diff and AFR','.png',sep='')), 
       plot=p4, width = 16, height = 12, dpi = 100)



























# #引入文件名,进行检验
# ioDuraTest <- factorX(subset(dura902,svrid %in% levels(data$svrid)))
# p <- "D:\\Data\\attrid\\FailIOCoarse\\test200-read\\Week"
# fname <- list.files(path = p)
# fname1 <- gsub('-W.*','',fname)
# fname <- data.frame(n1 = fname1,
#                     n2 = gsub('^.*_','',fname1),
#                     n3 = fname)
# 
# ioDuraTest$svrid <- fname$n1[match(ioDuraTest$svrid,fname$n2)]
# ioDuraTest$id <- as.numeric(gsub('_.*','',ioDuraTest$svrid))
# ioDuraTest <- ioDuraTest[order(ioDuraTest$cv,ioDuraTest$id),]
# # ioDuraTest <- ioDuraTest[order(ioDuraTest$id),]
# row.names(ioDuraTest) <- NULL
# 
# duraFname <- as.character(fname$n3[fname$n1 %in% 
#                         ioDuraTest$svrid[ioDuraTest$cv > -1.1 & ioDuraTest$cv < 1.1]])
# nduraFname <- as.character(fname$n3[!(fname$n3 %in% duraFname)])
# file.copy(file.path('D:\\Data\\attrid\\FailIOCoarse\\test200-read','Week',duraFname),
#           file.path('D:\\Data\\attrid\\FailIOCoarse\\test200-read','duraFname',duraFname))
# file.copy(file.path('D:\\Data\\attrid\\FailIOCoarse\\test200-read','Week',nduraFname),
#           file.path('D:\\Data\\attrid\\FailIOCoarse\\test200-read','nduraFname',nduraFname))
# 
# idxW1 <- c()
# idxW2 <- c(24,39,63,69,96,129,140,180,193)
# ioDuraTest$wType <- 'No Error'
# ioDuraTest$wType[ioDuraTest$id %in% idxW1] <- 'W1'
# ioDuraTest$wType[ioDuraTest$id %in% idxW2] <- 'W2'
# ioDuraTest <- ioDuraTest[order(ioDuraTest$wType),]
# idt1 <- subset(dura902,!is.na(cvDC),c('svrid','cv','cvDA','cvDB','cvDC'))