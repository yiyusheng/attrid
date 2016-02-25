# 测试用
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
# load(file.path(dir_data,'load_ftr_attrid.Rda'))
############################################################################################
# # T1. data1中三字段分位点
# load(file.path(dir_data,'data1Quan.Rda'))
# q902 <- data.frame(quan = seq(0,100,0.1),value = q902,cl = 'Read')
# q903 <- data.frame(quan = seq(0,100,0.1),value = q903,cl = 'write')
# q999 <- data.frame(quan = seq(0,100,0.1),value = q999,cl = 'Util')
# q <- rbind(q902,q903,q999)
# row.names(q) <- NULL
# ggplot(q,aes(x = quan,y = log2(value),group = cl,color = cl)) + geom_line()

# T2. 频谱图
# load(file.path(dir_data,'failIOSample.Rda'))
# svrid_cpl <- tapply(failIO$svrid,factor(failIO$svrid),length)
# svrid_full <- names(svrid_cpl)[as.numeric(svrid_cpl) == 17280]
# failIOSample <- factorX(subset(failIO,svrid %in% sample(svrid_full,100)))
# save(failIOSample,file = file.path(dir_data,'failIOSample.Rda'))
# failIO <- failIOSample
# 
# data <- failIO$a902[failIO$svrid == levels(failIO$svrid)[4]]   #数据选择
# data <- subset(failIO,svrid %in% levels(failIO$svrid)[4])
# 
# #FT之后三个字段最高的三种频率
# maxFreq <- function(sid){
#   L <- 3
# #   sprintf(print(sid))
#   data <- subset(failIO,svrid == sid)
#   #计算
#   fftR <- abs(fft(data$a902))[2:(N/2)]
#   fftW <- abs(fft(data$a903))[2:(N/2)]
#   fftU <- abs(fft(data$a999))[2:(N/2)]
#   #数据整理
#   df <- data.frame(f = f[2:(N/2)],fftR,fftW,fftU);
#   Freq <- c(df$f[order(df$fftR,decreasing = T)][1:L],
#             df$f[order(df$fftW,decreasing = T)][1:L],
#             df$f[order(df$fftU,decreasing = T)][1:L],
#             df$fftR[order(df$fftR,decreasing = T)][1:L],
#             df$fftW[order(df$fftW,decreasing = T)][1:L],
#             df$fftU[order(df$fftU,decreasing = T)][1:L])
# }
# 
# #参数设置与计算
# fs <- 1/300;N <- 17280;
# n <- 0:(N-1);t <- n/fs;
# f <- n*fs/N
# sidSet <- levels(failIO$svrid)[1:10]
# R <- data.frame(svrid = sidSet,t(sapply(sidSet,maxFreq)))
# R[,2:10] <- 1/R[,2:10]/86400
# row.names(R) <- NULL
# 
# load(file.path(dir_data,'fourierTransMThH.Rda'))
# R1 <- subset(frT,svrid %in% R$svrid)
# 
# # T3.svm尝试
# simData=function(radius,width,distance,sample_size)
# {
#   aa1=runif(sample_size/2)
#   aa2=runif(sample_size/2)
#   rad=(radius-width/2)+width*aa1
#   theta=pi*aa2
#   x=rad*cos(theta)
#   y=rad*sin(theta)
#   label=1*rep(1,length(x))
#   
#   x1=rad*cos(-theta)+rad
#   y1=rad*sin(-theta)-distance
#   label1=-1*rep(1,length(x1))
#   
#   n_row=length(x)+length(x1)
#   data=matrix(rep(0,3*n_row),nrow=n_row,ncol=3)
#   data[,1]=c(x,x1)
#   data[,2]=c(y,y1)
#   data[,3]=c(label,label1)
#   
#   data
#   
# }
# dataSim=simData(radius=10,width=6,distance=-6,sample_size=3000)
# colnames(dataSim)<-c("x","y","label")
# dataSim<-as.data.frame(dataSim)
# 
# require('e1071')
# m1 <- svm(label ~x*y, data =dataSim,cross=10,type="C-classification",kernel="sigmoid")
# m1
# summary(m1)
# pred1<-fitted(m1)
# table(pred1,dataSim[,3])
# 
# # T4. 傅里叶变换学习
# xs <- seq(-2*pi,2*pi,pi/100)
# wave.1 <- sin(3*xs)
# wave.2 <- sin(10*xs)
# par(mfrow = c(1, 2))
# plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
# plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
# 
# wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
# plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)
# 
# wave.4 <- wave.3
# wave.4[wave.3>0.5] <- 0.5
# plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)
# 
# repeat.xs     <- seq(-2*pi,0,pi/100)
# wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
# plot(xs,wave.3,type="l"); title("Repeating pattern")
# points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)
# 
# # T5.文件重命名以放入同一个文件夹中查看
# dir_rename1 <- 'D:/Data/attrid/FailIOCoarse/test200-read/Week'
# dir_rename2 <- 'D:/Data/attrid/FailIOCoarse/test200-write/Week'
# dir_rename3 <- 'D:/Data/attrid/FailIOCoarse/test200-util/Week'
# 
# names1 <- list.files(dir_rename1)
# names2 <- list.files(dir_rename2)
# names3 <- list.files(dir_rename3)
# 
# newnames1 <- gsub('Week','Week-Read',names1)
# newnames2 <- gsub('Week','Week-Write',names2)
# newnames3 <- gsub('Week','Week-Util',names3)
# 
# file.rename(file.path(dir_rename1,names1),
#             file.path(dir_rename1,newnames1))
# 
# file.rename(file.path(dir_rename2,names2),
#             file.path(dir_rename2,newnames2))
# 
# file.rename(file.path(dir_rename3,names3),
#             file.path(dir_rename3,newnames3))

# T4.时域与频域数据观察
# load(file.path(dir_data,'failIO.Rda'))
# svrCount <- tapply(failIO$svrid,factor(failIO$svrid),length)
# svr902 <- tapply(failIO$a902,factor(failIO$svrid),)
# svrCount <- data.frame(svrid = names(svrCount),count = as.numeric(svrCount))
# data <- subset(failIO,svrid %in% sample(svrCount$svrid[svrCount$count == 17280 & quantile(a902,0.5) > 0],200))
# data <- factorX(data)
# row.names(data) <- NULL
# save(data,file = file.path(dir_data,'freqFieldWatch200.Rda'))

load(file.path(dir_data,'freqFieldWatch200.Rda'))
fs <- 1/300
N <- 17280

#对数值和fft作图
mtplot <- function(sid){
  sidp <- gsub('.*_','',sid)
  print(sid)
  x <- subset(data,svrid == sidp,c('a999','timeNew'))
  names(x) <- c('value','time')
  x$value[x$value < 0] <- 0
  dataVo <- data.frame(time = x$time,value = x$value)
  x <- x[x$value <= quantile(x$value,0.99),]
  N <- length(x$value);n <- 0:(N-1);t <- n/fs;
  f <- n*fs/N;fft <- abs(fft(x$value))
  f <- f[2:(N/2)];fft <- fft[2:(N/2)]
  dataV <- data.frame(time = x$time,value = x$value)
  dataF <- data.frame(freq = f,fft)
  p1 <- ggplot(dataV,aes(x = time,y = value)) + 
    geom_line() + ggtitle(paste(sid,'time field',sep='_'))
  p2 <- ggplot(dataF,aes(x = freq,y = fft)) + 
    geom_line(stat = 'identity') + ggtitle(paste(sid,'freq field',sep='_'))
  png(filename = file.path(dir_data,'FailIOCoarse','test200-util',paste(sid,'.jpg',sep = '')),
      width = 800, height = 600, bg = "white")
  multiplot(p1,p2,cols = 2)
  dev.off()
  dataF <- dataF[order(dataF$fft,decreasing = T),];
  dataF$days <- 1/dataF$freq/86400
  dataF$round <- round(dataF$days)
  dataF$absR <- abs(dataF$days - dataF$round)
  dataF$realPeriod <- dataF$absR <= 0.05
  dataF$fftRate <- dataF$fft/sum(dataF$fft)*100
  dataFS <- subset(dataF,days > 1/3 & days < 10 & fft > 0 & realPeriod == T)
  return(list(dataVo,dataFS,dataF))
}

#对每周的数值作图，共8周又5天
cut <- c(as.POSIXct('2014-06-01'),as.POSIXct('2014-06-08'),as.POSIXct('2014-06-15'),
         as.POSIXct('2014-06-22'),as.POSIXct('2014-06-29'),as.POSIXct('2014-07-06'),as.POSIXct('2014-07-13'),
         as.POSIXct('2014-07-20'),as.POSIXct('2014-07-27'),as.POSIXct('2014-08-01'))
mtwplot <- function(sid,attr){
  sidp <- gsub('.*_','',sid)
  print(sid)
  x <- subset(data,svrid == sidp,c(attr,'timeNew'))
  names(x) <- c('value','time')
  x$value[x$value < 0|is.na(x$value)] <- 0
  dataV <- data.frame(time = x$time,value = x$value)
  p <- list()
  for(i in 1:9){
    if(attr == 'a999'){
      p[[i]] <- ggplot(subset(dataV,time >= cut[i] & time < cut[i+1]),aes(x = time,y = value)) + 
        geom_line() + ggtitle(paste(sid,'Week',i,sep='_'))
    }else{
      dv <- subset(dataV,time >= cut[i] & time < cut[i+1])
      dv$value[dv$value <= 1] <- 1
      p[[i]] <- ggplot(dv,aes(x = time,y = log2(value))) + 
        geom_line() + ggtitle(paste(sid,'Week',i,sep='_')) + ylab('value')
    }
  }
  if(attr == 'a999'){
    tarDir <- 'test200-util'
  }else if(attr == 'a902'){
    tarDir <- 'test200-read'
  }else if(attr == 'a903'){
    tarDir <- 'test200-write'
  }
  png(filename = file.path(dir_data,'FailIOCoarse',tarDir,'Week',paste(sid,'-Week.jpg',sep = '')),
      width = 1200, height = 1200, bg = "white")
  multiplot(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],p[[6]],p[[7]],p[[8]],p[[9]],cols = 1)
  dev.off()
  return()
}

sidset <- paste(1:length(levels(data$svrid)),levels(data$svrid),sep='_')
# data200 <- lapply(sidset,mtplot)
a <- lapply(sidset,mtwplot,c('a902'))
b <- lapply(sidset,mtwplot,c('a903'))
# save(data200,file = file.path(dir_data,'test200-util.Rda'))

# i <- 185;
# dataV <- data200[[i]][[1]];reshp <- data.frame(matrix(dataV$value,288,60));
# reshp$time <- (0:287)/12;reshp <- reshp[,c('time',names(reshp)[1:60])];
# reshp <- reshp[seq(1,nrow(reshp),12),];row.names(reshp) <- NULL
# dataF <- data200[[i]][[3]];dataF <- dataF[order(dataF$freq),];
# 
# list[rV,rFS,rF] <- mtplot(sidset[8]);
# print(rFS[1:5,c('fft','round','fftRate','absR')])

# T5.分析902与903的波动
load(file.path(dir_data,'freqFieldWatch200.Rda'))
data$a902[data$a902 <= 1] <- 1
data$a903[data$a903 <= 1] <- 1
data$a902l <- log2(data$a902)
data$a903l <- log2(data$a903)
data$fluc902 <- 0
data$fluc903 <- 0
data$fluc902[1:(nrow(data)-1)] <- data$a902l[2:nrow(data)] - data$a902l[1:(nrow(data)-1)]
data$fluc903[1:(nrow(data)-1)] <- data$a903l[2:nrow(data)] - data$a903l[1:(nrow(data)-1)]
data$svrid_day <- paste(data$svrid,data$time,sep='_')
r <- tapply(data$fluc902,data$svrid_day,function(x)which(x == max(abs(x))))

fluc <- strsplit(names(r),'_')
fluc <- data.frame(matrix(unlist(fluc),byrow = T,nrow = length(fluc)))
fluc$maxFV <- as.numeric(r)
names(fluc) <- c('svrid','time','maxFV')
