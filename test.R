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
load(file.path(dir_data,'failIOSample.Rda'))
# svrid_cpl <- tapply(failIO$svrid,factor(failIO$svrid),length)
# svrid_full <- names(svrid_cpl)[as.numeric(svrid_cpl) == 17280]
# failIOSample <- factorX(subset(failIO,svrid %in% sample(svrid_full,100)))
# save(failIOSample,file = file.path(dir_data,'failIOSample.Rda'))
failIO <- failIOSample

data <- failIO$a902[failIO$svrid == levels(failIO$svrid)[4]]   #数据选择
data <- subset(failIO,svrid %in% levels(failIO$svrid)[4])

#FT之后三个字段最高的三种频率
maxFreq <- function(sid){
  L <- 3
#   sprintf(print(sid))
  data <- subset(failIO,svrid == sid)
  #计算
  fftR <- abs(fft(data$a902))[2:(N/2)]
  fftW <- abs(fft(data$a903))[2:(N/2)]
  fftU <- abs(fft(data$a999))[2:(N/2)]
  #数据整理
  df <- data.frame(f = f[2:(N/2)],fftR,fftW,fftU);
  Freq <- c(df$f[order(df$fftR,decreasing = T)][1:L],
            df$f[order(df$fftW,decreasing = T)][1:L],
            df$f[order(df$fftU,decreasing = T)][1:L],
            df$fftR[order(df$fftR,decreasing = T)][1:L],
            df$fftW[order(df$fftW,decreasing = T)][1:L],
            df$fftU[order(df$fftU,decreasing = T)][1:L])
}

#参数设置与计算
fs <- 1/300;N <- 17280;
n <- 0:(N-1);t <- n/fs;
f <- n*fs/N
sidSet <- levels(failIO$svrid)[1:10]
R <- data.frame(svrid = sidSet,t(sapply(sidSet,maxFreq)))
R[,2:10] <- 1/R[,2:10]/86400
row.names(R) <- NULL

load(file.path(dir_data,'fourierTransMThH.Rda'))
R1 <- subset(frT,svrid %in% R$svrid)

# T3.svm尝试
simData=function(radius,width,distance,sample_size)
{
  aa1=runif(sample_size/2)
  aa2=runif(sample_size/2)
  rad=(radius-width/2)+width*aa1
  theta=pi*aa2
  x=rad*cos(theta)
  y=rad*sin(theta)
  label=1*rep(1,length(x))
  
  x1=rad*cos(-theta)+rad
  y1=rad*sin(-theta)-distance
  label1=-1*rep(1,length(x1))
  
  n_row=length(x)+length(x1)
  data=matrix(rep(0,3*n_row),nrow=n_row,ncol=3)
  data[,1]=c(x,x1)
  data[,2]=c(y,y1)
  data[,3]=c(label,label1)
  
  data
  
}
dataSim=simData(radius=10,width=6,distance=-6,sample_size=3000)
colnames(dataSim)<-c("x","y","label")
dataSim<-as.data.frame(dataSim)

require('e1071')
m1 <- svm(label ~x*y, data =dataSim,cross=10,type="C-classification",kernel="sigmoid")
m1
summary(m1)
pred1<-fitted(m1)
table(pred1,dataSim[,3])

# T4.时域与频域数据观察
# load(file.path(dir_data,'failIO.Rda'))
# svrCount <- tapply(failIO$svrid,factor(failIO$svrid),length)
# svrCount <- data.frame(svrid = names(svrCount),count = as.numeric(svrCount))
# data <- subset(failIO,svrid %in% sample(svrCount$svrid[svrCount$count == 17280],30))
# data <- factorX(data)
# row.names(data) <- NULL
# save(data,file = file.path(dir_data,'freqFieldWatch.Rda'))
load(file.path(dir_data,'freqFieldWatch.Rda'))
fs <- 1/300
N <- 17280

mtplot <- function(sid){
  x <- subset(data,svrid == sid,c('a902','timeNew'))
  names(x) <- c('value','time')
  x <- x[x$value <= quantile(x$value,0.99),]
  x$value[x$value < 0] <- 0
  N <- length(x$value);n <- 0:(N-1);t <- n/fs;
  f <- n*fs/N;fft <- abs(fft(x$value))
  f <- f[2:(N/2)];fft <- fft[2:(N/2)]
  dataV <- data.frame(time = x$time,value = x$value)
  dataF <- data.frame(freq = f,fft)
  p1 <- ggplot(dataV,aes(x = time,y = value)) + geom_line() + ggtitle(sid)
  p2 <- ggplot(dataF,aes(x = freq,y = fft)) + geom_line(stat = 'identity')
  multiplot(p1,p2,cols = 2)
  dataF <- dataF[order(dataF$fft,decreasing = T),];
  dataF$days <- 1/dataF$freq/86400
  dataF$round <- round(dataF$days)
  dataF$absR <- abs(dataF$days - dataF$round)
  dataF$realPeriod <- dataF$absR <= 0.05
  dataF$fftRate <- dataF$fft/sum(dataF$fft)*100
  dataFS <- subset(dataF,days > 1/3 & days < 10 & fft > 0 & realPeriod == T)
  return(list(dataV,dataFS,dataF))
}

sid <- levels(data$svrid)
list[rV,rFS,rF] <- mtplot(sid[7]);print(rFS[1:5,c('fft','round','fftRate','absR')])

# T5. 傅里叶变换学习
xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)

wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)

wave.4 <- wave.3
wave.4[wave.3>0.5] <- 0.5
plot(xs,wave.4,type="l",ylim=c(-1.25,1.25)); title("overflowed, non-linear complex wave"); abline(h=0,lty=3)

repeat.xs     <- seq(-2*pi,0,pi/100)
wave.3.repeat <- 0.5*sin(3*repeat.xs) + 0.25*sin(10*repeat.xs)
plot(xs,wave.3,type="l"); title("Repeating pattern")
points(repeat.xs,wave.3.repeat,type="l",col="red"); abline(h=0,v=c(-2*pi,0),lty=3)
