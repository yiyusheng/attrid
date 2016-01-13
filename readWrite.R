# 读与写的不同特性对故障的影响
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
source(file.path(dir_code,'AFR_io_prepare.R'))

#@@@ LOCAL FUNCTION @@@#

############################################################################################
# C1.读写总量与读写比例
ioC1 <- subset(tmp.io,mean_902 != 0 & mean_903 != 0)
ioC1$mean_902 <- log2(ioC1$mean_902*86400*365)
ioC1$mean_903 <- log2(ioC1$mean_903*86400*365)
ioC1$mean_9023 <- log2(2^ioC1$mean_902 + 2^ioC1$mean_903)
ioC1$rwRate <- (2^ioC1$mean_902)/(2^ioC1$mean_9023)


div902 <- seq(10,50,3)
ioC1$cut902 <- cut(ioC1$mean_902,div902)
ioC1$sep902 <- as.numeric(gsub('^\\(|,.*$','',ioC1$cut902))
div903 <- seq(26,50,2)
ioC1$cut903 <- cut(ioC1$mean_903,div903)
ioC1$sep903 <- as.numeric(gsub('^\\(|,.*$','',ioC1$cut903))
div9023 <- seq(26,44,2)
ioC1$cut9023 <- cut(ioC1$mean_9023,div9023)
ioC1$sep9023 <- as.numeric(gsub('^\\(|,.*$','',ioC1$cut9023))


fC1 <- subset(tmp.f, svr_id %in% ioC1$svrid)
fC1$sep902 <- ioC1$sep902[match(fC1$svr_id,ioC1$svrid)]
fC1$sep903 <- ioC1$sep903[match(fC1$svr_id,ioC1$svrid)]
fC1$sep9023 <- ioC1$sep9023[match(fC1$svr_id,ioC1$svrid)]

ioC1C <- subset(ioC1,dClass == 'C')
ioC1TS <- subset(ioC1,dClass == 'TS')
fC1C <- subset(fC1,dClass == 'C')
fC1TS <- subset(fC1,dClass == 'TS')

AFR902C <- data.frame(sep = div902,
                    count = sapply(div902,function(x)sum(ioC1C$sep902 == x)),
                    fcount = sapply(div902,function(x)sum(fC1C$sep902 == x)))
AFR903C <- data.frame(sep = div903,
                     count = sapply(div903,function(x)sum(ioC1C$sep903== x)),
                     fcount = sapply(div903,function(x)sum(fC1C$sep903 == x)))
AFR9023C <- data.frame(sep = div9023,
                      count = sapply(div9023,function(x)sum(ioC1C$sep9023== x)),
                      fcount = sapply(div9023,function(x)sum(fC1C$sep9023 == x)))
AFR902C$AFR <- AFR902C$fcount/AFR902C$count*6
AFR903C$AFR <- AFR903C$fcount/AFR903C$count*6
AFR9023C$AFR <- AFR9023C$fcount/AFR9023C$count*6
AFR902C <- subset(AFR902C,count > 100)
AFR903C <- subset(AFR903C,count > 100)
AFR9023C <- subset(AFR9023C,count > 100)

AFR902TS <- data.frame(sep = div902,
                      count = sapply(div902,function(x)sum(ioC1TS$sep902== x)),
                      fcount = sapply(div902,function(x)sum(fC1TS$sep902 == x)))
AFR903TS <- data.frame(sep = div903,
                      count = sapply(div903,function(x)sum(ioC1TS$sep903== x)),
                      fcount = sapply(div903,function(x)sum(fC1TS$sep903 == x)))
AFR9023TS <- data.frame(sep = div9023,
                       count = sapply(div9023,function(x)sum(ioC1TS$sep9023== x)),
                       fcount = sapply(div9023,function(x)sum(fC1TS$sep9023 == x)))
AFR902TS$AFR <- AFR902TS$fcount/AFR902TS$count*6
AFR903TS$AFR <- AFR903TS$fcount/AFR903TS$count*6
AFR9023TS$AFR <- AFR9023TS$fcount/AFR9023TS$count*6
AFR902TS <- subset(AFR902TS,count > 100)
AFR903TS <- subset(AFR903TS,count > 100)
AFR9023TS <- subset(AFR9023TS,count > 100)

tmp <- AFR902C
ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
ggplot(tmp,aes(x = sep,y = AFR)) + geom_line()

tmp <- AFR903C
ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
ggplot(tmp,aes(x = sep,y = AFR)) + geom_line()

tmp <- AFR9023C
ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
ggplot(tmp,aes(x = sep,y = AFR)) + geom_line()

tmp <- AFR902TS
ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
ggplot(tmp,aes(x = sep,y = AFR)) + geom_line()

tmp <- AFR903TS
ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
ggplot(tmp,aes(x = sep,y = AFR)) + geom_line()

tmp <- AFR9023TS
ggplot(tmp,aes(x = sep,y = count)) + geom_bar(stat = 'identity')
ggplot(tmp,aes(x = sep,y = AFR)) + geom_line()
# write.table(AFR902C,file.path(dir_data,'AFR902C.csv'),quote = F,row.names = F,sep = ',')
# write.table(AFR903C,file.path(dir_data,'AFR903C.csv'),quote = F,row.names = F,sep = ',')
# write.table(AFR902TS,file.path(dir_data,'AFR902TS.csv'),quote = F,row.names = F,sep = ',')
# write.table(AFR903TS,file.path(dir_data,'AFR903TS.csv'),quote = F,row.names = F,sep = ',')
# 读写占比计算
ioC1$rwRateRnd <- round(ioC1$rwRate*20)/20
rwRateTable <- colTableX(ioC1,c('rwRateRnd','sep9023'))
rwRateTable <- cbind(rwRateTable,splitToDF(rwRateTable$item))
rwRateTable$item <- NULL
rwRateTable$rate <- NULL
names(rwRateTable) <- c('count','rate','total')
rwRateTable$rate <- as.numeric(levels(rwRateTable$rate)[rwRateTable$rate])
rwRateTable$total <- as.numeric(levels(rwRateTable$total)[rwRateTable$total])
ggplot(subset(rwRateTable,count > 100),aes(x = rate,y = total,fill = log2(count))) + geom_tile()

