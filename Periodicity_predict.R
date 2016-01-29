# 使用SVM学习已标记数据的周期性，并计算预测精度
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
load(file.path(dir_data,'fourierTransMThH.Rda'))
perioLabel <- read.csv(file.path(dir_data,'failIOLabel902'),header = F,colClasses = 'character')
load(file.path(dir_data,'Perio_smp500Svrid.Rda'))

#@@@ LOCAL FUNCTION @@@#
readline <- function(str){
  n <- nchar(str)
  r <- sapply(1:n,function(x){
    as.numeric(substring(str,x,x))
  })
}

# P1.整理数据
PL <- data.frame(svrid = gsub('.png','',nm$name[nm$smp != 0]),
                 lab902 = unlist(sapply(1:34,function(x){
                  readline(perioLabel[x,])
})))
PL$lab902[PL$lab902 == 2] <- 0
ftrNeed <- c('svrid','F902fst','A902fst','A902Sum',
             'F903fst','A903fst','A903Sum',
             'F999fst','A999fst','A999Sum')
PL <- merge(PL,frT[,ftrNeed],by = 'svrid',all.x = T)
delrow <- apply(is.na(PL[,4:ncol(PL)]),1,any)
PL <- PL[!delrow,]

# P2.建模和预测
require('e1071')
m1 <- tune.svm(lab902 ~ F902fst + A902fst + A902Sum , data = PL, 
#                cross = 10,type = 'C-classification',kernel = "radial",
          cost = 10^(-2:2),gamma = 10^(-6:6))
pred1 <- fitted(m1)
table(pred1,PL$lab902)
r <- data.frame(svrid = PL$svrid,pred = pred1,lab = PL$lab902)
rE1 <- subset(r,pred == 1 & lab == 0)
rE2 <- subset(r,pred == 0 & lab == 1)

file.copy(file.path(dir_data,'FailIOCoarse',paste(rE1$svrid,'.png',sep='')),file.path(dir_data,'tmp1'))



# DEL
# names(perioLabel) <- 'lab'
# perioLabel$lab[perioLabel$lab == '0'] <- '000'
# perioLabel$lab[perioLabel$lab == '1'] <- '111'
# perioLabel$lab902 <- as.numeric(substring(perioLabel$lab,1,1))
# perioLabel$lab903 <- as.numeric(substring(perioLabel$lab,2,2))
# perioLabel$lab999 <- as.numeric(substring(perioLabel$lab,3,3))
# perioLabel$lab <- NULL
# perioLabel$svrid <- factor(svridFIO[1:nrow(perioLabel)])
# perioLabel <- perioLabel[c('svrid','lab902','lab903','lab999')]

file.copy(file.path(dir_data,'FailIOCoarse',paste(rE2$svrid,'.png',sep='')),file.path(dir_data,'tmp2'))
