# 读写周期性对故障率影响的研究
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
load(file.path(dir_data,'fourierTransMThH.Rda'))
#########################################################################################################
# P1.是否有周期性对故障率的影响
frT$dClass <- cmdb$dClass[match(frT$svrid,cmdb$svr_asset_id)]
frT$classs <- 'Normal'
frT$class[frT$svrid %in% data.f$svr_id] <- 'Failure'
ggplot(frT,aes(x = F902fst,fill = class)) + geom_histogram(binwidth = 1)

tmpP1.f <- subset(tmp.f,dClass == 'TS',c('ip','svr_id','dClass','dev_class_id','failShiptime'))
tmpP1.cmdb <- subset(tmp.cmdb,dClass == 'TS',c('svr_asset_id','dev_class_id','ip','shiptimeToLeft','dClass'))

tmpP1.cmdb$Prd902 <- round(frT$F902fst[match(tmpP1.cmdb$svr_asset_id,frT$svrid)])
tmpP1.f$Prd902 <- round(frT$F902fst[match(tmpP1.f$svr_id,frT$svrid)])


Tf <- tableX(tmpP1.f$Prd902)
Tcmdb <- tableX(tmpP1.cmdb$Prd902)
Tcmdb$fcount <- 0
Tcmdb$fcount <- Tf$count[match(Tcmdb$item,Tf$item)]
Tcmdb$rate <- Tcmdb$fcount/Tcmdb$count
Tcmdb$item <- as.numeric(levels(Tcmdb$item)[Tcmdb$item])
ggplot(subset(Tcmdb,count > 100),aes(x = item,y = rate)) + geom_bar(stat = 'identity')
Tcmdb <- Tcmdb[order(Tcmdb$rate),]

# P2.观察计算出来的周期是否准确，有1000台故障机的IO数据，已作图，可以直接看
ioplot <- function(sid){
  data <- subset(failIO,svrid == as.character(sid))
  data$fPoint <- 'Before Failure'
  data$fPoint[data$timeNew > data.f$f_time[data.f$svr_id == sid]] <- 'After Failure'
  p <- ggplot(subset(data,a902 < quantile(data$a902,0.99)),aes(x = timeNew,y = a902,color = fPoint)) + geom_line() +
    xlab('Time (days)') + ylab('Read (kb/s)') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          legend.text = element_text(size = 20),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p)
  data$a902
}
load(file.path(dir_data,'failIO.Rda'))
frTIO <- subset(frT,svrid %in% failIO$svrid,
                c('svrid','F902fst','A902fst','A902Sum','F903fst','A903fst','A903Sum'))
frTIO$A902rate <- frTIO$A902fst/frTIO$A902Sum
frTIO$A903rate <- frTIO$A903fst/frTIO$A903Sum
frTIO$rEqw <- frTIO$F902fst == frTIO$F903fst

frTIO <- frTIO[order(frTIO$A902rate,decreasing = T),]
row.names(frTIO) <- NULL
smpSvrid <- sort(sample(frTIO$svrid,20))
staSmp <- subset(frTIO,svrid %in% smpSvrid,c('svrid','F902fst','A902rate'))
staSmp <- staSmp[order(staSmp$svrid),]

staSmp <- subset(frTIO,abs(F902fst - 1)<0.01 & A902rate > 0.005)
smpSvrid <- staSmp$svrid
staSmp$period <- F;staSmp$corr <- F
count <- 20
r <- ioplot(as.character(smpSvrid[5]))
staSmp[count,c('period','corr')] <- c(F,T)

# P3