rm(list = ls());source('~/rhead')

#A
load(file.path(dir_datatendcastClear,'d6.Rda'))
data_sid <- subset(dt_dcast,svrid == 2469)
ds_zero <- subset(data_sid,wps == 0)

#B
df <- data.frame(id1 = factor(rep(c(1,2,3),10)),
                 id2 = factor(rep(c(4,5,6,7,8),each = 6)),
                 id3 = factor(rep(c('True','False'),15)),
                 value = round(rnorm(30,10,4)))
aggre_df <- aggregate(df[,4],by = list(df$id1,df$id2,df$id3),sum)
tapply_df <- tapply(df$value,list(df$id1,df$id2,df$id3),sum)
melt_tapply_df <- melt(tapply_df)

#C
load(file.path(dir_datatendcastClear,'b1.Rda'))
a <- subset(dt_dcast,svrid == 1885)
a1 <- factorX(subset(dt_dcast,util == 0 & xps > 100))
qua <- cbind(quantileX(a1$xps),quantileX(a1$iopsr))

library(futile.logger)
flog.threshold(WARN)
flog.info("hello%s",3,name = 'a')
flog.warn('heyhey')

#D
load(file.path(dir_data,'sta_zero.Rda'))
load(file.path(dir_data,'sta_dcastClear_result.Rda'))
load(file.path(dir_data,'sta_dcastClear.Rda'))
sta_ez_rate$iopsr[sta_ez_rate$iopsw == 1] <- 1
sta_ez_rate <- sta_ez_rate[1:12,]

#E
load(file.path(dir_datatendcast,'d1.Rda'))
a <- subset(dt_dcast,wps == 0)

#F
load(file.path(dir_datatendcastClear,'d9.Rda'))
load(file.path(dir_data,'wps_range_d9.Rda'))
dd <- subsetX(dt_dcast,svrid %in% levels(wps_range90$svrid))
splitDD <- split(dd,dd$svrid)

plot_idle_wtn <- function(df){
  startTS <- as.p('2014-08-01');endTS <- max((startTS + 86400*200),df$time)
  dd_svrid <- subsetX(df,time > startTS & time < endTS)
  dd_svrid_melt <- melt(dd_svrid[,-c(1,4,5)],id.vars = 'time')
  p <- ggplot(dd_svrid_melt,aes(x = time,y = log2(value),linetype = variable,group = variable,color = variable)) + scale_color_brewer(palette = 'Set2')+
    geom_line()+ geom_point(aes(shape = variable)) + ylim(c(0,12)) + ggtitle(df$svrid[1])
}

p <- lapply(splitDD,plot_idle_wtn)

png(filename = file.path(dir_data,'plot_idel_wtn_range90.jpg'),width = 1920*4, height = 1080*4)
multiplot(p[[1]],p[[2]],p[[3]],p[[4]],p[[5]],
          p[[6]],p[[7]],p[[8]],p[[9]],p[[10]],
          p[[11]],p[[12]],p[[13]],p[[14]],p[[15]],
          p[[16]],p[[17]],p[[18]],p[[19]],p[[20]],
          p[[21]],p[[22]],p[[23]],cols = 5)
dev.off()

# G
load(file.path(dir_datatendcastClear,'c5.Rda'))
dd <- filter_badiops_NA(dt_dcast,attrName)
dd$sizer <- dd$rps/dd$iopsr;dd$sizer[is.na(dd$sizer) | is.infinite(dd$sizer)] <- 0
dd$sizew <- dd$wps/dd$iopsw;dd$sizew[is.na(dd$sizew) | is.infinite(dd$sizew)] <- 0
quan_size <- apply(dd[,c('sizer','sizew')],2,function(x)roundX(quantileX(x[x != 0])))
quan_size$quan <- as.numeric(gsub('%','',row.names(quan_size)))
ggplot(quan_size,aes(quan)) + geom_line(aes(y = log2(sizer)),color = cbPalette[1]) + geom_line(aes(y = log2(sizew)),color = cbPalette[2])

# H
load(file.path(dir_data,'sca_similar_xps.Rda'))
load(file.path(dir_data,'sta_dcastClear.Rda'))
similar_xps$fn <- r_sta_svrid$fn[match(similar_xps$svrid,r_sta_svrid$svrid)]

load(file.path(dir_datatendcastClear,'b1.Rda'))
dd <- factorX(filter_badiops_NA(dt_dcast,attrName))
dd$flag_idler <- as.numeric(dd$rps < 1) + 2
dd$flag_idlew <- as.numeric(dd$wps < 64)
dd <- subsetX(dd,svrid %in% similar_xps$svrid)
splitDD <- split(dd,dd$svrid)
dd_svrid <- subset(splitDD[[1]],time > as.p('2014-07-01') & time < as.p('2014-07-02'))
table(dd_svrid$flag_idler,dd_svrid$flag_idlew)
ggplot(dd_svrid,aes(x = time)) + geom_line(aes(y = flag_idler),color = 'red') + geom_line(aes(y = flag_idlew),color = 'blue')
