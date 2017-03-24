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
dd <- subsetX(dt_dcast,svrid %in% c('4012','4013','4014','4015'))
splitDD <- split(dd,dd$svrid)
dd_svrid <- subsetX(dd,svrid == levels(dd$svrid)[1] & time > as.p('2014-07-08') & time < as.p('2014-07-09'))
dd_svrid_melt <- melt(dd_svrid[,-c(1,4,5)],id.vars = 'time')
ggplot(dd_svrid_melt,aes(x = time,y = log2(value),linetype = variable,group = variable,color = variable)) + scale_color_brewer(palette = 'Set2')+
  geom_line()+ ylim(c(0,12))
  
