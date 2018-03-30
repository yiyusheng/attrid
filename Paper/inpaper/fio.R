rm(list = ls());source('~/rhead');setwd(file.path(dir_c,'Disk_Workload/Paper'));
source('dir_func.R')

result.fio <- read.table(file.path(dir_data,'result_fio'),skip = 1,header = F,sep = '\t')
names(result.fio) <- c('rand_itv','fio_itv','time','sta_itv','bw_read','bw_write','util','bw')
result.fio$randomness <- with(result.fio,rand_itv/fio_itv)

ggplot(result.fio,aes(x=randomness,y=util))+geom_point()+geom_line(aes(group=fio_itv,color=factor(fio_itv)))
ggplot(result.fio,aes(x=randomness,y=bw))+geom_point()+geom_line(aes(group=fio_itv,color=factor(fio_itv)))
