###### VARIABLES ######
dirName <- 'Disk_Workload_201406-201407'
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99","#000000")
attrNameAll <- c('util','rps','iopsr','wps','iopsw');attrNameDis <- attrNameAll[c(2,4,1)]
attrNameExtend <- c('util','rps','wps',paste('iopsr_',1:24,sep=''),paste('iopsw_',1:24,sep=''))
dir_code <- paste(dir_c,dirName,sep='')
dir_data <- paste(dir_d,dirName,sep='')

if (osFlag){
  source('D:/Git/R_libs_user/R_custom_lib.R')
}else{
  dir_data15 <- '/home/yiyusheng/Data/Load_Data_2015/mergePartSvrid/'
  dir_data15D <- '/home/yiyusheng/Data/Load_Data_2015/mergePartSvridDcast/'
  dir_data15DC <- '/home/yiyusheng/Data/Load_Data_2015/mergePartSvridDcastClear/'
  dir_data14D <- '/home/yiyusheng/Data/Load_Data_2014/merge_1k/'
  dir_data14DC <- '/home/yiyusheng/Data/Load_Data_2014/merge_1kClear/'
  dir_data14 <- '/home/yiyusheng/Data/Load_Data_2014/merge_5k/'
  dir_data14C <- '/home/yiyusheng/Data/Load_Data_2014/merge_5kClear/'
  source('~/Code/R/R_libs_user/R_custom_lib.R')
  # options('width' = 150)
}

###### PACKAGES ######
require('scales')
require('grid')
require('ggplot2')
require('reshape2')
require('plyr')


