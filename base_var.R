###### VARIABLES ######
dirName <- 'Disk_Workload'
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99","#000000")
colr1 <- c('#41e0d0','#00ced1','#86cefb','#0099cd')
colr2 <- colr1[c(2,4)]
colr3 <- colr1[c(1,3,4)]

attrNameAll <- c('util','rps','iopsr','wps','iopsw');attrNameDis <- attrNameAll[c(2,4,1)]
attrNameExtend <- c('util','rps','wps',paste('iopsr_',1:24,sep=''),paste('iopsw_',1:24,sep=''))
dir_code <- file.path(dir_c,dirName)
dir_data <- file.path(dir_d,dirName)

if (osFlag == 'Windows'){
  source('D:/Git/R_libs_user/R_custom_lib.R')
}else{
  dir_data15 <- '/home/yiyusheng/Data/Load_Data_2015/mergePartSvrid/' # no dcast
  dir_data15AD <- '/home/yiyusheng/Data/Load_Data_2015/attridDcast/' #all iops
  dir_data15ADC <- '/home/yiyusheng/Data/Load_Data_2015/attridDcastClear/' #all iops
  dir_data15ADRC <- '/home/yiyusheng/Data/Load_Data_2015/attridDcastReduceClear/' # sum iops in one column
  
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


