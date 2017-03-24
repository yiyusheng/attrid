###### VARIABLES ######
dirName <- 'attrid'
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7","#CC6666", "#9999CC", "#66CC99","#000000")
attrName <- c('util','rps','iopsr','wps','iopsw')
dir_code <- paste(dir_c,dirName,sep='')
dir_data <- paste(dir_d,dirName,sep='')

if (osFlag){
  source('D:/Git/R_libs_user/R_custom_lib.R')
}else{
  dir_dataten <- '/home/yiyusheng/Data/tencProcess/mergePartSvrid/'
  dir_datatendcast <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcast/'
  dir_datatendcastClear <- '/home/yiyusheng/Data/tencProcess/mergePartSvridDcastClear/'
  source('~/Code/R/R_libs_user/R_custom_lib.R')
  # options('width' = 150)
}

###### PACKAGES ######
require('scales')
require('grid')
require('ggplot2')
require('reshape2')
require('plyr')


