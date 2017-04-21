#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: classify_failure_type.R
#
# Description: classify failure types in order to select typical figure to observe relationship between iops and failure.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-17 16:51:32
#
# Last   modified: 2017-04-17 16:51:34
#
#
#
rm(list = ls());source('~/rhead')
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
FR <- failRecord
splitFR <- split(FR,FR$faultType)

# Tag For all
FT <- FR
FT$fi_tag <- 'unknown'
fi <- list('Ping .* 不可达',
           'agent已经5分钟没有上报数据',
           '出错信息:Read-only file system',
           '逻辑盘号:.*物理槽位:.*磁盘状态:.*原因:.*',
           '盘符:.*物理槽位:.*SN:.*状态:.*',
           '槽位：.*硬盘SN：.*',
           'TPC异常处理流程发起故障处理流程，异常类型为',
           '重启ping检查失败,重启后无法ping通,重启失败.',
           '硬盘红灯',
           '\\[TSSD 坏盘运维\\]',
           '\\[在线换盘\\] IP .*',
           '\\[关机换盘\\] IP .*')
for(s in fi){
  idx <- which(grepl(s,FT$faultInfo) == T)
  # FT$fi_tag[idx] <- gsub("\\\\","",s)
  FT$fi_tag[idx] <- s
}

sta1 <- melt_table(FT$fi_tag,FT$faultType)
sta2 <- melt(table(FT$fi_tag))

# Check
splitA <- split(FT,FT$fi_tag)
a <- smp_df(splitA[[6]],100)
a1 <- splitA[[9]]

# mv figure to examine data.
dir_figure <- file.path(dir_data,'plot_svrid_iops_2015')
dir_figure_examine <- file.path(dir_data,'plot_svrid_iops_2015_check');check_dir(dir_figure_examine)
x <- file.remove(file.path(dir_figure_examine,list.files(dir_figure_examine)))

fname <- data.frame(fn = list.files(dir_figure));fname$svrid <- gsub('.*-|\\.png','',fname$fn)
r <- lapply(1:length(fi),function(i){
  df <- FT[FT$fi_tag == fi[i],]
  idx <- sample(nrow(df),50)
  df <- df[idx,]  
  prefix <- paste('tag',i,'-',sep='')
  fn_copy <- fname$fn[fname$svrid %in% df$svrid]
  from_path <- file.path(dir_figure,fn_copy)
  to_path <- file.path(dir_figure_examine,paste(prefix,fn_copy,sep=''))
  file.copy(from_path,to_path)
  df
})
FT_smp <- do.call(rbind,r)
save(FT,FT_smp,file = file.path(dir_data,'classify_failure_type.Rda'))
