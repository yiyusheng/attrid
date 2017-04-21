#!/usr/bin/env python2
# -*- coding: utf-8 -*-
# Filename: failure_record_comparison.R
#
# Description: compare failure record between [09-13] and [2014]/[2015]. I doult that there are several disks not replacing in [2014]/[2015].
# I would like to filter them. In this file, I target to find the difference to filter them by comparison.
# In fact, [2014] and [2015] do not have the invalid ftype_d1 and ftype_d2.
#
# Copyright (c) 2017, Yusheng Yi <yiyusheng.hust@gmail.com>
#
# Version 1.0
#
# Initial created: 2017-04-20 10:36:52
#
# Last   modified: 2017-04-20 10:36:53
#
#
#

rm(list = ls());setwd('~/Code/R/Disk_Workload/');source('~/rhead');
source('~/Code/R/Load_Data_Config_Failure/loadFunc.R')
#A.[09-13]
load(file.path(dir_dataCF,'uwork2013.Rda'))
#B.[2014_06-09]
load(file.path(dir_dataCF,'uwork2014_06_09.Rda'))
#C1.[2014]
load(file.path(dir_dataCF,'uwork2014.Rda'))
#C2.[2014M]
load(file.path(dir_dataCF,'merge_id_svrid.Rda'));f2014M <- merge_id_svrid;rm(merge_id_svrid);
names(f2014M) <- c('ftime','ftype','finfo','svrid','ip','tag','svrnum'); f2014M <- f2014M[,c('svrid','ftime','ftype','finfo','svrnum','ip','tag')]
#D.[2015]
load(file.path(dir_dataCF,'failRecord_1407-1506.Rda'));f2015 <- failRecord;rm(failRecord) 
names(f2015) <- c('svrid','finfo','ftype','ftime');f2015 <- f2015[,c('svrid','ftime','ftype','finfo')]

# C1. compare B and C1/C2 in order to tell if disk failure of B is included by C1/C2. The Answer is YES. 
f2014_06_09_vm <- factorX(f2014_06_09[check_vm(f2014_06_09$svrid),])
f2014_06_09 <- factorX(f2014_06_09[!check_vm(f2014_06_09$svrid),])

f2014_06_09_no_disk_replacing <- factorX(f2014_06_09[!check_disk_replacement(f2014_06_09),])
f2014_06_09 <- factorX(f2014_06_09[check_disk_replacement(f2014_06_09),])

svrid_ftime_2014_06_09 <- paste(f2014_06_09$svrid,f2014_06_09$ftime,sep='-')
svrid_ftime_2014 <- paste(f2014$svrid,f2014$ftime,sep='-')
length(intersect(svrid_ftime_2014_06_09,svrid_ftime_2014))
idx_dif <- match(setdiff(svrid_ftime_2014_06_09,svrid_ftime_2014),svrid_ftime_2014_06_09)
f2014_06_09_non_intersect <- factorX(f2014_06_09[idx_dif,])

# C2. compare number of failure between [2013] and [2014_06_09]. They are very similar in number. But they are different to [2014] and [2015]
f2013_no_disk_replacing <- factorX(f2013[!check_disk_replacement(f2013),])
f2013 <- factorX(f2013[check_disk_replacement(f2013),])
fnum_month_2013 <- melt(table(cut.POSIXt(f2013$ftime,breaks = 'month')))
fnum_month_2014_06_09 <- melt(table(cut.POSIXt(f2014_06_09$ftime,breaks = 'month')))
fnum_month_2014 <- melt(table(cut.POSIXt(f2014$ftime,breaks = 'month')))
fnum_month_2015 <- melt(table(cut.POSIXt(f2015$ftime,breaks = 'month')))

# C3. When I remove B from C1/C2, there a large part leave. The reserving part is what I'm interesting in. 
# These failure from ym's [2014] is not a real failure resulting in disk replacement.
f2014_subset_0609 <- subsetX(f2014,ftime < as.p('2014-10-01') & ftime >= as.p('2014-06-01'))
svrid_ftime_2014_subset_0609 <- paste(f2014_subset_0609$svrid,f2014_subset_0609$ftime, sep='-')
idx_dif <- match(setdiff(svrid_ftime_2014_subset_0609,svrid_ftime_2014_06_09),svrid_ftime_2014_subset_0609)
# C2. But when I remove B from C1/C2, there a large part leave. The reserving part is what I'm interesting in. 
# These failure from ym's [2014] is not a real failure resulting in disk replacement.
f2014_subset_0609 <- subsetX(f2014,ftime < as.p('2014-10-01') & ftime >= as.p('2014-06-01'))
svrid_ftime_2014_subset_0609 <- paste(f2014_subset_0609$svrid,f2014_subset_0609$ftime, sep='-')
idx_dif <- match(setdiff(svrid_ftime_2014_subset_0609,svrid_ftime_2014_06_09),svrid_ftime_2014_subset_0609)
f2014_non_intersect <- factorX(f2014_subset_0609[idx_dif,])

# C4. When I compare average number of failed server due to bad disk drive.[2014],[2015] and [201409] are all in the level of 1800-1900.
# I doult that ym do not extract failure record but failed servers. Then she exact all failure record of failed servers. 
# But some of them are not a disk replacement. I'd like to check the different part from C1's subset of 06-09 and B.
# If each of these servers are not failed only one time. It proves my guess.
cat(sprintf('Average number of failed server per month:\n[2014]%.2f\t[2015]%.2f\t[2014_06_09]%.2f\n',
            length(levels(f2014$svrid))/12,length(levels(f2015$svrid))/12,length(levels(f2014_06_09$svrid))/4))
table_failsvrid_count_2014 <- melt(table(f2014$svrid))
length(intersect(f2014_non_intersect$svrid,table_failsvrid_count_2014$Var1[table_failsvrid_count_2014$value > 1]))
f2014_non_intersect_single_failure <- subsetX(f2014_non_intersect,svrid %in% table_failsvrid_count_2014$Var1[table_failsvrid_count_2014$value == 1])

# C5. I'd like to start from f2014_06_09. tag all item in [f2014]
char_idx1 <- paste(f2014_non_intersect$svrid,f2014_non_intersect$ftime,sep='-')
char_idx2 <- paste(f2014_06_09_no_disk_replacing$svrid,f2014_06_09_no_disk_replacing$ftime,sep='-')
length(intersect(char_idx1,char_idx2))
f2014_06_09_no_disk_replacing_f2014intersect <- f2014_06_09_no_disk_replacing[match(char_idx1,char_idx2),]

# C6. Maybe f2014 is wrong but f2015 is good. We merge f2014 and f2015 in 201407-201412 to find if f2015 is bad
merge_f2014_f2015 <- merge(subsetX(f2014,ftime > as.p('2014-07-01') & ftime < as.p('2015-01-01')),
                           subsetX(f2015,ftime > as.p('2014-07-01') & ftime < as.p('2015-01-01')),
                           by = c('ftime','finfo','ftype'))
