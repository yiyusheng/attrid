rm(list = ls());source('~/rhead')
dir_dataset <- dir_data15AD
load(file.path(dir_dataset,'b1.Rda'))
DT <- dt_dcast
splitDT <- split(DT,DT$svrid)


load(file.path(dir_data,'failRecord_1407-1506.Rda'))
splitFR <- split(failRecord,failRecord$faultType)
fr <- splitFR[[1]]
