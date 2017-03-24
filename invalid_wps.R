# collect wps == 0
rm(list = ls());source('~/rhead');require(plyr)

get_sid_invalid_wps <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\t%s START!!!\n',date(),fn))
  target_svrid <- subset(r_sta_day,fn == fname[i])
  splitTS <- split(target_svrid,target_svrid$svrid)
  load(file.path(dir_datatendcastClear,fn))
  r <- lapply(splitTS,function(df){
    subset(dt_dcast,svrid %in% df$svrid[1] & time %in% df$time)
  })
  cat(sprintf('[%s]\t%s END!!!\n',date(),fn))
  do.call(rbind,r)
}

fname <- list.files(dir_datatendcastClear)
load(file.path(dir_data,'sta_dcastClear.Rda'))
r_sta_day$fn <- r_sta_svrid$fn[match(r_sta_day$svrid,r_sta_svrid$svrid)]
idx <- seq_len(length(fname))
r <- foreachX(idx,'get_sid_invalid_wps')
save(r,file = file.path(dir_data,'invalid_wps.Rda'))