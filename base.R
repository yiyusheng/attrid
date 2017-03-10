iops_aggragate <- function(df){
  df$iopsr <- rowSums(df[,grepl('iopsr',names(df))])
  df$iopsw <- rowSums(df[,grepl('iopsw',names(df))])
  df$iops <- df$iopsr + df$iopsw
  df$xps <- df$rps + df$wps
  
  df[,grepl('iops.*_.*',names(df))] <- NULL
  df
}

request_size <- function(df){
  df$rsize <- round(df$rps/df$iopsr,digits = 4)
  df$wsize <- round(df$wps/df$iopsw,digits = 4)
  df$rsize[df$rps == 0] <- 0
  df$wsize[df$wps == 0] <- 0
  df
}

iops_melt_clear <- function(data){
  attrid_ps <- 902:903
  attrid_iops <- 36810:36857
  
  data$value[data$value < 0] <- NA
  data$value[data$attrid %in% attrid_ps & data$value > 1e7] <- NA
  data$value[data$attrid %in% attrid_iops & data$value > 1e5] <- NA
  data$value[data$attrid == 999 & data$value > 100] <- 100
  data
}

iops_dcast_clear <- function(dt_dcast){
  col.na <- which(is.na(names(dt_dcast)))
  if(length(col.na) != 0){
    sid <- factor(unique(dt_dcast$svrid[dt_dcast[,col.na]!=0]))
    dt_dcast <- subset(dt_dcast,!(svrid %in% sid))
    dt_dcast[,col.na] <- NULL
  }
  
  dt_dcast[grepl('ps|util|iops',names(dt_dcast))][dt_dcast[grepl('ps|util|iops',names(dt_dcast))] < 0] <- NA
  dt_dcast$rps[dt_dcast$rps > 1e7] <- NA
  dt_dcast$wps[dt_dcast$wps > 1e7] <- NA
  dt_dcast$util[dt_dcast$util > 100] <- 100
  if(any(grepl('iops',names(dt_dcast)))){
    dt_dcast[grepl('iops',names(dt_dcast))][dt_dcast[grepl('iops',names(dt_dcast))] > 1e5] <- NA
  }
  dt_dcast
}

get_fname <- function(svrid){
  r_sta$fn[match(svrid,r_sta$svr_id)]
}