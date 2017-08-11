###### FUNCTIONS######
iops_aggragate <- function(df){
  x <- ifelse(sum(grepl('iopsr',names(df))) != 1,df$iopsr <- rowSums(df[,grepl('iopsr',names(df))]),names(df)[grepl('iopsr',names(df))] <- 'iopsr')
  x <- ifelse(sum(grepl('iopsw',names(df))) != 1,df$iopsw <- rowSums(df[,grepl('iopsw',names(df))]),names(df)[grepl('iopsw',names(df))] <- 'iopsw')
  
  df$iops <- rowSums(df[,c('iopsr','iopsw')])
  df$xps <- rowSums(df[,c('rps','wps')])
  
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
  data$value[data$attrid %in% attrid_ps & data$value > 1e6] <- NA
  data$value[data$attrid %in% attrid_iops & data$value > 1e6] <- NA
  data$value[data$attrid == 999 & data$value > 100] <- 100
  
  data
}


iops_dcast_clear <- function(dt_dcast,fn){
  num_id <- data.frame(id = c('a','b','c','d','e','f','g'),
                       num = c(0,2,3,12,13,24,24))
  fnum <- num_id$num[num_id == gsub('\\d.*','',fn)]
  dt_dcast <- dt_dcast[,c('svrid','time','rps','wps','util',
                          paste('iopsr',seq_len(fnum),sep='_'),
                          paste('iopsw',seq_len(fnum),sep='_'))]
  
  
  # R1. Radical Filter: method to remove all anomaly line
  idx_iops <- rowSums(apply(dt_dcast[grepl('iops',names(dt_dcast))],2,function(x)x > 1e6))
  idx_remove <- dt_dcast$rps > 1e6 | dt_dcast$wps > 1e6 | dt_dcast$util > 110 | idx_iops > 0
  cat(sprintf('REMOVE %d[%.8f] lines in function iops_dcast_clear\tfile:%s\n',sum(idx_remove > 0),sum(idx_remove > 0)/length(idx_remove > 0),fn))
  dt_dcast <- dt_dcast[!idx_remove,]
  dt_dcast$util[dt_dcast$util > 100] <- 100
  
  # R2. set all inefficient value to NA
  dt_dcast[grepl('ps|util|iops',names(dt_dcast))][dt_dcast[grepl('ps|util|iops',names(dt_dcast))] < 0] <- NA
  
  dt_dcast
}

get_fname <- function(svrid){
  r_sta$fn[match(svrid,r_sta$svr_id)]
}

filter_badiops_NA <- function(df,attrName,fn = 'd1.Rda'){
  load(file.path(dir_data,'sta_dcastClear_result.Rda'))
  df <- remove_line_byvalue(df[,c('svrid','time',attrName)])
  if(!grepl('a\\d+',fn))df <- factorX(subset(df,!(svrid %in% invalid_iopsw$svrid)))
  df
}

cut_level <- function(arr,cut,f2n = T){
  if(f2n){
    return(fct2num(cut(arr,cut,cut[-length(cut)],right = F)))
  }else{
    return(cut(arr,cut,cut[-length(cut)],right = F))
  }
}

rate_data <- function(df,regstr = 'X\\d+'){
  col_value <- names(df)[grepl(regstr,names(df))]
  df$count <- apply(df[,col_value],1,sum)
  df[,col_value] <- roundX(df[,col_value]/df$count)
  df
}

# F. expand server to disk based on the specified disk number in DT
svrid_expand_disk <- function(DT,freq='numD'){
  DT[rep(row.names(DT),DT[[freq]]),]
}