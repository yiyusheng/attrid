nonzero_fraction <- function(arr){
  round(sum(arr > 0,na.rm = T)/sum(arr >= 0,na.rm = T),digits = 4)
}

sta_nonzero <- function(i){
  fn <- fname[i]
  load(file.path(dir_datatendcastClear,fn))
  r <- aggregate(dt_dcast[,-c(1,2)],list(dt_dcast$svrid),nonzero_fraction)
  names(r)[1] <- 'svrid'
  r
}

sta_eachzero <- function(i){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  dd <- remove_line_byvalue(dt_dcast[,c('svrid','time','util','rps','iopsr','wps','iopsw')])
  
  flag_zero <- expand.grid(c(0,1),c(0,1),c(0,1),c(0,1),c(0,1))
  names(flag_zero) <- c('util','rps','iopsr','wps','iopsw')
  
  r <- mapply(function(z1,z2,z3,z4,z5){
    tmp <- dd
    if(z1 == 1)tmp <- subset(tmp,util == 0)
    if(z2 == 1)tmp <- subset(tmp,rps == 0)
    if(z3 == 1)tmp <- subset(tmp,iopsr == 0)
    if(z4 == 1)tmp <- subset(tmp,wps == 0)
    if(z5 == 1)tmp <- subset(tmp,iopsw == 0)
    tmp <- factorX(tmp)
    if(nrow(tmp) == 0){
      return(data.frame(Group.1 = -1,util = -1,rps = -1,iopsr = -1,wps = -1,iopsw = -1,count = -1,countsid = -1))
    }else{
      arrg <- aggregate(tmp[,-c(1,2)],by = list(rep(1,nrow(tmp))),function(x)sum(x == 0))
      arrg$count <- nrow(tmp)
      arrg$countsid <- length(levels(tmp$svrid))
      return(arrg)
    }
  },flag_zero$util,flag_zero$rps,flag_zero$iopsr,flag_zero$wps,flag_zero$iopsw)
  
  r1 <- data.frame(matrix(unlist(r),byrow = T,nrow = nrow(flag_zero)))
  names(r1) <- c('id',paste(names(flag_zero),'z',sep=''),'count','countsid')
  r2 <- cbind(flag_zero,r1)
  r2$id <- fn
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  r2
}

get_lines <- function(arr){
  subset(sta_ez_valid,util == arr[1] & rps == arr[2] & iopsr == arr[3] & wps == arr[4] & iopsw == arr[5])
}

get_lines_dd <- function(arr){
  tmp <- dt_dcast
  if(arr[1] == 1)tmp <- subset(tmp,util == 0)
  if(arr[2] == 1)tmp <- subset(tmp,rps == 0)
  if(arr[3] == 1)tmp <- subset(tmp,iopsr == 0)
  if(arr[4] == 1)tmp <- subset(tmp,wps == 0)
  if(arr[5] == 1)tmp <- subset(tmp,iopsw == 0)
  factorX(tmp)
}

gen_meaningful_flag <- function(df){
  coltag <- attrName
  colz <- paste(attrName,'z',sep='')
  idx_enable <- df[,coltag] == 0
  
  mf1 <- as.integer(df[,colz] > 0.8)
  mf1[!idx_enable] <- 0
  
  mf2 <- as.integer(df[,colz] < 0.2)
  mf2[mf2 == 1] <- -1
  
  df1 <- create_mirror_df(df,0)
  df1[,colz] <- round(mf1 + mf2)
  df1
}