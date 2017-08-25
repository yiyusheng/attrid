# F1. add average for rps/wps/xps ------------------------------------------------------------
add_average <- function(object_data,attr){
  # divide by disk number
  ceiling(object_data[[paste('sum',attr,sep='_')]]/object_data$count/object_data$numD)
}

# F2. add level for rps/wps/xps ------------------------------------------------------------
add_level <- function(object_data,attr,itv,maxlimit){
  # value_limit(log2(fct2num(cut(arr,cut_point,cut_point[-length(cut_point)],include.lowest = T))),minp,maxp)
  ave <- add_average(object_data,attr)
  r <- ceiling(ave/itv)*itv
  r[r>maxlimit] <- maxlimit
  return(r)
}

# F3. add average and level for rps,wps and xps ------------------------------------------------------------
add_average_bandwidth <- function(object_data,attr_bw,itv=0){
  object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
  object_data$sum_xps <- with(object_data,sum_rps+sum_wps)
  for(i in seq_len(nrow(attr_bw))){
    ave_name <- paste('average',attr_bw$attr[i],sep='_')
    ave_level_name <- paste(ave_name,'level',sep='_')
    object_data[[ave_name]] <- add_average(object_data,attr_bw$attr[i])
    if(itv==0){
      object_data[[ave_level_name]] <- add_level(object_data,attr_bw$attr[i],attr_bw$maxlimit[i]/100,attr_bw$maxlimit[i])
    }else{
      object_data[[ave_level_name]] <- add_level(object_data,attr_bw$attr[i],itv,attr_bw$maxlimit[i])
    }
  }
  return(object_data)
}

# F4. expand server to disk based on the specified disk number in DT ------------------------------------------------------------
svrid_expand_disk <- function(DT,freq='numD'){
  DT[rep(row.names(DT),DT[[freq]]),]
}

# F5. generate failure rate based on a attributes ------------------------------------------------------------
gen_data <- function(object_data,attr,io = io14,f=f201409){
  object_data <- subset(object_data,svrid %in% io$svrid)
  object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
  object_data$age <- cmdbSMP$age[match(object_data$svrid,cmdbSMP$svrid)]
  object_data <- svrid_expand_disk(object_data)
  
  fail_data <- f
  fail_data <- mchAttr(fail_data,object_data,'svrid','svrid',attr)
  fr <- ioAFR(object_data,fail_data,attr = attr)
  fr$ave_age <- 
  
  fr$percentage <- array_rate(fr$count)*100
  fr$percf <- array_rate(fr$fCount)*100
  fr$level <- 'low'
  fr$level[fr$AFR > 5] <- 'high'
  return(list(object_data,fail_data,fr))
}

# F6. generate figure ------------------------------------------------------------
gen_fr <- function(object_data,attr,io = io14,f=f201409,prt=T,countLimit=100){
  list[object_data,fail_data,fr] <- gen_data(object_data,attr,io = io14,f=f201409)
  
  p_count <- ggplot(fr,aes_string(x=attr))+geom_bar(aes(y=percentage),stat = 'identity')+ylab('Percentage(%)')+
    guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  
  p_countF <- ggplot(fr,aes_string(x=attr))+geom_bar(aes(y=percentage),stat = 'identity')+ylab('Percentage(%)')+
    guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  
  p_fr <- ggplot(subset(fr,count>countLimit),aes_string(x = attr)) + geom_bar(aes(y=AFR,fill=level),stat = 'identity')+ylab('Failure Rate(%)')+
    guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  
  if(prt)multiplot(plotlist = list(p_count,p_fr))
  
  names(fr)[1] <- attr
  return(list(fr,p_fr,p_count,p_countF,object_data,fail_data))
}

# F7. AFR calculate ------------------------------------------------------------
ioAFR <- function(io,f,attr,diskCount = 1,timefactor = 6){
  t1 <- melt(table(io[,attr]))
  t2 <- melt(table(f[,attr]))
  if(length(attr) == 1){
    names(t1)[1] <- attr
    names(t2)[1] <- attr
  }
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100*timefactor
  tMerge <- replace_value(tMerge)
  return(tMerge)
}

# F8. Plot the relationship between age/mode/disk number and specified attribute  ------------------------------------------------------------
plot_amd_diff <- function(object_data,attr){
  current_bw <- attr
  object_data$attr<- factor(object_data[[attr]])
  # 8.1 Age CDF ----
  p_age_cdf <- ggplot(object_data,aes(x=age/365,group=attr,color=attr)) + stat_ecdf(size=1) + 
    guides(group = guide_legend(title=current_bw),color=guide_legend(title=current_bw)) + xlab('Age(years)') + ylab('CDF')+
    guides(group = guide_legend(title=NULL),color=guide_legend(title=NULL),fill=guide_legend(title=NULL))+
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.title = element_text(size = 26), legend.position = 'bottom')
  
  # 8.2 Age bar ----
  table_age <- setNames(melt(tapply(object_data$age/365,object_data$attr,mean)),c('attr','age'))
  p_age_bar <- ggplot(table_age,aes(x=attr,y=age)) + geom_bar(stat='identity')+ 
    xlab(current_bw)+ylab('Age(years)')+
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.position = 'bottom')
  
  # 8.3 Model bar ----
  table_model <- setNames(melt_table(object_data$attr,object_data$mainModel),nm = c('class','model','count'))
  table_model$rate <- array_rate(table_model$count)*100
  p_model_bar <- ggplot(table_model,aes(x=class,y=rate,fill=model)) + geom_bar(stat = 'identity',position='fill')+
    xlab(current_bw)+ylab('Disk Model Ratio(%)')+
    guides(group = guide_legend(title=NULL),color=guide_legend(title=NULL),fill=guide_legend(title=NULL))+
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),legend.position = 'bottom')
  
  # 8.4 Disk number bar ----
  table_numD <- setNames(melt_table(object_data$attr,object_data$numD),nm = c('attr','numD','count'))
  table_numD$rate <- array_rate(table_numD$count)*100
  p_numD_bar <- ggplot(table_numD,aes(x=attr,y=rate,fill=factor(numD))) + geom_bar(stat = 'identity',position='fill')+
    xlab(current_bw)+ylab('Disk Number Ratio(%)')+
    guides(group = guide_legend(title=NULL),color=guide_legend(title=NULL),fill=guide_legend(title=NULL))+
    theme(axis.text = element_text(size = 24),axis.title = element_text(size = 26),
          legend.text = element_text(size = 26),legend.title = element_text(size = 26), legend.position = 'bottom')
  
  return(list(p_age_cdf,p_age_bar,p_model_bar,p_numD_bar))
}

# F9.convert percentage to fraction based on the quantile table ------------------------------------------------------------
get_quan_percentage <- function(DT,x){
  #get the fraction of duty cycle which is larger than x
  Q <- melt(apply(DT[,-c(1,102)],1,function(arr)sum(arr>=x)))
  Q$svrid <- row.names(Q)
  names(Q) <- c('fraction','svrid')
  row.names(Q) <- NULL
  Q$geqx <- x
  return(Q)
}

# F10. generate the fraction that duty cycle great than x based on the duty cycle value count table ------------------------------------------------------------
get_fraction_on_count <- function(DT,x){
  col_name <- paste('V',x,sep='')
  ind <- which(names(DT)==col_name)
  ind_start <- 2
  ind_end <- 102
  
  DT$fraction <- rowSums(DT[ind:ind_end])/DT$count*100
  Q <- DT[,c('svrid','fraction')]
  Q$threshold <- x
  return(Q)
}
