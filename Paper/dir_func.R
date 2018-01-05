# F1. Generate average bandwidth ------------------------------------
add_average <- function(object_data,attr){
  # [USELESS]add average for rps/wps/xps
  # divide by disk number
  ceiling(object_data[[paste('sum',attr,sep='_')]]/object_data$count/object_data$numD)
}

add_level <- function(object_data,attr,bins,maxlimit){
  # [USELESS]add level for rps/wps/xps
  ave <- add_average(object_data,attr)
  itv <- maxlimit/bins
  r <- ceiling(ave/itv)*itv
  r[r>maxlimit] <- maxlimit
  return(r)
}

add_average_bandwidth <- function(object_data,attr_bw,bins=100){
  # [USELESS]add average and level for rps,wps and xps
  object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
  object_data$sum_xps <- with(object_data,sum_rps+sum_wps)
  for(i in seq_len(nrow(attr_bw))){
    ave_name <- paste('average',attr_bw$attr[i],sep='_')
    ave_level_name <- paste(ave_name,'level',sep='_')
    ave_trunc_name <- paste(ave_name,'trunc',sep='_')
    
    object_data[[ave_name]] <- add_average(object_data,attr_bw$attr[i])
    object_data[[ave_trunc_name]] <- object_data[[ave_name]]
    object_data[[ave_trunc_name]][object_data[[ave_trunc_name]]>attr_bw$maxlimit[i]] <- attr_bw$maxlimit[i]
    object_data[[ave_level_name]] <- add_level(object_data,attr_bw$attr[i],bins,attr_bw$maxlimit[i])
  }
  return(object_data)
}

# F2. expand data, generate failure rate and plot figures ------------------------------------
svrid_expand_disk <- function(DT,freq='numD'){
  # expand server to disk based on the specified disk number in DT
  DT$svrid_old <- DT$svrid
  id <- unlist(lapply(DT[[freq]],function(x)seq_len(x)))
  DT <- DT[rep(row.names(DT),DT[[freq]]),]
  DT$svrid <- factor(paste(DT$svrid,id,sep='-'))
  return(DT)
}

svrid_expand_fail <- function(DT,freq='fcount'){
  # expand server to disk based on the specified disk number in DT
  DT$svrid_old <- DT$svrid
  id <- unlist(lapply(1:nrow(DT),function(i){
    if(DT$numD[i]==1){
      rep(1,DT$fcount[[i]])
    }else{
      seq_len(DT$fcount[[i]])
    }
  }))
  DT <- DT[rep(row.names(DT),DT[[freq]]),]
  DT$svrid <- factor(paste(DT$svrid,id,sep='-'))
  return(DT)
}

ioAFR <- function(io,f,attr,diskCount = 1,timefactor = 6){
  # AFR calculate
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

gen_data <- function(object_data,attr,io = io14,f=f201409,expand=F,rsmp='',add_io = T){
  # generate failure rate based on a attributes
  # expand is only used for expand and no expand generate the failure rate with a expanded data
  if(expand==T){
    object_data <- subset(object_data,svrid %in% io$svrid)
    
    object_data <- mchAttr(object_data,model_svrid,'svrid','svrid',c('numD','mainModel'))
    object_data$age <- cmdbSMP$age[match(object_data$svrid,cmdbSMP$svrid)]
    
    if(add_io){
      object_data$adc <- quantile_dutycycle$mean[match(object_data$svrid,quantile_dutycycle$svrid)]
      object_data$abw <- quan_xps$mean[match(object_data$svrid,quan_xps$svrid)]
      object_data$dcq100 <- quantile_dutycycle$Q100[match(object_data$svrid,quantile_dutycycle$svrid)]
    }
    
    object_data <- svrid_expand_disk(object_data)
    object_data$numD <- factor(object_data$numD,levels = c('1','12'))
    return(object_data)
  }else{
    fail_data <- subset(f,svrid %in% object_data$svrid_old)
    fail_data$numD <- model_svrid$numD[match(fail_data$svrid,model_svrid$svrid)]
    
    table_f <- setNames(melt(table(fail_data$svrid)),nm=c('svrid','count'))
    fail_data <- fail_data[!duplicated(fail_data$svrid),]
    fail_data$fcount <- table_f$count[match(fail_data$svrid,table_f$svrid)] #times of failure happened in a server
    fail_data <- svrid_expand_fail(fail_data,freq = 'fcount')
    
    # We need a descartes set via svrid if svrid happens 2 in f and happanes 4 in object_data
    # then it should happens 2*4=8 in fail_data
    fail_data <- merge(fail_data,object_data[,c('svrid',attr)],by='svrid')
    # fail_data <- mchAttr(fail_data,object_data,'svrid','svrid',attr)
    
    fr <- ioAFR(object_data,fail_data,attr = attr)
    
    fr$percentage <- array_rate(fr$count)*100
    fr$percf <- array_rate(fr$fCount)*100
    
    fr$level <- 'low'
    # fr$level[fr$AFR > 2] <- 'median'
    fr$level[fr$AFR > 5] <- 'high'
    fr$level <- factor(fr$level,levels = c('low','high'))
    
    if(rsmp!='' & is.numeric(object_data[[rsmp]]) & length(attr)==1){
      rsmp_attr <- list2df(tapply(object_data[[rsmp]],object_data[[attr]],mean),n = c('value','attr'))
      fr[[paste('mean',rsmp,sep='_')]] <- rsmp_attr$value[match(fr[[attr]],rsmp_attr$attr)]
    }

    return(list(fr,object_data,fail_data))
  }
}

gen_fr <- function(object_data,attr,io = io14,f=f201409,prt=F,countLimit=100,balanced_binning=T,rsmp='',maxy){
  # generate figure
  list[fr,object_data,fail_data] <- gen_data(object_data,attr,io = io14,f=f201409,expand = F,rsmp=rsmp)
  
  if(balanced_binning){
    fr <- fr[fr[[attr]]!=0,]
    bin_width <- min(fr[[attr]])
    p_fr <- ggplot(subset(fr,count>countLimit),aes_string(x = attr)) + 
      geom_bar(aes(y=AFR,fill=level),stat = 'identity',position=position_nudge(x=-bin_width/2))+ylab('Failure Rate(%)')+
      geom_smooth(aes(y=AFR),color='red',linetype=2,se=F)+
      guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
      gen_theme()
    
    p_count <- ggplot(fr,aes_string(x=attr))+geom_bar(aes(y=percentage),stat = 'identity',position=position_nudge(x=-bin_width/2))+
      ylab('Percentage(%)')+guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
      gen_theme()
    
    p_countF <- ggplot(fr,aes_string(x=attr))+geom_bar(aes(y=percf),stat = 'identity',position=position_nudge(x=-bin_width/2))+
      ylab('Percentage(%)')+guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
      gen_theme()
    
  }else{
    fr$xmi <- c(0,fr[[attr]][-length(fr[[attr]])])
    fr$xma <- fr[,1]-(fr[,1]-fr$xmi)*0.1
    
    p_fr <- ggplot(subset(fr,count>countLimit))+
      geom_rect(aes(xmin=xmi,xmax=xma,ymin=0,ymax=AFR,fill=level),color='black',size=0.05)+ylab('Failure Rate(%)')+xlab(attr)+
      geom_smooth(aes(x=(xma+xmi)/2,y=AFR),span=0.3,color='red',linetype=2)+
      guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +gen_theme()+
      # scale_fill_brewer(palette='Set2') +
      coord_cartesian(ylim=c(0,20)) 
    
    p_count <- ggplot(subset(fr,count>countLimit))+
      geom_rect(aes(xmin=xmi,xmax=xma,ymin=0,ymax=percentage),color='black',size=0.05)+ylab('Percentage(%)')+
      guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
      gen_theme()+
      coord_cartesian(ylim=c(0,maxy*1.1))   
    
    p_countF <- ggplot(subset(fr,count>countLimit))+
      geom_rect(aes(xmin=xmi,xmax=xma,ymin=0,ymax=percf),color='black',size=0.05)+ylab('Percentage(%)')+
      guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
      gen_theme()
  }

  
  
  if(prt)multiplot(plotlist = list(p_count,p_fr))
  names(fr)[1] <- attr
  return(list(fr,p_fr,p_count,p_countF,object_data,fail_data))
}

gen_fr_split <- function(i,attr,splitDTQ=splitDTQ,quan_low=NULL,quan_high=NULL){
  # generate failure rate cut by attr at quantile i and quantile 1-i
  r <- lapply(splitDTQ,function(df){
    if(is.null(quan_low))quan_low <- quantile(df[[attr]],i)
    if(is.null(quan_high))quan_high <- quantile(df[[attr]],1-i)
    df$class <- 'median'
    df$class[df[[attr]]<=quan_low] <- 'low'
    df$class[df[[attr]]>quan_high] <- 'high'
    df$class <- factor(df$class,levels=c('low','median','high'))
    return(list(df,data.frame(level = df$mean_level[1],mean = mean(df[[attr]]),low = quan_low,high = quan_high)))
  })
  DT_class <- do.call(rbind,lapply(r,'[[',1))
  DT_div <- do.call(rbind,lapply(r,'[[',2))
  list[data_fr] <- gen_data(DT_class,c('mean_level','class'),expand = F,rsmp = '')
  # a <- by(DT_class,list(DT_class$mean_level,DT_class$class),function(df)mean(df$age))
  # a1 <- setNames(melt(array(a,dim(a),dimnames(a))),c('mean_level','class','mean_age'))
  # data_fr <- merge(data_fr,a1)
  # perf <- sum(data_fr$percf[data_fr$class=='low'])
  return(list(data_fr,DT_div))
}

gen_result_feature <- function(DT,attr,attr_max=NULL,balanced_binning=T,bins=20,bin_count=1000,has_level=F,rsmp='age',maxy=1.2){
  # get result for a special attr in DT
  attr_level <- paste(attr,'level',sep='_')
  if(is.null(attr_max))attr_max<- quantile(DT[[attr]],0.99,na.rm = T)
  if(has_level==F)DT <- binning_data(DT,attr,attr_max,balanced_binning,bins,bin_count)
  list[data_fr,p_fr,p_count,p_countf,object_data,f_data] <- 
    gen_fr(DT,attr_level,prt=F,balanced_binning=balanced_binning,rsmp=rsmp,maxy=maxy)
  corr <- cor(data_fr[,1],data_fr$AFR)
  cat(sprintf('[%s]\t %s corr:%.4f\tEND!!!\n',date(),attr_level,corr))
  return(list(data_fr,p_fr,p_count,corr,object_data,f_data))
}

gen_result_feature_all <- function(DT,attr,attr_max,attr_flat,attrf_max,balanced_binning,
                                   bins=20,bin_count=5300,resample=F,ob=F,maxy=1.2){
  #all in one
  attr_level <- paste(attr,'level',sep='_')
  if(resample){
    # generate resampled data based on attr_flat to make attr_flat balanced on attr
    cat(sprintf('[%s]\tattr:%s\tattr_flat:%s START !!!\n',date(),attr,attr_flat))
    if(is.numeric(DT_quan[[attr_flat]])){
      trunc_arr <- trunc_level(DT,attr_flat,0,attrf_max,bins,detail=F)
    }else{
      trunc_arr <- DT[[attr_flat]]
    }
    table_arr <- setNames(melt(table(trunc_arr)),nm = c('attr_flat','count'))
    table_arr$rate <- array_rate(table_arr$count)
    
    DT_quan_level <- binning_data(DT=DT,attr=attr,attr_max=attr_max,
                                  balanced_binning=balanced_binning,bins=20,bin_count=bin_count)
    DT_quan_level$id <- seq_len(nrow(DT_quan_level))
    splitDQL <- split(DT_quan_level,DT_quan_level[[attr_level]])
    
    resample_id <- function(splitDQL){
      # resample index for each bin
      r <- lapply(splitDQL,function(df){
        if(is.numeric(df[[attr_flat]])){
          df$arr_trunc <- trunc_level(df,attr_flat,0,attrf_max,bins,detail=F)
          df$arr_trunc_level <- gen_binned_array(df,'arr_trunc',table_arr$attr_flat)
        }else{
          df$arr_trunc <- df[[attr_flat]]
          df$arr_trunc_level <- df[[attr_flat]]
        }
        table_arr$num <- ceiling(table_arr$rate*nrow(df))
        id <- list()
        for(i in seq_len(nrow(table_arr))){
          dfi <- subset(df,arr_trunc_level==table_arr[['attr_flat']][i])
          if(nrow(dfi)==0)next
          id[[i]] <- dfi$id[sample(seq_len(nrow(dfi)),table_arr$num[i],replace = T)]
        }
        id <- unlist(id)
      })
      unlist(r)
    }
    
    id <- sapply(1:3,function(i)resample_id(splitDQL))
    # id <- c(resample_id(splitDQL),resample_id(splitDQL),resample_id(splitDQL),resample_id(splitDQL),resample_id(splitDQL))
    DT_resample <- DT_quan_level[id,]
    list[data_fr,p_fr,p_count,corr,data_ob] <- 
      gen_result_feature(DT=DT_resample,attr=attr,attr_max=attr_max,balanced_binning=balanced_binning,
                         bins=20,bin_count=bin_count,has_level = T,rsmp=attr_flat,maxy=maxy)
    p_factors <- plot_relationship_factors(data_ob,attr_level,balanced_binning=balanced_binning,maxy=maxy)
    cat(sprintf('[%s]\tattr:%s\tattr_flat:%s END !!!\n',date(),attr,attr_flat))
  }else{
    list[data_fr,p_fr,p_count,corr,data_ob] <- 
      gen_result_feature(DT=DT_quan,attr=attr,attr_max=attr_max,balanced_binning=balanced_binning,
                         bins=20,bin_count=bin_count,rsmp=attr_flat,maxy=maxy)
    p_factors <- plot_relationship_factors(data_ob,attr_level,balanced_binning=balanced_binning)
  }
  
  if(resample){
    str <- sprintf('%s\n[rsmp:%s\tbb:%s]',attr,attr_flat,balanced_binning)
  }else{
    str <- sprintf('%s\n[rsmp:%s\tbb:%s]',attr,'No',balanced_binning)
  }
  p_fr <- p_fr+xlab(str)
  p_count <- p_count+xlab(str)
  p_factors <- lapply(p_factors,function(x)x+xlab(str))
  
  if(ob==F){
    return(list(data_fr,p_fr,p_count,corr,p_factors))
  }else{
    return(list(data_fr,p_fr,p_count,corr,p_factors,data_ob))
  }
}

gen_result_feature_all_dopa <- function(i){
    r <- gen_result_feature_all(DT=DT_quan,attr=fct2ori(para$attr[i]),attr_max=para$am[i],attr_flat=fct2ori(para$af[i]),
                                attrf_max=para$ma[i],bins=20,bin_count=para$bc[i],balanced_binning=para$bb[i],
                                resample=para$rsmp[i],ob=para$ob[i],maxy=para$my[i])
}

gen_rect_metric <- function(df,attrx,attry,attrz=NULL){
  # generate data structure for geom_rect
  if(is.null(attrz)){
    df <- df[order(df[[attrx]]),]
  }else{
    df <- df[order(df[[attrx]],df[[attrz]]),]
  }
  
  dfx <- data.frame(x=sort(unique(df[[attrx]])))
  attrx_min <- min(0,dfx$x)
  dfx$xmin <- c(attrx_min,dfx$x[-length(dfx$x)])
  dfx$xmax <- dfx$x
  dfx$xmedian <- with(dfx,(xmax+xmin)/2)
  
  df <- mchAttr(df,dfx,attrx,'x',c('xmin','xmax','xmedian'))
  
  df$ymax <- as.numeric(unlist(tapply(df[[attry]],df[[attrx]],cumsum)))
  df$ymin <- as.numeric(unlist(tapply(df$ymax,df[[attrx]],function(arr){
    attry_min <- min(0,arr)
    c(attry_min,arr[-length(arr)])
  })))
  
  return(df)
}

# F3. Plot figures ------------------------------------
plot_relationship <- function(object_data,attr1,attr2,type1='numeric',type2='numeric',balanced_binning = F,maxy=1.2){
  # plot relationship of attr1 and attr2, attr1(type1) is the x axis and attr2(type2) is the y axis
  # If type2 is numeric, we use the geom_point, the geom_line and the geom_errorbar.
  # If type2 is factor, we use the geom_rect
  
  if(type2=='numeric'){
    table_2attr <- list2df(tapply(object_data[[attr2]],object_data[[attr1]],function(arr)c(mean(arr),sd(arr))),
                                   n = c('mean_attr2','sd_attr2','attr1'))
    if(type1=='numeric')table_2attr$attr1 <- as.numeric(table_2attr$attr1)
    table_2attr_rect <- gen_rect_metric(table_2attr,'attr1','mean_attr2')
    table_2attr_rect$ymin <- with(table_2attr_rect,mean_attr2-sd_attr2)
    # table_2attr_rect$ymin[table_2attr_rect$ymin < 0] <- 0
    table_2attr_rect$ymax <- with(table_2attr_rect,mean_attr2+sd_attr2)
    
    p_rls <- ggplot(table_2attr_rect,aes(x=xmedian,y=mean_attr2))+
      geom_errorbar(aes(ymin=ymin,ymax=ymax))+
      geom_line(color='red',size=0.5)+geom_point(size=2,color='red')+xlab(attr1)+ylab(attr2)+
      gen_theme()
  }else if(type2=='factor'){
    table_2attr <- setNames(melt_table(object_data[[attr1]],object_data[[attr2]]),nm = c('attr1','attr2','count'))
    table_2attr$rate <- array_rate(table_2attr$count)*100
    if(balanced_binning){
      p_rls <- ggplot(table_2attr,aes(x=attr1,y=rate,fill=factor(attr2))) + geom_bar(stat='identity',position='fill')+
        xlab(attr1)+ylab('Percentage(%)')+
        guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
        gen_theme()+theme(legend.position = 'bottom',legend.text = element_text(size = 18))
    }else{
      table_2attr_rect <- gen_rect_metric(table_2attr,'attr1','rate','attr2')
      p_rls <- ggplot(table_2attr_rect)+
        geom_rect(aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=factor(attr2)),color='black',size=0.05)+
        xlab(attr1)+ylab('Percentage(%)')+
        geom_smooth(aes(x=xmedian,y=ymax),span=0.3,color='red',linetype=2)+
        guides(fill = guide_legend(title=NULL),color=guide_legend(title=NULL)) +
        gen_theme()+theme(legend.position = 'bottom',legend.text = element_text(size = 18))+
        coord_cartesian(ylim=c(0,maxy*1.1))      
    }

  }
  return(p_rls)
}



plot_relationship_factors <- function(object_data,attr,balanced_binning=T,maxy=1.2){
  p_adc_errorbar <- plot_relationship(object_data,attr,'adc',type2='numeric',balanced_binning = balanced_binning,maxy=maxy)
  p_abw_errorbar <- plot_relationship(object_data,attr,'abw',type2='numeric',balanced_binning = balanced_binning,maxy=maxy)
  p_dcq100_errorbar <- plot_relationship(object_data,attr,'dcq100',type2='numeric',balanced_binning = balanced_binning,maxy=maxy)
  p_age_errorbar <- plot_relationship(object_data,attr,'age',type2='numeric',balanced_binning = balanced_binning,maxy=maxy)
  p_model_bar <- plot_relationship(object_data,attr,'mainModel',type2='factor',balanced_binning = balanced_binning,maxy=maxy)
  p_numD_bar <- plot_relationship(object_data,attr,'numD',type2='factor',balanced_binning = balanced_binning,maxy=maxy)
  return(list(age=p_age_errorbar,
              adc=p_adc_errorbar,
              abw=p_abw_errorbar,
              # dcq100=p_dcq100_errorbar,
              mainModel=p_model_bar,
              numD=p_numD_bar))
}

# F4. generate fraction feature ------------------------------------
get_quan_percentage <- function(DT,x,leq = T){
  # convert percentage to fraction based on the quantile table
  #get the fraction of duty cycle which is larger than x
  col_Q <- paste('Q',1:100,sep='')
  if(leq){
    flag_Q <- DT[,col_Q]>=x
  }else{
    flag_Q <- DT[,col_Q]<=x
  }
  Q <- melt(rowSums(flag_Q)/100)
  Q$svrid <- row.names(Q)
  names(Q) <- c('fraction','svrid')
  row.names(Q) <- NULL
  Q$geqx <- x
  return(Q)
}

get_fraction_on_count <- function(DT,x){
  # [USELESS]generate the fraction that duty cycle great than x based on the duty cycle value count table
  DT$count <- rowSums(DT[,-1])
  valueV <- as.numeric(gsub('V','',names(DT)[grepl('V',names(DT))]))
  x <- valueV[which.min(abs(valueV-x))]
  
  col_name <- paste('V',c(x,min(valueV),max(valueV)),sep='')
  ind <- which(names(DT)==col_name[1])
  ind_start <- which(names(DT)==col_name[2])
  ind_end <- which(names(DT)==col_name[3])
  
  DT$fraction <- rowSums(DT[ind:ind_end])/DT$count
  Q <- DT[,c('svrid','fraction')]
  Q$threshold <- x
  return(Q)
}

gen_result_fraction <- function(DT,spc_value,bins=100){
  # get result for fraction of value large than special value
  # quanList <- get_fraction_on_count(DT,spc_value)
  quanList <- get_quan_percentage(DT,spc_value)
  quanList <- subset(quanList,svrid %in% io14$svrid)
  quanList$fraction_level <- gen_binned_array(quanList,'fraction',
                                              gen_balanced_binning_point(0,1,bins))
  
  list[data_fr,p_fr,p_count,p_countF,data_ob] <- gen_fr(quanList,attr='fraction_level',prt=F,countLimit = 10)
  data_fr$class <- spc_value
  corr <- with(data_fr,cor(fraction_level,AFR))
  cat(sprintf('[%s]\t %.2f corr:%.4f\tEND!!!\n',date(),spc_value,corr))
  
  list(data_fr,p_fr,p_count,corr,data_ob)
}
# F5. format bandwidth and chopping data ------------------------------------
format_bandwidth <- function(DT,bt=c(4000,5000,9000)*2,bins=100,truncate=F){
  # generate xps, format them and truncate them
  DT$numD <- model_svrid$numD[match(DT$svrid,model_svrid$svrid)]
  
  DT$rps <- DT$rps/DT$numD
  DT$rps[DT$rps<1 & DT$rps>0] <- 1
  
  DT$wps <- DT$wps/DT$numD
  DT <- subset(DT,wps>0)
  DT$wps[DT$wps<1] <- 1
  
  DT$xps <- with(DT,rps+wps)
  
  DT$rps_trunc <- DT$rps
  DT$wps_trunc <- DT$wps
  DT$xps_trunc <- DT$xps
  if(truncate==T){
    DT$rps_trunc[DT$rps>=bt[1]] <- bt[1]
    DT$rps_level <- ceiling(DT$rps_trunc/(bt[1]/bins))*(bt[1]/bins)
    DT$wps_trunc[DT$wps>=bt[2]] <- bt[2]
    DT$wps_level <- ceiling(DT$wps_trunc/(bt[2]/bins))*(bt[2]/bins)
    DT$xps_trunc[DT$xps>=bt[3]] <- bt[3]
    DT$xps_level <- ceiling(DT$xps_trunc/(bt[3]/bins))*(bt[3]/bins)
  }
  
  return(DT)
}

format_smart <- function(DT){
  col_raw <- col_smart[c(4,15)]
  col_value <- col_smart[-c(4,15)]
  for(n in col_raw){
    DT[[n]][DT[[n]]>=1e5] <- -1
  }
  for(n in col_value){
    DT[[n]][DT[[n]] >= 252] <- -1
  }
  return(DT)
}

trunc_level <- function(DT,attr,min_value=0,max_value,Tbins=20,detail=F,detail_count=1000){
  # truncate value and generate level
  attr_value <- DT[[attr]]
  attr_value[attr_value>max_value] <- max_value
  attr_value[attr_value<min_value] <- min_value
  itv <- (max_value-min_value)/Tbins
  if(Tbins>1)level <- ceiling(attr_value/itv)*(itv)
  
  if(detail & Tbins>1){
    table_attr_level <- setNames(melt(table(level)),nm=c('attr','count'))
    table_attr_level$detail_tag <- ifelse(table_attr_level$count>detail_count*2,1,0)
    
    for(i in 1:nrow(table_attr_level)){
      if(table_attr_level$detail_tag[i]==1){
        left_value <- ifelse(i==1,min_value,table_attr_level$attr[i-1])
        right_value <- table_attr_level$attr[i]
        cur_itv <- unique(ceiling((right_value-left_value)/(table_attr_level$count[i]/detail_count)))
        x <- ifelse(i==1,cur_ind <- which(level>=left_value & level<=right_value),cur_ind <- which(level>left_value & level<=right_value))
        level[cur_ind] <- ceiling(attr_value[cur_ind]/cur_itv)*cur_itv
      }else{
        next
      }
    }
  }
  return(level)
}

trunc_test <- function(DT,attr,startvalue,step,bins=20,limit_count_bin=100){
  # testing the truncate value
  min_count_bin <- limit_count_bin
  trunc_value <- startvalue
  while(min_count_bin >= limit_count_bin){
    # arr <- trunc_level(DT,attr,max_value=trunc_value,Tbins=bins)
    arr <- gen_binned_array(DT,attr,gen_balanced_binning_point(0,trunc_value,bins))
    table_arr <- table(arr)
    if(length(table_arr) < bins | min(table_arr)<limit_count_bin){
      break
    }else{
      cat(sprintf('%s trunc_value:%.4f pass!\n',attr,trunc_value))
      trunc_value <- trunc_value+step
    }
  }
  return(trunc_value)
}

gen_unbalanced_binning_point <- function(DT,attr,min_value=0,max_value,bin_count=1000){
  # truncate value and generate binning point without fixed interval but with finxed sample count in the bin
  attr_value <- DT[[attr]]
  attr_value[attr_value>max_value] <- max_value
  attr_value[attr_value<min_value] <- min_value
  itv <- (max_value-min_value)/1000
  
  sorted_value <- sort(attr_value)
  # binning_point <- unique(ceiling(sorted_value[seq(1,length(sorted_value),bin_count)]/itv)*itv)
  binning_point <- unique(sorted_value[seq(1,length(sorted_value),bin_count)])
  binning_point[1] <- min_value
  binning_point[length(binning_point)] <- max_value
  return(binning_point)
}

gen_balanced_binning_point <- function(min_value=0,max_value,Tbins=20){
  # truncate value and generate binning point with balanced binning point
  itv <- (max_value-min_value)/Tbins
  binning_point <- seq(min_value,max_value,itv)
  return(binning_point)
}

gen_binned_array <- function(DT,attr,binning_point){
  # truncate value and chopping data
  attr_value <- DT[[attr]]
  min_value <- min(binning_point)
  max_value <- max(binning_point)
  attr_value[attr_value>max_value] <- max_value
  attr_value[attr_value<min_value] <- min_value
  level <- fct2num(cut(attr_value,binning_point,binning_point[-1]))
  level[attr_value==min_value] <- min_value
  return(level)
}

binning_data <- function(DT,attr,attr_max,balanced_binning=T,bins=20,bin_count=5300){
  # binning data
  attr_level <- paste(attr,'level',sep='_')
  if(balanced_binning){
    bp <- gen_balanced_binning_point(0,attr_max,bins)
  }else{
    # attr_max <- quantile(DT[[attr]],0.99)
    bp <- gen_unbalanced_binning_point(DT,attr,0,attr_max,bin_count = bin_count)
  }
  DT[[attr_level]] <- gen_binned_array(DT,attr,binning_point = bp)
  # return(DT[,c('svrid','numD','mainModel','age',attr_level)])
  return(DT)
}

gen_corr_high_rank <- function(i,only_corr=T){
  cat(sprintf('[%s] No.%f START!!!\n',date(),i))
  col_attr <- paste('L',i,sep='')
  list[data_fr,p_fr,p_count] <- gen_result_feature(DT_quan,col_attr,100)
  corr <- cor(data_fr[,1],data_fr$AFR)
  if(only_corr){
    return(data.frame(i,corr))
  }else{
    return(list(data_fr,p_fr,p_count))
  }
}

save_fig <- function(p,title){
  eps_title <- paste(title,'.eps',sep='')
  jpg_title <- paste(title,'.jpg',sep='')
  ggsave(file=file.path(dir_data,'Paper','eps',eps_title), plot=p, width = 10, height = 6, dpi = 100)#
  ggsave(file=file.path(dir_data,'Paper','jpg',jpg_title), plot=p, width = 10, height = 6, dpi = 100)#
}

gen_theme <- function(){
  theme(axis.text = element_text(size = 28),axis.title = element_text(size = 32),
        axis.text.x = element_text(margin = margin(t=5)),axis.text.y = element_text(margin = margin(r=5)),
        axis.title.x = element_text(margin = margin(t=20)),axis.title.y = element_text(margin = margin(r=20)),
        legend.text = element_text(size = 32),legend.title = element_text(size = 36),
        legend.position = c(0.05,0.95),legend.justification = c(0,1),legend.background = element_rect(fill = alpha('grey',0.5)),
        legend.key.width = unit(2.5,units = 'line'),legend.key.height = unit(2.5,units = 'line'))
}