missingcol_fill <- function(df,colname,default = 0){
  df[is.na(df)] <- default
  missing_col <- setdiff(colname,names(df))
  if(length(missing_col) == 0)return(df)
  
  tmp <- data.frame(matrix(default,length(missing_col)*nrow(df),nrow = nrow(df)))
  names(tmp) <- missing_col
  df1 <- cbind(df,tmp)
  return(df1[,as.character(colname)])
}

sta_level_eachsvrid <- function(dd,attrv_level,attrd_level,corr_colname){
  tmp <- melt_table(dd$svrid,dd[[attrv_level]],dd[[attrd_level]])
  tmp <- melt_table(tmp$Var2,tmp$Var3)
  tmp <- dcast(tmp,Var1~Var2,value.var = 'value')
  tmp$Var1 <- NULL
  missingcol_fill(tmp,as.character(corr_colname))
}

sta_level_attr_pair <- function(attrv,attrd){
  # cat(sprintf('%s\t%s\n',attrv,attrd))
  attrv_level <- paste(attrv,'_level',sep='')
  attrd_level <- paste(attrd,'_level',sep='')
  corr_colname <- cutList[[paste('cut_',attrd,sep = '')]]
  corr_colname <- corr_colname[-length(corr_colname)]
  
  tmp_count <- missingcol_fill(as.data.frame.matrix(table(dd[[attrv_level]],dd[[attrd_level]])),as.character(corr_colname))
  names(tmp_count) <- col_level
  df1 <- data.frame(a1 = attrv,a2 = attrd,a1_value = as.numeric(row.names(tmp_count)),tmp_count)
  
  tmp_svrid <- sta_level_eachsvrid(dd,attrv_level,attrd_level,corr_colname)
  names(tmp_svrid) <- col_level
  df2 <- data.frame(a1 = attrv,a2 = attrd,a1_value = as.numeric(row.names(tmp_count)),tmp_svrid)
  
  list(df1,df2)
}

sta_level <- function(i,cutList){
  fn <- fname[i]
  cat(sprintf('[%s]\tfile:%s\tSATRT!!!\n',date(),fn))
  load(file.path(dir_datatendcastClear,fn))
  dd <- remove_line_byvalue(dt_dcast[,c('svrid','time',attrName)])
  dd <- factorX(subset(dd,!(svrid %in% invalid_iopsw$svrid)))
  # dd <- factorX(subset(dd,svrid %in% levels(dd$svrid)[1:15]))
  
  dd$util_level <- cut_level(dd$util,cutList$cut_util)
  dd$rps_level <- cut_level(dd$rps,cutList$cut_rps)
  dd$iopsr_level <- cut_level(dd$iopsr,cutList$cut_iopsr)
  dd$wps_level <- cut_level(dd$wps,cutList$cut_wps)
  dd$iopsw_level <- cut_level(dd$iopsw,cutList$cut_iopsw)
  
  # dd$util_level <- fct2num(cut(dd$util,cutList$cut_util,cutList$cut_util[-length(cutList$cut_util)],right = F))
  
  attr_pair <- data.frame(attr_value = rep(attrName,each = length(attrName)),attr_dist = rep(attrName,length(attrName)))
  attr_pair <- subset(attr_pair, attr_value != attr_dist)
  
  r <- mapply(sta_level_attr_pair,attr_pair$attr_value,attr_pair$attr_dist,SIMPLIFY = F)
  r1 <- do.call(rbind,lapply(r,'[[',1));r1$fn <- fn;row.names(r1) <- NULL
  r2 <- do.call(rbind,lapply(r,'[[',2));r2$fn <- fn;row.names(r2) <- NULL
  
  cat(sprintf('[%s]\tfile:%s\tEND!!!\n',date(),fn))
  list(r1,r2)
}

plot_sla <- function(){
  dir_plot <- 'plot_sta_level'
  x <- lapply(attrName,function(an){
    difAn <- setdiff(attrName,an)
    sta_level_aggre <- r_item$aggre
    p <- lapply(difAn,function(any){
      sla <- subset(sta_level_aggre,a1 == an & a2 == any)
      names(sla)[-c(1,2,3,ncol(sla))] <- paste('X',cutList[[paste('cut',any,sep='_')]][-(length(cutList$cut_util))],sep='')
      sla_melt <- melt(sla[-c(1,2,ncol(sla))],id.vars = 'a1_value')
      sla_melt$a1_value <- factor(sla_melt$a1_value)
      ggplot(sla_melt,aes(x = a1_value,y = value, fill = variable)) + geom_bar(stat = 'identity') + 
        xlab(an) + ylab(any) + scale_fill_brewer(palette="Spectral")
    })
    
    plot_path <- file.path(dir_data,dir_plot)
    if(!dir.exists(plot_path))dir.create(plot_path)
    png(filename = file.path(plot_path,paste(an,'.jpg',sep='')),width = 800, height = 600)
    multiplot(p[[1]],p[[2]],p[[3]],p[[4]],cols = 2)                                                                          
    dev.off()
    return(p)
  })
}

get_sta <- function(l){
  sta_level <- do.call(rbind,l)
  sta_level_aggre <- aggregate(sta_level[,col_level],by = list(sta_level$a1,sta_level$a2,sta_level$a1_value),sum)
  sta_level_aggre$count <- apply(sta_level_aggre[,col_level],1,sum)
  names(sta_level_aggre)[1:3] <- c('a1','a2','a1_value')
  sta_level_rate <- sta_level_aggre
  sta_level_rate[,col_level] <- roundX(sta_level_rate[,col_level]/sta_level_rate$count)
  list(ori = sta_level,aggre = sta_level_aggre,rate = sta_level_rate)
}