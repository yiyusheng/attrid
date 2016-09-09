#@@@ FUNCTION @@@#
# F1.plot
AFR_plot <- function(cm,title){ 
  # fit a bathtub curve based on cm
  start <- 2.75
  smp <- subset(cm,item >=start & item <= 3.25 & class == 'Nserv',c('item','AFR'))
  x <- smp$item;y <- smp$AFR
  fit <- lm(y~poly(x,2,raw = T))
  x1 <- seq(start,3.25,0.05)
  y1 <- predict(fit,data.frame(x = x1))
  x2 <- x1 - start
  y2 <- rev(y1)
  fitCurve <- data.frame(item = c(x2,x1),AFR = c(y2,y1),class = 'line')
  
  p1 <- ggplot(cm,aes(x = item,y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    # xlab('Disk Age (years)') + ylab('Failure Rate (%)') + 
    # # scale_x_continuous(breaks = floor(min(cm$item)):ceiling(max(cm$item))) +
    # scale_y_continuous(breaks = seq(0,10,1)) +
    # guides(fill = guide_legend(title=NULL)) + 
    # theme_bw() +
    # theme(panel.background = element_rect(color = 'black'),
    #       panel.grid.minor = element_line(size = 0.4),
    #       panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
    #       #           panel.grid.major.x = element_blank(),
    #       
    #       plot.title = element_blank(),
    #       axis.line = element_line(color = 'black'),
    #       axis.text = element_text(size = 26),
    #       axis.title = element_text(size = 26),
    #       axis.text.x = element_text(angle = 40,hjust = 1),
    #       
    #       legend.key.width = unit(1.5,units = 'line'),
    #       legend.key.height = unit(1.5,units = 'line'),
    #       legend.text = element_text(size = 26),
    #       legend.position = c(0.05,0.95),
    #       legend.justification = c(0,1),
    #       legend.background = element_rect(fill = alpha('grey',0.5))
    # )
  p2 <- p1 + geom_line(data = fitCurve,aes(x = item,y = AFR,group = 1,linetype = 'Bathtub Curve')) 
  
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), plot=p1, width = 10, height = 6, dpi = 100)
  p2
}

#F1.5 AFR for warranty effect

AFR_plot_warranty <- function(cm,title){ 
  cm <- subset(cm,item >= 2)
  p <- ggplot(subset(cm),aes(x = as.character(item),y = AFRdiff,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Disk Age (years)') + ylab('Failure Rate (%)') + 
    # scale_x_continuous(breaks = floor(min(cm$item)):ceiling(max(cm$item))) +
    scale_y_continuous(breaks = seq(0,8,1)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_line(size = 0.4),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted', size = 1),
          #           panel.grid.major.x = element_blank(),
          
          plot.title = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 26),
          axis.title = element_text(size = 26),
          # axis.text.x = element_text(angle = 40,hjust = 1),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(0.05,0.95),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), 
         plot=p, width = 10, height = 6, dpi = 100)
  p
}

#F2. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}

# F3. cut fail time and shelf time into by 3 months
cut3mon <- function(t,cutValue){
  t1 <- floor(t*4)
  t2 <- t1/4
  # t2 <- t2 + 0.25
  t2[t2 > cutValue] <- floor(t2[t2 > cutValue])
  t2
}

# F4. AFR for attr without time
AFR_attr_notime <- function(f,io,attr1,attr2,diskCount,dev = ""){
  if(dev != ""){
    f <- subset(f,grepl(dev,dClass))
    io <- subset(io,grepl(dev,dClass))
  }
  eval(parse(text = sprintf('tio <- tableX(io$%s)',attr2)))
  eval(parse(text = sprintf('tf <- tableX(f$%s)',attr1)))
  tiof <- merge(tio,tf,by = 'item',all = T)
  names(tiof) <- c('item','count_io','rate_io','count_f','rate_f')
  tiof$AFR <- tiof$count_f/tiof$count_io/diskCount*100
  if(dev == 'C'){
    tiof$class <- 'Non-Storage Servers'
  }else if(dev == 'TS'){
    tiof$class <- 'Storage Servers'
  }else if(dev == 'TS1T'){
    tiof$class <- 'Storage Servers[1T]'
  }else if(dev == 'TS2T'){
    tiof$class <- 'Storage Servers[2T]'
  }else{
    tiof$class <- attr2
  }
  tiof <- tiof[,c('item','class','AFR','count_f','count_io','rate_f','rate_io')]
  
  item_num <- as.numeric(fct2ori(tiof$item))
  if(all(!is.na(item_num))){
    tiof$item <- item_num
    tiof <- tiof[order(tiof$item),]
    row.names(tiof) <- NULL
  }
  tiof
}

# F5. virtualize server to multiple disks to compute AFR accurately
# df <- tmp.f;dc <- tmp.cmdb
virt_disk <- function(df,dc){
  # virtualize disks
  colC <- c('svr_asset_id','dev_class_id','ip','raid','use_time','dClass',
            'shiptimeToLeft','shiptimeToRight','shTimeQu')
  idx.rep <- rep(seq_len(nrow(dc)),dc$diskNum)
  diskidx <- unlist(tapply(idx.rep,idx.rep,function(x)seq_len(length(x))))
  
  virtDC <- dc[idx.rep, colC]
  virtDC$diskidx <- diskidx
  virtDC$status <- 'working'
  virtDC$f_time <- as.POSIXct('2014-01-01',tz = 'UTC')
  virtDC <- virtDC[order(virtDC$svr_asset_id),]
  virtDC$id <- seq_len(nrow(virtDC))
  virtDCF <- factorX(subset(virtDC,svr_asset_id %in% df$svr_id))
  virtDCN <- factorX(subset(virtDC,!(svr_asset_id %in% df$svr_id)))
  
  df <- df[order(df$svr_id),]
  df$id <- seq_len(nrow(df))
  
  # For failure
  mer <- merge(df[,c('svr_id','id')],virtDC[,c('svr_asset_id','id')],
             by.x = 'svr_id',by.y = 'svr_asset_id',all.x = T)
  mer <- mer[order(mer$id.x,mer$id.y),]
  idx.dc <- split(mer$id.y,f = mer$svr_id)
  
  # index establish
  splitF <- split(seq_len(nrow(df)),df$svr_id)
  splitC <- split(seq_len(nrow(virtDCF)),virtDCF$svr_asset_id)
  svridF <- levels(df$svr_id)
  
  tmpCol <- list()
  newvirtDCF <- lapply(seq_len(length(svridF)),function(i){
    print(i)
    f <- df[splitF[[i]],]
    c <- virtDCF[splitC[[i]],]
    len.f <- nrow(f)
    len.c <- nrow(c)
    max.c <- max(c$diskidx)
    
    # Add New
    tmp <- c[rep(len.c,len.f),]
    tmp$diskidx <- tmp$diskidx + 1:len.f
    tmp$use_time <- f$f_time
    c <- rbind(c,tmp)
    
    # Add failure
    c$status[1:len.f] <- 'failed'
    c$f_time[1:len.f] <- f$f_time
    
    c
  })
  newvirtDCF <- do.call(rbind,newvirtDCF)
  
  virtDC <- rbind(newvirtDCF,virtDCN)
  virtDC
}