# Function for AFR_io
#@@@ LOCAL Function @@@#
AFR <- function(f,cm,lastYears,diskCount,dev = ''){
  if(dev != ''){
    f <- subset(f,grepl(dev,dClass))
    cm <- subset(cm,grepl(dev,dClass))
  }
  
  divAll <- c(0,(seq(1,(lastYears-1)) - 1/12),lastYears)
  divF <- seq(0,lastYears)
  cutF <- tableX(cut(f$failShiptime,divF))
  cutLeft <- tableX(cut(cm$shiptimeToLeft,divAll))
  cutF$idx <- as.numeric(gsub("\\(|,.*","",cutF$item))
  cutLeft$idx <- as.numeric(gsub("\\(|,.*","",cutLeft$item))
  
  cutLeft <- cutLeft[order(cutLeft$idx),]
  cutLeft$idx <- seq(0,(lastYears-1))
  cutMerge <- merge(cutF,cutLeft,by = 'idx')
  cutMerge$rate.x <- NULL
  cutMerge$rate.y <- NULL
  cutMerge$AFR <- cutMerge$count.x/cutMerge$count.y/diskCount
  return(cutMerge)
}

AFR_plot <- function(cutMerge,title,yl){ 
  if (yl == -1){
    p1 <- ggplot(cutMerge,aes(x = item,y = AFR*100*6)) + geom_bar(stat = 'identity') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + 
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 26, face = 'bold'),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  } else if (yl == -2){
    p1 <- ggplot(cutMerge,aes(x = item,y = AFR*100*6,fill = class)) + 
      geom_bar(stat = 'identity',position = 'dodge') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + guides(fill = guide_legend(title=NULL)) + 
      #       scale_alpha(guide = F) +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 30),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 26, face = 'bold'),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  } else {
    p1 <- ggplot(cutMerge,aes(x = item,y = AFR*100*6)) + geom_bar(stat = 'identity') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + 
      ylim(c(0,yl)) +
      theme(axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 18),
            legend.title = element_text(size = 20),
            plot.title = element_text(size = 26, face = 'bold'),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  }
  print(p1)
  ggsave(file=file.path(dir_data,'ship_time',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
}

AFR_value <- function(p3.f,p3.cmdb,p3.io,attr,levelCount,lastYears,diskCount){
  # 求区间
  div902 <- quantile(p3.io$mean_902/diskCount,seq(0,1,1/levelCount))
  div903 <- quantile(p3.io$mean_903/diskCount,seq(0,1,1/levelCount))
  div999 <- quantile(p3.io$mean_999,seq(0,1,1/levelCount))
  divAll <- c(0,(seq(1,(lastYears-1)) - 1/12),lastYears)
  divF <- seq(0,lastYears)
  # 给每台机器添加区间
  p3.io$lv902 <- cut(p3.io$mean_902/diskCount,div902)
  p3.io$lv903 <- cut(p3.io$mean_903/diskCount,div903)
  p3.io$lv999 <- cut(p3.io$mean_999,div999)
  
  mergecol <- c('svrid','lv902','lv903','lv999')
  p3.cmdb <- merge(p3.cmdb,p3.io[,mergecol],by.x = 'svr_asset_id',by.y = 'svrid')
  p3.f <- subset(p3.f,svr_id %in% cmdbio$svr_asset_id)
  p3.f <- merge(p3.f,p3.io[,mergecol],by.x = 'svr_id',by.y = 'svrid')
  
  p3.cmdb$lvUsetime <- as.character(cut(p3.cmdb$shiptimeToLeft,divAll))
  p3.f$lvUsetime <- as.character(cut(p3.f$failShiptime,divF))
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(0,0.917]'] <- '(0,1]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(0.917,1.92]'] <- '(1,2]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(1.92,2.92]'] <- '(2,3]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(2.92,3.92]'] <- '(3,4]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(3.92,4.92]'] <- '(4,5]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(4.92,5.92]'] <- '(5,6]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(5.92,7]'] <- '(6,7]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(4.92,6]'] <- '(5,6]'
  p3.cmdb$lvUsetime[p3.cmdb$lvUsetime == '(3.92,5]'] <- '(4,5]'
  
  col.table <- c(paste('lv',attr,sep=''),'lvUsetime')
  cutP3f <- colTableX(p3.f,col.table)
  cutP3cmdb <- colTableX(p3.cmdb,col.table)
  cutMerge <- merge(cutP3f,cutP3cmdb,by = 'item',all = T)
  cutMerge <- cbind(cutMerge,splitToDF(cutMerge$item,header = c('value','shipTime')))
  cutMerge <- subset(cutMerge,shipTime != 'NA' & value != 'NA',c('value','shipTime','count.x','count.y'))
  cutMerge <- factorX(cutMerge)
  cutMerge$value <- factor(cutMerge$value,
                           levels = levels(cutMerge$value)[
                             order(as.numeric(gsub("\\(|,.*","",levels(cutMerge$value))))])
  
  cutMerge$AFR <- cutMerge$count.x/cutMerge$count.y/diskCount
  #   cutMerge <- cutMerge[order(cutMerge$shipTime),]
  return(cutMerge)
}



#对任何一个字段，不同时间段的故障率
AFR_attr <- function(f,cmdb,attr,lastYears,diskCount,dev = '',defValue = ' 0'){
  # 求区间
  f <- factorX(f)
  cmdb <- factorX(cmdb)
  if (dev != ''){
    f <- subset(f,dClass == dev)
    cmdb <- subset(cmdb,dClass == dev)
  }
  divAll <- c(0,(seq(1,(lastYears-1)) - 1/12),lastYears)
  divF <- seq(0,lastYears)
  cmdb$lvUsetime <- as.character(cut(cmdb$shiptimeToLeft,divAll))
  f$lvUsetime <- as.character(cut(f$failShiptime,divF))
  cmdb$lvUsetime[cmdb$lvUsetime == '(0,0.917]'] <- '(0,1]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(0.917,1.92]'] <- '(1,2]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(1.92,2.92]'] <- '(2,3]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(2.92,3.92]'] <- '(3,4]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(3.92,4.92]'] <- '(4,5]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(4.92,5.92]'] <- '(5,6]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(5.92,7]'] <- '(6,7]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(4.92,6]'] <- '(5,6]'
  cmdb$lvUsetime[cmdb$lvUsetime == '(3.92,5]'] <- '(4,5]'
  
  col.table <- c(attr,'lvUsetime')
  cutP3f <- colTableX(f,col.table)
  cutP3cmdb <- colTableX(cmdb,col.table)
  cutMerge <- merge(cutP3f,cutP3cmdb,by = 'item',all = T)
  cutMerge <- cbind(cutMerge,splitToDF(cutMerge$item,header = c('value','shipTime')))
  cutMerge <- subset(cutMerge,shipTime != 'NA' & value != 'NA',c('value','shipTime','count.x','count.y'))
  cutMerge <- factorX(cutMerge)
  cutMerge$value <- factor(cutMerge$value,
                           levels = levels(cutMerge$value)[
                             order(as.numeric(gsub("\\(|,.*","",levels(cutMerge$value))))])
  cutMerge <- subset(cutMerge,value != '')
  levels(cutMerge$value)[levels(cutMerge$value) == '-1'] <- defValue
  cutMerge$AFR <- cutMerge$count.x/cutMerge$count.y/diskCount
  cutMerge <- cutMerge[order(cutMerge$shipTim,cutMerge$value),]
  return(cutMerge)
}

#分字段处理，不处理时间
AFR_attr_notime <- function(f,io,attr1,attr2,diskCount,dev = ""){
  if(dev != ""){
    f <- subset(f,grepl(dev,dClass))
    io <- subset(io,grepl(dev,dClass))
  }
  eval(parse(text = sprintf('tio <- tableX(io$%s)',attr2)))
  eval(parse(text = sprintf('tf <- tableX(f$%s)',attr1)))
  tiof <- merge(tio,tf,by = 'item',all = T)
  names(tiof) <- c('item','count_io','rate_io','count_f','rate_f')
  tiof$AFR <- tiof$count_f/tiof$count_io/diskCount*6
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
}

AFR_value_plot <- function(cutMerge,title,yl,
                           subdir = '',valueFilter = '',cylim = -1){
  plotCol <- c('value','shipTime','AFR')
  if (valueFilter[1] != ''){
    cutMerge <- subset(cutMerge,!(value %in% valueFilter))
  }
  if (cylim != -1){
    cutMerge <- subset(cutMerge,count.y >= cylim)
  }
  cutMerge <- factorX(cutMerge)
  naFill <- cbind(expand.grid(value = levels(cutMerge$value),shipTime = cutMerge$shipTime),AFR = NA)
  cutMerge <- rbind(subset(cutMerge,,plotCol),naFill)
  if (yl == -1){
    p1 <- ggplot(cutMerge,aes(x = shipTime,y = AFR*100*6,fill = factor(value))) + 
      geom_bar(stat = 'identity',position = 'dodge') + 
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + guides(fill = guide_legend(title=NULL)) + 
      theme(plot.title = element_text(size = 26, face = 'bold'),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 20),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  } else {
    p1 <- ggplot(cutMerge,aes(x = shipTime,y = AFR*100*6,fill = factor(value))) + 
      geom_bar(stat = 'identity',position = 'dodge') +
      xlab('Ship Time (years)') + ylab('Annual Failure Rate (%)') + 
      ggtitle(title) + guides(fill = guide_legend(title=NULL)) + 
      ylim(c(0,yl)) +
      theme(plot.title = element_text(size = 26, face = 'bold'),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 20),
            legend.text = element_text(size = 25),
            legend.title = element_text(size = 20),
            legend.position = c(0,1),
            legend.justification = c(0,1),
            legend.background = element_rect(fill = alpha('grey',0.5)))
  }
  print(p1)
  ggsave(file=file.path(dir_data,'ship_time',subdir,paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
}