#Function set

# F1. AFR of single attr
sinAttrFR <- function(data,dataF,singlePartCount,title,units,disks){
  div <- as.numeric(quantile(data,seq(0,1,1/singlePartCount)))
  cut <- tableX(cut(data,div))
  cutF <- tableX(cut(dataF,div))
  cutM <- merge(cut,cutF,by = 'item')
  names(cutM) <- c('item','count','rateA','failure','rateB')
  cutM$rate <- cutM$failure/cutM$count
  cutM$rate <- cutM$rate/disks
  cutM$left <- as.numeric(gsub("\\(|,.*","",cutM$item))
  cutM <- cutM[order(cutM$left),]
  cutM$item <- factor(cutM$item,levels = as.character(cutM$item))
  cutM$left <- factor(cutM$left,levels = as.character(cutM$left))
  p1 <- ggplot(cutM,aes(x = left,y = rate*100))+geom_bar(stat = 'identity') + 
    xlab(paste('Partition (',units,')',sep='')) + ylab('Failure Rate (%)') + 
    ggtitle(title) +
    #     scale_color_discrete(name = 'Class') +
    #     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          #               axis.text.x = element_text(angle = 0,hjust = 1),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p1)
  ggsave(file=file.path(dir_data,'sta_attrid',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
  return(cutM)
}

# F2. two attr AFR
dblAttrFR <- function(mean_io,doublePartCount,title,xl,yl){
  
  #F2.1 data announcement
  partA <- data.frame(matrix(0,doublePartCount,doublePartCount))
  partF <- data.frame(matrix(0,doublePartCount,doublePartCount))
  partR <- data.frame(matrix(0,doublePartCount,doublePartCount))
  
  #F2.2 split
  cn <- names(mean_io)
  names(mean_io) <- c('svrid','colA','colB','class')
  divA <- as.numeric(quantile(mean_io$colA,seq(0,1,1/doublePartCount)))
  divA <- divA + mean(divA)*1e-20*seq(0,(length(divA)-1),1)
  divB <- as.numeric(quantile(mean_io$colB,seq(0,1,1/doublePartCount)))
  divB <- divB + mean(divB)*1e-20*seq(0,(length(divB)-1),1)
  
  #F2.3 calculate the number and ratio
  for (i in 1:(length(divA)-1)){
    for (j in 1:(length(divB)-1)){
      tmp <- mean_io[mean_io$colA > divA[i] & mean_io$colA <= divA[i+1] &
                       mean_io$colB > divB[j] & mean_io$colB <= divB[j+1],]
      partA[i,j] <- nrow(tmp)
      partF[i,j] <- nrow(subset(tmp,class == 'Failure'))
    }
  }
  partR <- partF/partA
  
  #F2.4 preprocess and melt
  #filter region not containing enough data
  partR[is.na(partR) | partA <= nrow(mean_io_C)/doublePartCount/doublePartCount/4] <- -1
  partR <- round(partR*1e5)/1e5
  names(partR) <- divB[1:doublePartCount]
  row.names(partR) <- divA[1:doublePartCount]
  meltR <- melt(as.matrix(partR))
  names(meltR) <- c('colA','colB','rate')
  meltR$colA <- factor(round(meltR$colA*1e5)/1e5)
  meltR$colB <- factor(round(meltR$colB*1e5)/1e5)
  div_tag <- as.numeric(quantile(meltR$rate[meltR$rate >= 0],seq(0,1,1/3),na.rm = T))
  div_tag <- div_tag + mean(div_tag)*1e-10*seq(0,length(div_tag)-1,1)
  meltR$tag <- cut(meltR$rate,div_tag)
  
  p1 <- ggplot(subset(meltR, rate >= 0),
               aes(x = colA, y = colB, shape = factor(tag), colour = factor(tag))) +
    geom_point(size = 15) + scale_size_continuous(range = c(3,10)) + 
    ggtitle(title) + xlab(xl) + ylab(yl) + 
    scale_color_discrete(name = 'Failure Rate') + scale_shape_discrete(name = 'Failure Rate') +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          axis.text.x = element_text(angle = 20,hjust = 1),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          #         legend.position = c(0,1),
          #         legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p1)
  ggsave(file=file.path(dir_data,'sta_attrid',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
  return(list(partA,partF,partR))
}

# F3. single attr cumulate AFR
sinAttrCumFR <- function(data,attr,title){
  tmp_io <- data
  tmp_io <- tmp_io[order(tmp_io[[attr]]),]
  tmp_io$culfail <- unlist(sapply(1:nrow(tmp_io),function(x){
    sum(tmp_io$class[1:x] == 'Failure')
  }))
  tmp_io$idx <- seq(1:nrow(tmp_io))
  write.csv(tmp_io,file = file.path(dir_data,title))
}

# F4. MCF computing, unfinished and abort
mcf <- function(data,attr){
  data <- data[order(data[[attr]]),]
  mcf <- data.frame(x = sort(unique(data[[attr]])))
  #   mcf$total
  #   mcf$fail
  mcf$pointFR <- mcf$fail/mcf$total
  #   mcf$cumu
}

# F5. single attr AFR with mannual partition
# for example, time are partitioned to [0,5,1],[5,20,5],[20,100,40]
sinAttrManualFR <- function(data,dataF,div,title,units,disks){
  cut <- tableX(cut(data,div))
  cutF <- tableX(cut(dataF,div))
  cutM <- merge(cut,cutF,by = 'item')
  names(cutM) <- c('item','count','rateA','failure','rateB')
  cutM$rate <- cutM$failure/cutM$count
  cutM$rate <- cutM$rate/disks
  cutM$left <- as.numeric(gsub("\\(|,.*","",cutM$item))
  cutM <- cutM[order(cutM$left),]
  cutM$item <- factor(cutM$item,levels = as.character(cutM$item))
  cutM$left <- factor(cutM$left,levels = as.character(cutM$left))
  p1 <- ggplot(cutM,aes(x = left,y = rate*100))+geom_bar(stat = 'identity') + 
    xlab(paste('Partition (',units,')',sep='')) + ylab('Failure Rate (%)') + 
    ggtitle(title) +
    #     scale_color_discrete(name = 'Class') +
    #     guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    theme(axis.text = element_text(size = 18),
          axis.title = element_text(size = 20),
          #               axis.text.x = element_text(angle = 0,hjust = 1),
          legend.text = element_text(size = 18),
          legend.title = element_text(size = 20),
          plot.title = element_text(size = 26, face = 'bold'),
          legend.position = c(0,1),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5)))
  print(p1)
  ggsave(file=file.path(dir_data,'sta_attrid',paste(title,'.png',sep='')), plot=p1, width = 16, height = 12, dpi = 100)
  return(cutM)
}

# F6. table for multiple columns
colTableX <- function(data,col,decreasing = T,rm.na = F){
  colMerge <- data[[col[1]]]
  for (i in seq(2,length(col))){
    colMerge <- paste(colMerge,data[[col[i]]],sep='_')
  }
  return(tableX(colMerge,decreasing = decreasing))
}

# F7. merge splited column and export a data.frame
splitToDF <- function(data,split = '_',header = ''){
  r <- data.frame(matrix(unlist(strsplit(as.character(data),split)),byrow = T,nrow = length(data)))
  if (header[1] != ''){
    names(r) <- header
  }
  return(r)
}

# F8. comupute rate of increment
incCalc <- function(data){
  len <- length(data)
  inc <- (data[2:len] - data[1:(len-1)])/data[1:(len-1)]
  inc <- c(0,inc)
  return(inc)
}

# F9. multiplot
multiplot <- function(..., plotlist = NULL, file, cols = 1, layout = NULL) {
  require(grid)
  
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots == 1) {
    print(plots[[1]])
    
  } else {
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    for (i in 1:numPlots) {
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# F10.return multiple value and recive by list
# from https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
  x
}

## F11.for log2 computation, set [0,1] to 1 and [-1,0] to -1
mdf4log2 <- function(x){
  x[x < 1 & x >= 0] <- 1
  x[x > -1 & x < 0] <- -1
  x
}
log4neg <- function(x){
  x <- mdf4log2(x)
  x[x > 0] <- log2(x[x > 0])
  x[x < 0] <- log2(abs(x[x < 0])) * -1
  x
}

# F12.order for column of df in order to plot conveniently
item_order <- function(df,attr = 'item'){
  od <- as.numeric(gsub('^\\[|^\\(|,.*$','',df[[attr]]))
  df[attr] <- factor(df[[attr]],levels = df[[attr]][order(od)])
  df <- df[order(od),] 
  row.names(df) <- NULL
  df
}
