
## F1.for log2 computation, set [0,1] to 1 and [-1,0] to -1
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

# F2.order for column of df in order to plot conveniently
item_order <- function(df,attr = 'item'){
  od <- as.numeric(gsub('^\\[|^\\(|,.*$','',df[[attr]]))
  df[attr] <- factor(df[[attr]],levels = df[[attr]][order(od)])
  df <- df[order(od),] 
  row.names(df) <- NULL
  df
}

#F3. AFR calculate
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
  tMerge <- subset(tMerge,!is.na(AFR))
}

#F4. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}

# F5.plot
AFR_plot <- function(DT,title,ylimL,ylimR,para_x,para_y,para_fill,para_xlab,para_ylab){ 
  p1 <- ggplot(DT,aes_string(x = para_x,y = para_y,fill = para_fill)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab(para_xlab) + ylab(para_ylab) + 
    scale_y_continuous(limits = c(ylimL,ylimR),oob = rescale_none,breaks = seq(ylimL,ylimR,(ylimR-ylimL)/5)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
          
          plot.title = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(0.05,0.95),legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), plot=p1, width = 8, height = 6, dpi = 100)
}

# F6. AFR of non-time class
AFR_attr_notime <- function(f,io,attr,diskCount,dev = ""){
  if(dev != ""){
    f <- subset(f,grepl(dev,dClass))
    io <- subset(io,grepl(dev,dClass))
  }
  tio <- setNames(melt(table(io[[attr]])),c('item','count'))
  tf <- setNames(melt(table(f[[attr]])),c('item','count'))
  tiof <- merge(tio,tf,by = 'item',all = T)
  names(tiof) <- c('item','count_io','count_f')
  
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
    tiof$class <- attr
  }
  tiof <- tiof[,c('item','class','AFR','count_f','count_io')]
  tiof
}