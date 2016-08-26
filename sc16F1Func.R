#@@@ FUNCTION @@@#
# F1.plot
AFR_plot <- function(cm,title){ 
  p1 <- ggplot(cm,aes(x = as.character(item),y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Disk Age (years)') + ylab('Annual Failure Rate (%)') + 
    # scale_x_continuous(breaks = floor(min(cm$item)):ceiling(max(cm$item))) +
    scale_y_continuous(breaks = seq(floor(min(cm$AFR)),10,1)) +
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
          axis.text.x = element_text(angle = 40,hjust = 1),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = c(0.05,0.95),
          legend.justification = c(0,1),
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), plot=p1, width = 10, height = 6, dpi = 100)
  p1
}

#F2. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}

# F3. cut fail time and shelf time into by 3 months
cut3mon <- function(t){
  t1 <- floor(t*4)
  t2 <- t1/4
  # t2 <- t2 + 0.25
  t2[t2 > 4] <- floor(t2[t2 > 4])
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
