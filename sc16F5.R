# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source(file.path('D:/Git/attrid','attr_config.R'))

#@@@ Function @@@#
source('D:/Git/R_Function/Rfun.R')
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'AFR_io_function.R'))

#@@@ LOAD DATA @@@#
load(file.path(dir_data,'load_ftr_attrid.Rda'))
source(file.path(dir_code,'AFR_io_prepare.R'))

#####################################################################################################
#@@@ FUNCTION @@@#
# F1.plot
AFR_plot <- function(cm,title,ylimL,ylimR){ 
  cm1 <- cm
  p1 <- ggplot(cm1,aes(x = factor(itemN),y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Coefficient of Variable') + ylab('Annual Failure Rate (%)') + 
    scale_y_continuous(limits = c(ylimL,ylimR),oob = rescale_none,breaks = seq(ylimL,ylimR,0.2)) +
    # scale_x_continuous(breaks = floor(min(cm1$maxCVd)):ceiling(max(cm1$maxCVd))) +
    # scale_y_continuous(breaks = seq(0,8,1)) +
    guides(fill = guide_legend(title=NULL)) + 
    theme_bw() +
    theme(panel.background = element_rect(color = 'black'),
          panel.grid.minor = element_blank(),
          panel.grid.major = element_line(colour = 'grey', linetype = 'dotted'),
#           panel.grid.major.x = element_blank(),
          
          plot.title = element_blank(),
          axis.line = element_line(color = 'black'),
          axis.text = element_text(size = 24),
          axis.title = element_text(size = 26),
          
          legend.key.width = unit(1.5,units = 'line'),
          legend.key.height = unit(1.5,units = 'line'),
          legend.text = element_text(size = 26),
          legend.position = 'top',
          legend.background = element_rect(fill = alpha('grey',0.5))
    )
  print(p1)
  ggsave(file=file.path(dir_data,'sc16',paste(title,'.eps',sep='')), plot=p1, width = 8, height = 6, dpi = 100)
}

#F2. AFR calculate
ioAFR <- function(io,f,attr,diskCount = 1){
  t1 <- melt(table(io[,attr]))
  t2 <- melt(table(f[,attr]))
  if(length(attr) == 1){
    names(t1)[1] <- attr
    names(t2)[1] <- attr
  }
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*100
  tMerge <- subset(tMerge,!is.na(AFR))
}

#F3. replace using Nserv and Sserv
classExchg <- function(df){
  df$class[grepl('[N|n]on',df$class)] <- 'Nserv'
  df$class[!grepl('[N|n]on',df$class) & grepl('[S|s]torage',df$class)] <- 'Sserv'
  df
}
#####################################################################################################
load(file.path(dir_data,'ioDura_result.Rda'))
# divCV <- c(0,0.01*2^(seq(1:8)),Inf)
# divCV <- c(0.01*c(1,2,5),0.1*c(1,2,5),c(1,2,5),Inf)
divCV <- c(0,0.02,0.04,0.06,0.08,0.1,0.2,0.4,0.6,0.8,1.0,Inf)

ioDuraCV <- data.frame(svrid = dura999$svrid)
ioDuraCV$cvD999 <- dura999$cv[match(ioDuraCV$svrid,dura999$svrid)]
ioDuraCV$cvD9023 <- dura9023$cv[match(ioDuraCV$svrid,dura9023$svrid)]
ioDuraCV$cut9023 <- cut(ioDuraCV$cvD9023,divCV,include.lowest = T)
ioDuraCV$cut999 <- cut(ioDuraCV$cvD999,divCV,include.lowest = T)
ioDuraCV <- factorX(subset(ioDuraCV,!is.na(cut999) & !is.na(cut9023)))

f <- merge(tmp.f,ioDuraCV,by.x = 'svr_id',by.y = 'svrid')
io <- merge(tmp.cmdb,ioDuraCV,by.x = 'svr_asset_id',by.y = 'svrid')

AFR_cvD999C <- item_order(AFR_attr_notime(f,io,'cut999','cut999',1,dev = 'C'))
AFR_cvD9023C <- item_order(AFR_attr_notime(f,io,'cut9023','cut9023',1,dev = 'C'))
AFR_cvD999TS <- item_order(AFR_attr_notime(f,io,'cut999','cut999',12,dev = 'TS'))
AFR_cvD9023TS <- item_order(AFR_attr_notime(f,io,'cut9023','cut9023',12,dev = 'TS'))

AFR_cvD999 <- classExchg(rbind(AFR_cvD999C,AFR_cvD999TS))
AFR_cvD9023 <- classExchg(rbind(AFR_cvD9023C,AFR_cvD9023TS))
AFR_cvD999$itemN <- as.numeric(gsub('^\\[|^\\(|,.*$','',AFR_cvD999$item))
AFR_cvD9023$itemN <- as.numeric(gsub('^\\[|^\\(|,.*$','',AFR_cvD9023$item))

AFR_plot(AFR_cvD9023,'fig5A',1,2.2)
AFR_plot(AFR_cvD999,'fig5B',0.6,2.2)
