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
load(file.path(dir_data,'ioFluc9023Simp.Rda'))

#####################################################################################################
#@@@ FUNCTION @@@#
# F1.plot
AFR_plot <- function(cm,title){ 
  cm1 <- cm
  cm1$maxCVd <- factor(cm1$maxCVd)
  p1 <- ggplot(cm1,aes(x = factor(maxCVd),y = AFR,fill = class)) + 
    geom_bar(stat = 'identity',position = 'dodge') +
    xlab('Coefficient of Variable') + ylab('Annual Failure Rate (%)') + 
    # scale_x_continuous(breaks = floor(min(cm1$maxCVd)):ceiling(max(cm1$maxCVd))) +
    scale_y_continuous(breaks = seq(0,8,1)) +
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
#####################################################################################################
# For cmdbio
cmdbio$dClassN <- 'Sserv'
cmdbio$dClassN[grepl('C',cmdbio$dClass)] <- 'Nserv'
cmdbio$warP <- 'Under warranty'
cmdbio$warP[cmdbio$shTime >= 3] <- 'Warranty expired'
f <- tmp.f
f$dClassN <- cmdbio$dClassN[match(f$svr_id,cmdbio$svr_asset_id)]
f$warP <- cmdbio$warP[match(f$svr_id,cmdbio$svr_asset_id)]

# For fluc
ioFluc <- ioFluc9023
ioFluc$cv9023[is.na(ioFluc$cv9023)] <- 0
ioFluc$cv9023 <- abs(ioFluc$cv902)
ioFluc <- subset(ioFluc,svrid %in% cmdbio$svr_asset_id)
ioFluc <-factorX(ioFluc)

meCV <- tapply(ioFluc$cv9023,ioFluc$svrid,mean)
maxCV <- tapply(ioFluc$cv9023,ioFluc$svrid,max)
topmaxCV <- tapply(ioFluc$cv9023,ioFluc$svrid,function(x){
  x <- sort(x,decreasing = T)
  mean(x[1:10])
})

disp <- data.frame(svrid = names(meCV),
                   meanCV = as.numeric(meCV),
                   maxCV = as.numeric(maxCV),
                   topmaxCV = as.numeric(topmaxCV))

# cut maxCV
# disp$maxCV <- round(disp$maxCV)
divCV <- c(seq(0,5,0.5),max(disp$maxCV,na.rm = T))
disp$maxCVd <- cut(disp$maxCV,divCV,include.lowest = T)
disp$maxCVd <- gsub('^\\[|^\\(|,.*$','',disp$maxCVd)

# Merge
io <- merge(disp,cmdbio[,c('svr_asset_id','dClassN','warP')],
            by.x = 'svrid',by.y = 'svr_asset_id')
f<- merge(disp,f[,c('svr_id','dClassN','warP')],
          by.x = 'svrid',by.y = 'svr_id')
io$classA <- paste(io$dClassN,'(',io$warP,')',sep='')

ioC <- subset(io,dClassN == 'Nserv')
ioTS <- subset(io,dClassN == 'Sserv')
fC <- subset(f,dClassN == 'Nserv')
fTS <- subset(f,dClassN == 'Sserv')

#plot CDF
# p1 <- ggplot(io,aes(x = maxCV,color = classA)) + stat_ecdf(size = 1) + xlim(c(0,17))
# print(p1)
# ggsave(file=file.path(dir_data,'sc16','fig4A.eps'), plot=p1, width = 8, height = 6, dpi = 100)

#plot AFR
AFRflucTS <- ioAFR(ioTS,fTS,c('maxCVd'),12)
AFRflucC <- ioAFR(ioC,fC,c('maxCVd'))
AFRflucC$class <- 'Nserv'
AFRflucTS$class <- 'Sserv'

# pC <- AFR_plot(AFRflucC,'fig4B')
# pTS <- AFR_plot(AFRflucTS,'fig4C')
p <- AFR_plot(rbind(AFRflucC,AFRflucTS),'fig4D')

