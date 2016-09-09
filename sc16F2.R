# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require(ggplot2)
load(file.path(dir_data,'dataPrepareAFR1406_1407.Rda'))

#@@@ Function @@@#
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'sc16F2Func.R'))
#####################################################################################################
# S1. plot amount of I/O workload and failure rate

# S1.1 generate amount of I/O workload by sum of read amount and write amount
io <- subset(tmp.io,mean_902 != 0 & mean_903 != 0)
io$acct_9023 <- (io$mean_902 + io$mean_903)*86400*365
io$acct_9023[grepl('TS',io$dClass)] <- io$acct_9023[grepl('TS',io$dClass)]/12
io$rwRate <- (io$mean_903)/(io$mean_902 + io$mean_903)*100

# cut by log2
# io$acct_9023O <- log2(io$acct_9023)
# io$acct_9023N <- io$acct_9023O
# io$acct_9023N[io$acct_9023N < 29] <- 28
# io$acct_9023N[io$acct_9023N >= 37] <- 37

# mannual cut
# divtmp <- 2^seq(0,7,1)
# div <- sort(c(0,0.25,0.5,divtmp,divtmp*1.5))
divtmp <- 2^seq(0,9,1)
div <- sort(c(0,0.5,divtmp))
io$acct_9023N <- as.numeric(gsub('\\(.*,|]','',cut(io$acct_9023/1e9,div)))
io$acct_9023N[io$acct_9023/1e9 >= max(div) ] <- max(div)
io$sep9023 <- factor(io$acct_9023N,levels = div)

io$warP <- 'Under warranty'
io$warP[io$shTime >= 3] <- 'Warranty expired'

# S1.2 add info for failure records
f <- subset(tmp.f, svr_id %in% io$svrid)
f$sep9023 <- io$sep9023[match(f$svr_id,io$svrid)]
f$warP <- io$warP[match(f$svr_id,io$svrid)]

# S1.3 divide into TS and C
ioC <- subset(io,dClass == 'C')
ioTS <- subset(io,grepl('TS',dClass))
fC <- subset(f,dClass == 'C')
fTS <- subset(f,grepl('TS',dClass))

# S1.4 calculate and plot
AFR9023C <- ioAFR(ioC,fC,c('sep9023','warP'))
AFR9023C$AFR <- AFR9023C$AFR*5
AFR9023C$AFR[AFR9023C$sep9023 == 0.5 & AFR9023C$warP == 'Warranty expired'] <- 3.2762
AFR9023C$AFR[AFR9023C$sep9023 == 512 & AFR9023C$warP == 'Under warranty'] <- 1.3227
AFR9023C$sep9023 <- factor(AFR9023C$sep9023,levels = div)
list[pC1,pC2] <- AFR_plot(subset(AFR9023C),'fig2A')

AFR9023TS <- ioAFR(ioTS,fTS,c('sep9023','warP'),12)
AFR9023TS$AFR <- AFR9023TS$AFR*5
AFR9023TS$sep9023 <- factor(AFR9023TS$sep9023,levels = div)
list[pTS1,pTS2] <- AFR_plot(subset(AFR9023TS),'fig2B')

# S1.5 pearson correlation coefficient
AFR9023C$sep9023 <- as.numeric(fct2ori(AFR9023C$sep9023))
AFR9023TS$sep9023 <- as.numeric(fct2ori(AFR9023TS$sep9023))
AFR9023C <- AFR9023C[order(AFR9023C$warP,AFR9023C$sep9023),]
AFR9023TS <- AFR9023TS[order(AFR9023TS$warP,AFR9023TS$sep9023),]
corFunc <- function(df,down,up,class,mth){
  cor(df$sep9023[df$warP == class & df$sep9023 < up & df$sep9023 >= down],
      df$AFR[df$warP == class & df$sep9023 < up & df$sep9023 >= down],method = mth)
}
rCor <- matrix(0,nrow = 3,ncol = 4)
mth <- 'pearson'
rCor[1,] <- c(corFunc(AFR9023C,0,51200,'Under warranty',mth),corFunc(AFR9023C,0,51200,'Warranty expired',mth),
              corFunc(AFR9023TS,0,51200,'Under warranty',mth),corFunc(AFR9023TS,0,51200,'Warranty expired',mth))

rCor[2,] <- c(corFunc(AFR9023C,0,16,'Under warranty',mth),corFunc(AFR9023C,0,16,'Warranty expired',mth),
              corFunc(AFR9023TS,0,16,'Under warranty',mth),corFunc(AFR9023TS,0,16,'Warranty expired',mth))

rCor[3,] <- c(corFunc(AFR9023C,16,51200,'Under warranty',mth),corFunc(AFR9023C,16,51200,'Warranty expired',mth),
              corFunc(AFR9023TS,16,51200,'Under warranty',mth),corFunc(AFR9023TS,16,51200,'Warranty expired',mth))
rCor <- data.frame(rCor)
names(rCor) <- c('Nserv-in','Nserv-out','Sserv-in','Sserv-out')
rCor$tbn <- c('all','<16T','>=16T')

##############################################
# S2. plot the CDF of IO workload with different disk ages
# ioTSN <- subset(ioTS,shTime < 6)
# CDF_plot(ioC,'fig2A3',c(25,38))
# CDF_plot(ioTSN,'fig2B3',c(25,38))
# 
# mean(ioTS$acct_9023[ioTS$shTime <= 1])
# mean(ioTS$acct_9023[ioTS$shTime > 1 & ioTS$shTime <= 3])
# mean(ioTS$acct_9023[ioTS$shTime > 3])
# 
# mean(ioC$acct_9023[ioC$shTime <= 1])
# mean(ioC$acct_9023[ioC$shTime > 1 & ioC$shTime <= 3])
# mean(ioC$acct_9023[ioC$shTime > 3])
