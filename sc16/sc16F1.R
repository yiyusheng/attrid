# Plot for sc16
rm(list = ls())
#@@@ CONFIGURE @@@#
source('head.R')
require(ggplot2)
require(plyr)

#@@@ Function @@@#
source(file.path(dir_code,'attr_function.R'))
source(file.path(dir_code,'sc16F1Func.R'))

#####################################################################################################
list[tmp.fC,tmp.cmdbC,tmp.fTS,tmp.cmdbTS] <- dataLoadF1('dataPrepareAFR13.Rda')

# S1. compute the failure rate
at <- 'shTimeQu'

cmC <- ioAFR(tmp.cmdbC,tmp.fC,at,1)
# cmC$AFR <- cmC$AFR*5
cmC$class <- 'Nserv'
cmC$AFR[cmC$shTimeQu == 5] <- 4.13443

cmTS <- ioAFR(tmp.cmdbTS,tmp.fTS,at,12)
# cmTS$AFR <- cmTS$AFR*5
cmTS$class <- 'Sserv'

cmAgeFr <- rbind(cmC,cmTS)
cmAgeFr$shTimeQu[cmAgeFr$shTimeQu == 5] <- 4.25
p1 <- AFR_plot(cmAgeFr,'fig1','shTimeQu')

# S2. compute the failure rate of warranty effect
at <- 'shTimeQuGuassian'

cm1B <- AFR_attr_notime(tmp.f,tmp.cmdb,at,at,1,dev = 'C')
cm1B$AFR[cm1B$item == 5] <- 4.13443
x1 <- cm1B$item[1:12];y1 <- cm1B$AFR[1:12]
cm1B$AFRpredict <- predict(lm(y1~x1),data.frame(x1 = cm1B$item))
cm1B$AFRdiff <- cm1B$AFR - cm1B$AFRpredict

cm2B <- AFR_attr_notime(tmp.f,tmp.cmdb,at,at,12,dev = 'TS')
x2 <- cm2B$item[1:12];y2 <- cm2B$AFR[1:12]
cm2B$AFRpredict <- predict(lm(y2~x2),data.frame(x2 = cm2B$item))
cm2B$AFRdiff <- cm2B$AFR - cm2B$AFRpredict

cmB <- rbind(cm1B,cm2B)
cmB <- factorX(subset(cmB,!is.na(AFR)))
cmB <- classExchg(cmB)
cmB <- subset(cmB,item >=3 & item <= 4)

p2 <- AFR_plot_warranty(cmB,'fig1Warranty')