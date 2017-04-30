rm(list = ls());source('~/rhead')

#A
dir_dataset <- dir_data15AD
load(file.path(dir_dataset,'b1.Rda'))
DT <- dt_dcast
splitDT <- split(DT,DT$svrid)
load(file.path(dir_data,'failRecord_1407-1506.Rda'))
splitFR <- split(failRecord,failRecord$faultType)
fr <- splitFR[[1]]

#B
require(OIsurv)
data("tongue")
my.surv <- with(tongue,Surv(time[type==1],delta[type==1]))
my.fit <- summary(survfit(my.surv ~ 1))
H.hat <- -log(my.fit$surv)
H.hat <- c(H.hat, tail(H.hat, 1))
