# 以6-7两月为观察窗口，求这两个月的机器故障对应上架时间的年故障率
# 上架时间区分分0-3个月，3-6个月，6-12个月，1-2年，2-3年，3-4年
# 计算每个区间内的故障机数量和机器总量。
# 例如3-6个月的故障率计算：
#   将时间窗口内的故障机器，故障时的已服役时间为3-6个月的机器作为分子，去重
#   将上架时间距6月份-1个月至-6个月中（即去年12月至5月）上架的机器作为分母。
#   因为这些机器在观察窗口中发生故障是会被计入分子的
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


#########################################################################################################
# # P2.计算年故障率
cm1 <- AFR_attr_notime(tmp.f,tmp.cmdb,'fsTime','shTime',1,dev = 'C')
cm2 <- AFR_attr_notime(tmp.f,tmp.cmdb,'fsTime','shTime',12,dev = 'TS')
cm3 <- AFR_attr_notime(tmp.f,tmp.cmdb,'fsTime','shTime',12,dev = 'TS1T')
cm4 <- AFR_attr_notime(tmp.f,tmp.cmdb,'fsTime','shTime',12,dev = 'TS2T')
cmDev <- AFR_attr_notime(tmp.f,tmp.cmdb,'dev_class_id','dev_class_id',1)
cm <- rbind(cm1,cm2)
cm <- factorX(subset(cm,!is.na(AFR) & item != '6'))
plotCol <- c('item','class','AFR')
naFill <- cbind(expand.grid(item = levels(cm$item),class = levels(factor(cm$class))),AFR = NA)
AFR_plot(rbind(subset(cm,,plotCol),naFill),'Ship Time and Annual Failure Rate',-2)
# # 
# P3.分IO计算年故障率,将3个IO字段各自根据分位点分成N档，把档次标出
#C类
tmp.fC <- subset(tmp.f,dev_class_id == 'C1')
tmp.cmdbC <- subset(tmp.cmdb,dev_class_id == 'C1')
tmp.ioC <- subset(tmp.io,dev_class_id == 'C1')
cmv902C <- AFR_value(tmp.fC,tmp.cmdbC,tmp.ioC,'902',3,lastYears,1)
cmv903C <- AFR_value(tmp.fC,tmp.cmdbC,tmp.ioC,'903',3,lastYears,1)
cmv999C <- AFR_value(tmp.fC,tmp.cmdbC,tmp.ioC,'999',3,lastYears,1)
AFR_value_plot(cmv902C,'Ship Time and IO read (non-storage servers)',25)
AFR_value_plot(cmv903C,'Ship Time and IO Write (non-storage servers)',20)
AFR_value_plot(cmv999C,'Ship Time and IO Time (non-storage servers)',30)
# TS类
tmp.fTS <- subset(tmp.f,dev_class_id != 'C1')
tmp.cmdbTS <- subset(tmp.cmdb,dev_class_id != 'C1')
tmp.ioTS <- subset(tmp.io,dev_class_id != 'C1')
cmv902T <- AFR_value(tmp.fTS,tmp.cmdbTS,tmp.ioTS,'902',3,lastYears,12)
cmv903T <- AFR_value(tmp.fTS,tmp.cmdbTS,tmp.ioTS,'903',3,lastYears,12)
cmv999T <- AFR_value(tmp.fTS,tmp.cmdbTS,tmp.ioTS,'999',3,lastYears,12)
AFR_value_plot(cmv902T,'Ship Time and IO read (storage servers)',25)
AFR_value_plot(cmv903T,'Ship Time and IO Write (storage servers)',20)
AFR_value_plot(cmv999T,'Ship Time and IO Time (storage servers)',30)

# 数据整合与准备
tmpP3.io <- subset(tmp.io,mean_902 > 0 & mean_903 > 0)
tmpP3.cmdb <- tmp.cmdb
tmpP3.f <- tmp.f
tmpP3.io$total902 <- log2(tmpP3.io$mean_902*86400*365)
tmpP3.io$total903 <- log2(tmpP3.io$mean_903*86400*365)
tmpP3.io$total9023 <- log2(tmpP3.io$mean_902*86400*365 + tmpP3.io$mean_903*86400*365)
tmpP3.cmdb$total902 <- tmpP3.io$total902[match(tmpP3.cmdb$svr_asset_id,tmpP3.io$svrid)]
tmpP3.cmdb$total903 <- tmpP3.io$total903[match(tmpP3.cmdb$svr_asset_id,tmpP3.io$svrid)]
tmpP3.cmdb$total9023 <- tmpP3.io$total9023[match(tmpP3.cmdb$svr_asset_id,tmpP3.io$svrid)]
# C类
tmpP3.fC <- subset(tmpP3.f,dClass == 'C',c('ip','svr_id','dClass','dev_class_id','failShiptime'))
tmpP3.cmdbC <- subset(tmpP3.cmdb,dClass == 'C',c('svr_asset_id','dev_class_id','ip',
                                               'shiptimeToLeft','dClass','total902','total903','total9023'))
tmpP3.cmdbC <- subset(tmpP3.cmdbC,!is.na(total902) & !is.na(total903) & !is.na(total9023))
cut902 <- c(10,20,30,50)
cut903 <- c(26,31,34,42)
cut9023 <- c(26,30,39,40,44)
tmpP3.cmdbC$sep902 <- as.numeric(gsub('^\\(|,.*$','',cut(tmpP3.cmdbC$total902,cut902)))
tmpP3.cmdbC$sep903 <- as.numeric(gsub('^\\(|,.*$','',cut(tmpP3.cmdbC$total903,cut903)))
tmpP3.cmdbC$sep9023 <- as.numeric(gsub('^\\(|,.*$','',cut(tmpP3.cmdbC$total9023,cut9023)))

tmpP3.fC$sep902 <- tmpP3.cmdbC$sep902[match(tmpP3.fC$ip,tmpP3.cmdbC$ip)]
tmpP3.fC$sep903 <- tmpP3.cmdbC$sep903[match(tmpP3.fC$ip,tmpP3.cmdbC$ip)]
tmpP3.fC$sep9023 <- tmpP3.cmdbC$sep9023[match(tmpP3.fC$ip,tmpP3.cmdbC$ip)]
tmpP3.fC <- subset(tmpP3.fC,!is.na(sep902) & !is.na(sep903) & !is.na(sep9023))

AFR.totalC <- AFR_attr(tmpP3.fC,tmpP3.cmdbC,'sep9023',6,1)
AFR_value_plot(AFR.totalC,'Ship Time and Read Total (Non-Storage Server)',-1,subdir = '0114io')

# TS类
tmpP3.fTS <- subset(tmpP3.f,dClass == 'TS',c('ip','svr_id','dClass','dev_class_id','failShiptime'))
tmpP3.cmdbTS <- subset(tmpP3.cmdb,dClass == 'TS',c('svr_asset_id','dev_class_id','ip',
                                                 'shiptimeToLeft','dClass','total902','total903'))

tmpP3.cmdbTS <- subset(tmpP3.cmdbTS,!is.na(total902) & !is.na(total903))
cut902 <- c(10,20,30,50)
cut903 <- c(26,31,34,42)
tmpP3.cmdbTS$sep902 <- as.numeric(gsub('^\\(|,.*$','',cut(tmpP3.cmdbTS$total902,cut902)))
tmpP3.cmdbTS$sep903 <- as.numeric(gsub('^\\(|,.*$','',cut(tmpP3.cmdbTS$total903,cut903)))

tmpP3.fTS$sep902 <- tmpP3.cmdbTS$sep902[match(tmpP3.fTS$ip,tmpP3.cmdbTS$ip)]
tmpP3.fTS$sep903 <- tmpP3.cmdbTS$sep903[match(tmpP3.fTS$ip,tmpP3.cmdbTS$ip)]
tmpP3.fTS <- subset(tmpP3.fTS,!is.na(sep902) & !is.na(sep903))

AFR.totalTS <- AFR_attr(tmpP3.fTS,tmpP3.cmdbTS,'sep902',6,12)
AFR_value_plot(AFR.totalTS,'Ship Time and Read Total (Storage Server)',-1,subdir = '0114io')

# 
# # P4.对存储系统配置进行分析（接口，容量，model数，Type,城市，raid)
# # P4.1A 处理接口A
# disk_ip$SATA <- 'MIX'
# disk_ip$SATA[disk_ip$SATA2 == 0] <- 'SATA3'
# disk_ip$SATA[disk_ip$SATA3 == 0] <- 'SATA2'
# # P4.2 处理容量
# modelC <- c('ST3500514NS','ST500NM0011','ST1000NM0011','ST31000524NS','ST3250310NS')
# modelTS <- c('ST31000524NS','ST1000NM0011','ST2000NM0011','ST32000645NS')
# disk_ip$modelF <- ''
# disk_ip$modelF[disk_ip$disk_model %in% modelC[1:2] & 
#                  disk_ip$total == 500 & disk_ip$dClass == 'C'] <- '500G(C)'
# disk_ip$modelF[disk_ip$disk_model %in% modelC[3:4] & 
#                  disk_ip$total == 1000 & disk_ip$dClass == 'C'] <- '1000G(C)'
# disk_ip$modelF[disk_ip$disk_model %in% modelC[5] & 
#                  disk_ip$total == 250 & disk_ip$dClass == 'C'] <- '250G(C)'
# disk_ip$modelF[disk_ip$modelA %in% modelTS[1:2] &  disk_ip$dClass == 'TS' &
#                  (disk_ip$modelB == '' | disk_ip$modelB %in% modelTS[1:2]) & 
#                  disk_ip$disk_model_c1 <= 2] <- '1000G(TS)'
# disk_ip$modelF[disk_ip$modelA %in% modelTS[3:4] &  disk_ip$dClass == 'TS' &
#                  (disk_ip$modelB == '' | disk_ip$modelB %in% modelTS[3:4]) & 
#                  disk_ip$disk_model_c1 <= 2] <- '2000G(TS)'
# # P4.3 TS机型的城市
# cmdb$city <- factor(substr(cmdb$idc_name,1,2))
# city <- list(c('深圳','广州','东莞','香港','汕头'),
#           c('上海','杭州','南京'),
#           c('天津','济南','北京'),
#           c('西安','成都'))
# cmdb$cityF <- ''
# cmdb$cityF[cmdb$city %in% city[[1]]] <- 'South of CN'
# cmdb$cityF[cmdb$city %in% city[[2]]] <- 'East of CN'
# cmdb$cityF[cmdb$city %in% city[[3]]] <- 'North of CN'
# cmdb$cityF[cmdb$city %in% city[[4]]] <- 'West of CN'
# # P4.4 OS提取
# cmdb$OS <- gsub('-.*$','',cmdb$os_kernal)
# OS <- c('2.6.16.60','2.6.32.43')
# cmdb$OSF <- ''
# cmdb$OSF[cmdb$OS %in% OS] <- cmdb$OS[cmdb$OS %in% OS]
# # Merge并选择col
# col.cmdb <- c('svr_asset_id','ip','dev_class_id','type_name','raid','dClass','cityF','OSF','shiptimeToLeft')
# col.diskIP <- c('ip','SATA','modelF','disk_model_c1')
# strConf <- merge(cmdb[,col.cmdb],disk_ip[,col.diskIP],by = 'ip',all = T)
# # 处理接口B
# strConf$SATA[strConf$dev_class_id %in% class_B] <- 'SAS'
# strConf <- factorColX(strConf,c('dClass','cityF','OSF','SATA','modelF'))
# # 为故障单添加字段
# data.fM <- merge(strConf[,c(1,4:ncol(strConf))],data.f,by = 'ip',all.y = T)
# 
# # P4.5 分别处理
# # # AFR for dev
# # data.dev <- subset(strConf,dClass != '' & dClass != 'B' & 
# #                      !(SATA == 'MAX' & shiptimeToLeft >= 4) & ip %in% cmdbio$ip)
# # data.dev$dClass1 <- ''
# # data.dev$dClass1[data.dev$dClass == 'C'] <- 'Non-Storage Servers' 
# # data.dev$dClass1[data.dev$dClass == 'TS'] <- 'Storage Servers' 
# # data.fM1 <- subset(data.fM,dClass != '' & dClass != 'B' & ip %in% cmdbio$ip & ip %in% data.dev$ip)
# # data.fM1$dClass1 <- ''
# # data.fM1$dClass1[data.fM1$dClass == 'C'] <- 'Non-Storage Servers' 
# # data.fM1$dClass1[data.fM1$dClass == 'TS'] <- 'Storage Servers' 
# # AFR.dev <- AFR_attr(data.fM1,data.dev,'dClass1',6,1)
# # AFR.dev$AFR[AFR.dev$value == 'Storage Servers'] <- AFR.dev$AFR[AFR.dev$value == 'Storage Servers']/12
# # AFR_value_plot(AFR.dev, 'Ship Time and Annual Failure Rate(No MIX)',-1,subdir = 'Storage Configuration')
# 
# #OS
# AFR.osC <- AFR_attr(data.fM,strConf,'OSF',7,1,dev = 'C')
# AFR.osB <- AFR_attr(data.fM,strConf,'OSF',7,2,dev = 'B')
# AFR.osTS <- AFR_attr(data.fM,strConf,'OSF',7,12,dev = 'TS')
# AFR_value_plot(AFR.osC,'Ship Time and OS version (Connectting server)',-1,subdir = 'Storage Configuration')
# AFR_value_plot(AFR.osB,'Ship Time and OS version (Computting server)',-1,subdir = 'Storage Configuration')
# AFR_value_plot(AFR.osTS,'Ship Time and OS version (Storage server)',-1,subdir = 'Storage Configuration')
# #SATA
# AFR.stC <- AFR_attr(data.fM,strConf,'SATA',6,1,dev = 'C')
# AFR.stB <- AFR_attr(data.fM,strConf,'SATA',7,2,dev = 'B')
# AFR.stTS <- AFR_attr(data.fM,strConf,'SATA',6,12,dev = 'TS')
# AFR_value_plot(AFR.stC,'Ship Time and Interface (Connectting server)',
#                -1,subdir = 'Storage Configuration',,valueFilter = c('SATA2','SATA3'))
# AFR_value_plot(AFR.stB,'Ship Time and Interface (Computting server)',-1,subdir = 'Storage Configuration')
# AFR_value_plot(subset(AFR.stTS,!(value == 'SATA3' & shipTime == '(4,5]')),'Ship Time and Interface (Storage server)',-1,subdir = 'Storage Configuration',cylim = 50)
# #City
# AFR.ctC <- AFR_attr(data.fM,strConf,'cityF',6,1,dev = 'C')
# AFR.ctB <- AFR_attr(data.fM,strConf,'cityF',6,2,dev = 'B')
# AFR.ctTS <- AFR_attr(data.fM,strConf,'cityF',6,12,dev = 'TS')
# AFR_value_plot(AFR.ctC,'Ship Time and location (Connectting server)',-1,subdir = 'Storage Configuration')
# AFR_value_plot(AFR.ctB,'Ship Time and location (Computting server)',-1,subdir = 'Storage Configuration')
# AFR_value_plot(AFR.ctTS,'Ship Time and location (Storage server)',-1,subdir = 'Storage Configuration')
# #model_c
# AFR.mdcTS <- AFR_attr(subset(data.fM,disk_model_c1 <= 4),strConf,'disk_model_c1',6,12,dev = 'TS')
# AFR_value_plot(AFR.mdcTS,'Ship Time and model number (Storage server)',
#                -1,subdir = 'Storage Configuration',cylim = 15)
# # #type_name
# # AFR.tnC <- AFR_attr(data.fM,strConf,'type_name',7,1,dev = 'C')
# # AFR.tnB <- AFR_attr(data.fM,strConf,'type_name',7,2,dev = 'B')
# # AFR_value_plot(AFR.tnC,'Ship Time and blade carrier (Connectting server)',
# #                -1,subdir = 'Storage Configuration')
# # AFR_value_plot(AFR.tnB,'Ship Time and blade carrier (Computting server)',
# #                -1,subdir = 'Storage Configuration',valueFilter = c('2U4SChild','Server'))
# # #model
# # AFR.mdC <- AFR_attr(data.fM,strConf,'modelF',7,1,dev = 'C')
# # AFR.mdTS <- AFR_attr(data.fM,strConf,'modelF',7,12,dev = 'TS')
# # AFR_value_plot(AFR.mdC,'Ship Time and capacity (Connectting server)',-1,subdir = 'Storage Configuration')
# # AFR_value_plot(AFR.mdTS,'Ship Time and capacity (Storage server)',-1,subdir = 'Storage Configuration',cylim = 10)

# # p5. 系统硬盘启停次数特征(999)
# load(file.path(dir_data,'diskLaunchFreq.Rda'))
# names(diskLaunchFreq) <- c('lCount','count','svrid')
# diskLaunchFreq <- subset(diskLaunchFreq,count > 10000)
# diskLaunchFreq$launchRate <- diskLaunchFreq$lCount/diskLaunchFreq$count
# divLR <- quantile(diskLaunchFreq$launchRate[diskLaunchFreq$launchRate != 0],seq(0,1,1/3))
# divLR <- c(0,0.025,0.5)
# diskLaunchFreq$divLR <- as.character(cut(diskLaunchFreq$launchRate,divLR))
# diskLaunchFreq$divLR[diskLaunchFreq$launchRate == 0] <- '0'
# diskLaunchFreq$divLR <- factor(diskLaunchFreq$divLR)
# tmp.io$LaunchR <- diskLaunchFreq$divLR[match(tmp.io$svrid,diskLaunchFreq$svrid)]
# tmp.f$LaunchR <- diskLaunchFreq$divLR[match(tmp.f$svr_id,diskLaunchFreq$svrid)]
# tmp.io$shiptimeToLeft <- cmdb$shiptimeToLeft[match(tmp.io$svrid,cmdb$svr_asset_id)]
# AFR.LRC <- AFR_attr(tmp.f,tmp.io,'LaunchR',6,1,dev = 'C')
# AFR.LRTS <- AFR_attr(tmp.f,tmp.io,'LaunchR',6,12,dev = 'TS')
# AFR.LRC$value <- factor(AFR.LRC$value,levels(AFR.LRC$value)[c(2,1,3)])
# AFR.LRTS$value <- factor(AFR.LRTS$value,levels(AFR.LRTS$value)[c(2,1,3)])
# AFR_value_plot(AFR.LRC,'Ship Time and Launch Frequency (Non-Storage Server)',-1)
# AFR_value_plot(AFR.LRTS,'Ship Time and Launch Frequency (Storage Server)',-1)

# P6.999峰值特征
load(file.path(dir_data,'peak999.Rda'))
peak999 <- subset(peak999,!is.na(rMax) & !is.na(rCsig902) & !is.na(rCsigE100))
div.rMax <- quantile(peak999$rMax,seq(0,1,1/3))
div.rC100 <- quantile(peak999$rC100,seq(0,1,1/3))
div.rM100 <- quantile(peak999$rM100,seq(0,1,1/3))
div.rCsig999 <- quantile(peak999$rCsig999,seq(0,1,1/3))
div.rMsig999 <- quantile(peak999$rMsig999,seq(0,1,1/3))
div.rCsig902 <- quantile(peak999$rCsig902,seq(0,1,1/3))
div.rMsig902 <- quantile(peak999$rMsig902,seq(0,1,1/3))
div.rCsig903 <- quantile(peak999$rCsig903,seq(0,1,1/3))
div.rMsig903 <- quantile(peak999$rMsig903,seq(0,1,1/3))
div.rCsigE100 <- quantile(peak999$rCsigE100,seq(0,1,1/3))

div.rMax <- c(0,100,max(peak999$rMax,na.rm = T))
div.rC100 <- c(0,3,20,max(peak999$rC100))
div.rM100 <- c(0,100,104,max(peak999$rM100))
div.rCsig999[length(div.rCsig999)] <- 1 + div.rCsig999[length(div.rCsig999)]
div.rCsigE100 <- c(0,5,70,max(peak999$rCsigE100))
attr <- c('rMax','rC100','rM100','rCsig999','rMsig999','rCsig902','rMsig902','rCsig903','rMsig903','rCsigE100')
attr <- c('rCsigE100')

for (i in seq(1,length(attr))){
  eval(parse(text = sprintf('peak999$div%s <- as.character(cut(peak999$%s,div.%s))',attr[i],attr[i],attr[i])))
  if(attr[i] == 'rCsigE100'){
    eval(parse(text = sprintf("peak999$div%s[peak999$%s == -1] <- '未知异常'",attr[i],attr[i])))
  }
  eval(parse(text = sprintf("peak999$div%s[peak999$%s == 0] <- '-1'",attr[i],attr[i])))
  eval(parse(text = sprintf("peak999$div%s <- factor(peak999$div%s)",attr[i],attr[i])))
}

tmp.io <- merge(tmp.io,peak999,by = 'svrid')
tmp.f <- merge(tmp.f,peak999,by.x = 'svr_id',by.y = 'svrid')
tmp.io$shiptimeToLeft <- cmdb$shiptimeToLeft[match(tmp.io$svrid,cmdb$svr_asset_id)]
for (i in seq(1,length(attr))){
  eval(parse(text = sprintf("AFR.%sC <- AFR_attr(tmp.f,tmp.io,'div%s',6,1,dev = 'C')",attr[i],attr[i])))
  eval(parse(text = sprintf("AFR.%sTS <- AFR_attr(tmp.f,tmp.io,'div%s',6,12,dev = 'TS')",attr[i],attr[i])))
  if(attr[i] == 'rMax'){
    AFR.rMaxC <- factorX(subset(AFR.rMaxC,value != ' 0'))
    AFR.rMaxTS <- factorX(subset(AFR.rMaxTS,value != ' 0'))
  }else if(attr[i] == 'rCsigE100'){
    AFR.rCsigE100C <- factorX(subset(AFR.rCsigE100C, value != '未知异常'))
    AFR.rCsigE100TS <- factorX(subset(AFR.rCsigE100TS, value != '未知异常'))
  }
  eval(parse(text = sprintf("AFR_value_plot(AFR.%sC,'Ship Time and %s (Non-Storage Server)',yl = -1,
                            subdir = 'peak999',valueFilter = c(' 0','未知异常'),cylim = 60)",attr[i],attr[i])))
  eval(parse(text = sprintf("AFR_value_plot(AFR.%sTS,'Ship Time and %s (Storage Server)',-1,
                            subdir = 'peak999',cylim = 60,valueFilter = c(' 0','未知异常'))",attr[i],attr[i])))
}

# P7.查看故障类型与io特征的关系
tmp.fA <- subset(tmp.f,grepl('硬盘故障',fType))
tmp.fA$fType <- factorX(gsub('_uwork','',tmp.fA$fType))
tmp.fA$failShiptime <- round(tmp.fA$failShiptime)
attr.fA <- names(tmp.fA)[8:ncol(tmp.fA)]
for(i in seq(1,length(attr.fA))){
  eval(parse(text = sprintf("p <- ggplot(tmp.fA,aes(x = fType, fill = factor(%s))) + geom_histogram(position = 'dodge')",attr.fA[i])))
  ggsave(file=file.path(dir_data,'fType_io',paste(attr.fA[i],'.png',sep='')), plot=p, width = 16, height = 12, dpi = 100)
}

# P8.了解是否有虚拟机对应母机的IO数据，看是否有可能对虚拟平台的机器的故障率进行处理。
Server <- read.csv(file = 'C:/MyCloud/论文_系统级硬盘故障分析/数据/可能用得上的数据/Server_fromM1_IEGfile.csv')
VServer <- read.csv(file = 'C:/MyCloud/论文_系统级硬盘故障分析/数据/可能用得上的数据/virtual_Server_fromM1_IEGfile.csv')
VServer$svrid <- factor(gsub('-VM.*','',VServer$svr_asset_id))
VServer$monDev <- Server$dev_class_name[match(VServer$svrid,Server$svr_asset_id)]
MonServer <- data.frame(svrid = levels(VServer$svrid),
                        vmCount  = as.numeric(tapply(VServer$svrid,VServer$svrid,length)))
MonServer$dev_class_name <- Server$dev_class_name[match(MonServer$svrid,Server$svr_asset_id)]
MonServer$ip <- cmdb$ip[match(MonServer$svrid,cmdb$svr_asset_id)]

load(file = file.path(dir_data,'attr.Rda'))

MonInModel <- subset(MonServer,ip %in% disk_ip$ip)
MonInIO <- subset(MonServer, svrid %in% peak999$svrid)

# P9.SMART SN与出厂时间研究
load(file = file.path(dir_data,'smart_1208.Rda'))
reg_ip <- "((\\d+\\.){3}\\d+)(.*)"
smart <- factorX(subset(smart_1208,grepl(reg_ip,ip) & modelNum != ''))
a <- unique(smart$modelNum)

# P10.对读写负载的占比对故障率的影响研究
tmp.io$total <- tmp.io$mean_902 + tmp.io$mean_903
tmp.io$Rperc <- tmp.io$mean_902/tmp.io$total*100
div_rate <- c(0,30,70,100)
# div_rate <- c(0,20,40,60,80,100)
tmp.io$cut_rate <- cut(tmp.io$Rperc,div_rate,include.lowest = T)
tmp.io$lb_rate <- factor(gsub('^\\(|^\\[|,.*$','',tmp.io$cut_rate))
levels(tmp.io$lb_rate) = c('Write Workload Dominant','Balance','Read Workload Dominant')
tmp.f$lb_rate <- tmp.io$lb_rate[match(tmp.f$svr_id,tmp.io$svrid)]

AFR_rateC <- AFR_attr_notime(tmp.f,tmp.io,'lb_rate',1,dev = 'C')
AFR_rateTS <- AFR_attr_notime(tmp.f,tmp.io,'lb_rate',12,dev = 'TS')
AFR_rate <- rbind(AFR_rateC,AFR_rateTS)

#画图
p <- ggplot(AFR_rate,aes(x = item,y = AFR,fill = class)) + geom_bar(stat = 'identity',position = 'dodge') +
  xlab('I/O Workload Distribution') + ylab('Annual Failure Rate (%)') + ggtitle('I/O Workload Distribution and AFR') +
  guides(fill = guide_legend(title=NULL)) + 
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 20),
        legend.text = element_text(size = 30),
        legend.title = element_text(size = 20),
        plot.title = element_text(size = 26, face = 'bold'),
        legend.position = c(.6,.8),
        legend.justification = c(0,0),
        legend.background = element_rect(fill = alpha('grey',0.5)))
print(p)
ggsave(file=file.path(dir_data,'IO_workload_distribution',paste('IO_workload_distribution','.png',sep='')), 
       plot=p, width = 16, height = 12, dpi = 100)