#F1. AFR calculate(herited from NewSC16/base_func.R)
ioAFR <- function(io,f,attr,attr_numDisk = 'numD',diskCount = 1){
  # t1 <- melt(table(io[,attr]))
  t1 <- aggregate(io[[attr_numDisk]],by = lapply(attr,function(x)io[[x]]),sum)
  t2 <- melt(table(f[,attr]))
  names(t1) <- names(t2)
  if(length(attr) == 1){
    names(t1)[1] <- attr
    names(t2)[1] <- attr
  }
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'count','fCount')
  tMerge$AFR <- tMerge$fCount/tMerge$count/diskCount*600
  tMerge <- subset(tMerge,!is.na(AFR))
}

# F2. BER calculate
BER_gen <- function(io,f,attr,bitattr,diskCount = 1){
  t1 <- aggregate(io[[bitattr]],by = lapply(attr,function(x)io[[x]]),sum)
  t2 <- melt(table(f[,attr]))
  names(t1) <- names(t2)
  if(length(attr) == 1){
    names(t1)[1] <- attr
    names(t2)[1] <- attr
  }
  tMerge <- merge(t1,t2,by = attr,all = T)
  names(tMerge) <- c(attr,'bitCount','fCount')
  tMerge$BER <- tMerge$fCount/tMerge$bitCount/diskCount
  tMerge <- subset(tMerge,!is.na(BER))
}

# F3. Cut by proposed divide points in order to eliminate the unbalanced tail parts
cutX <- function(arr,seqArr,highpoint){
  minDT <- min(arr)
  maxDT <- max(arr)
  seqArrFinal <- c(minDT,seqArr,maxDT)
  brokepoint <- c(seqArr,highpoint)
  r <- fct2num(cut(arr,seqArrFinal,brokepoint))
}

