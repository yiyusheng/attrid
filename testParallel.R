library(doParallel)
ck <- makeCluster(2)
registerDoParallel(ck)
base <- sample(10,3)
foreach(i = 1:3,.combine = list) %dopar% {
  print(i)
  data.frame(a = sqrt(base[i]),b = base[i]^2,c = i^0.5)
}
stopCluster(ck)