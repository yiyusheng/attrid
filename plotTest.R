# plotTest
rm(list = ls())
source('head.R')
library(ggplot2)

####################################
# P1. Add regression line
x <- c(1:100)
y <- 2 + 3 * x + rnorm(100, sd = 1)
lmFit <- lm(y ~ x)

new <- data.frame(x = c(1,2,3))
predict(lmFit,new)
# ggplot(df,aes(x = x, y = y)) + geom_bar(stat = 'identity') +
  # geom_smooth(method = 'lm')

# P2. Fit a gaussian function with difined polynomio function
data <- cmB[cmB$class == 'Nserv',c('item','AFRdiff')]
y <- data$AFRdiff
x <- data$item
# Define a Gaussian function (of four parameters).
f <- function(x, theta)  { 
  m <- theta[1]; s <- theta[2]; a <- theta[3]; b <- theta[4];
  a*exp(-0.5*((x-m)/s)^2) + b
}
#
# Estimate some starting values.
m.0 <- x[which.max(y)]; s.0 <- (max(x)-min(x))/4; b.0 <- min(y); a.0 <- (max(y)-min(y))
#
# Do the fit.  (It takes no time at all.)
fit <- nls(y ~ f(x,c(m,s,a,b)), data.frame(x,y), start=list(m=m.0, s=s.0, a=a.0, b=b.0))
#
# Display the estimated location of the peak and its SE.
summary(fit)$parameters["m", 1:2]

# plot
ggplot(data.frame(x = x,y = f(x,coef(fit))),aes(x = x,y = y)) + geom_line()

# P3.bar + line in one figure
x=c(0:10)
y1=c(0,.5,1,1.5,2,2.5,3,3.5,4,4.5,5)
y2=append(c(1:5),c(6,8,10,12,14,16))
mydata1=data.frame(x=x,line=y2,Type="Line")
mydata2=data.frame(x=x,bar=y1,Type="Bar")

ggplot(data=mydata1) + geom_line(aes(x=x,y=line,linetype=Type)) +
  geom_bar(data=mydata2,aes(x=x,y=bar,fill=Type),stat="identity") +
  scale_fill_manual(values=c("red")) +
  guides(fill = guide_legend(override.aes=list(fill=c("red")))) +
  labs(fill="", linetype="")
