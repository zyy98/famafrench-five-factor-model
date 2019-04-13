#读取数据
a <- read.csv("", sep=",", header=T)
b <- read.csv("", sep=",", header=T)
#检验缺失值
which(a == -99.99) 
which(a == -999)
which(b == -99.99) 
which(b == -999)
#处理表格大小
l <- length(b)
end <- 668
#提出因子
time <- a[1:end,1]
rmrf <- a[1:end,2]
smb <- a[1:end,3]
hml <- a[1:end,4]
rmw <- a[1:end,5]
cma <- a[1:end,6]
rf <- a[1:end,7]
mom <- a[1:end,8]

#算GRS相关的两个值
library(GRS.test)
factor.mat = a[1:end,c(2,3,4,5,6,8)]                        #改变数值调整因子
ret.mat = b[1:end,2:l] - rf
f1<- round(GRS.test(ret.mat,factor.mat)$GRS.stat,2)
f2<- round(GRS.test(ret.mat,factor.mat)$GRS.pval,3)

# A|ai|   s^2/Arai^2   R^2
fun2 = function(x){
  fit3 <- lm((x-rf) ~ rmrf + smb + hml + rmw + cma + mom)        #调整因子
  x <- summary(fit3)
  res <- as.matrix(cbind(fit3$coefficients[1], x$coefficients[1,2], x$adj.r.square))
  return(res)
}
f<- apply(b[1:end,2:l],2,fun2)
ai<- f[1,]
f3 <- round(mean(abs(ai)),3)
#A|ai|／A|ri|
ri = list()
for (i in 2:l){
  d = mean(b[1:end,i] - rf - mean(rmrf))
  ri = cbind(ri,d)
}
ri = as.double(ri)
f4<- round(mean(abs(ai))/mean(abs(ri)),2)
#Aai^2/Ari^2
f5<- round(mean((ai)^2)/mean((ri)^2),2)
std <- f[2,]
f6 <- round(mean(std^2)/mean(ai^2),2)
Rsquare <- f[3,]
f7<- round(mean(Rsquare),2)

table2<- cbind(f1,f2,f3,f4,f5,f6,f7)
table2
write.csv(table2,"/Users/zhangyuanyiGO/Desktop/table2.csv")


