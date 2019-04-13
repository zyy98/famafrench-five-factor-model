##对下载文件进行了预处理，factor文件中加上了MOM，

#读取文件
a <- read.csv("", sep=",", header=T)
which(a == -99.99) 
which(a == -999)
##每次更改文件路径
b <- read.csv("", sep=",", header=T)
which(b == -99.99) 
which(b == -999)
#确定处理表格大小
l <- length(b)
end <- 668
#定义变量
time <- a[1:end,1]
rmrf <- a[1:end,2]
smb <- a[1:end,3]
hml <- a[1:end,4]
rmw <- a[1:end,5]
cma <- a[1:end,6]
rf <- a[1:end,7]
mom <- a[1:end,8]
#最小二乘拟合，输出参数和参数的t值（三因子法）
fun = function(x){
  fit2 <- lm((x-rf) ~ rmrf)
  s <- summary(fit2)
  ##根据需求选取部分参数输出
  select_variable <- 2          
  result <- cbind(fit2$coefficients[select_variable],s$coefficients[select_variable,3])
}
c <- apply(b[1:end,2:l],2,fun)        #结果
#调整输出格式，便于excel粘贴
##注意ni是7乘5，其它是5乘5，改5为7
cc <- list()
for(j in 1:nrow(c)){
  for(i in 1:(ncol(c)/5)){
    cc<- round(rbind(cc,c[j,(5*i-4):(5*i)]),2)
  }
}
#输出csv
write.csv(cc,"/Users/zhangyuanyiGO/Desktop/3_factors_reg.csv")


#计算hmlo
fit <-lm(hml ~ rmrf + smb + rmw + cma, data = a)
hmlo <- fit$coefficients[1] + fit$residuals
#最小二乘拟合，输出参数和参数的t值（五因子法）
fun2 = function(x){
  fit3 <- lm((x-rf) ~ rmrf + smb + hmlo + rmw + cma)
  s <- summary(fit3)
  ##根据需求调整参数
  select_variable <- c(1:6)
  result <- cbind(fit3$coefficients[select_variable],s$coefficients[select_variable,3])
}
d <- apply(b[1:end,2:l],2,fun2)
#调整输出格式，便于excel粘贴
dd <- list()
for(j in 1:nrow(d)){
  for(i in 1:(ncol(d)/5)){
    dd<- round(rbind(dd,d[j,(5*i-4):(5*i)]),2)
  }
}
#输出csv
write.csv(dd,"/Users/zhangyuanyiGO/Desktop/5_factors_reg.csv")
