#读取文件
b <- read.csv("/Users/zhangyuanyiGO/Desktop/table_1/factors.csv", sep=",", header=T)
rf <- b[1:end,7]
##每次更改文件路径
a <- read.csv("/Users/zhangyuanyiGO/Desktop/table_5/NI_table_5.csv", sep=",", header=T)
#检验缺失值
which(a == -99.99) 
which(a == -999)
#确定处理表格大小，如更新表格将end改为668
l <- length(a)
end <- 668
#计算excess return的均值方差
d = a[1:end,1]
for(i in 2:l){
  d <- cbind(d, (a[1:end,i] - rf))
}
mean <- round(apply(d[,2:l],2,mean),2)
means<- as.matrix(mean)
##注意ni是7乘5，其它是5乘5
mean_0 <- means[1:7]
for(i in 2:(length(means)/7)){
  mean_0<- rbind(mean_0,means[(7*i-6):(7*i)])
}

sd <- round(apply(d[,2:l],2,sd),2)
sds<- as.matrix(sd)
sd_0 <- sds[1:7]
for(i in 2:(length(sds)/7)){
  sd_0<- rbind(sd_0,sds[(7*i-6):(7*i)])
}

#计算B/M, OP, Inv和自身属性
a1 <- read.csv("/Users/zhangyuanyiGO/Desktop/table_5/average_bm.csv", sep=",", header=T)
a11<- round(apply(a1[1:end,2:l],2,mean),2)
a2 <- read.csv("/Users/zhangyuanyiGO/Desktop/table_5/op.csv", sep=",", header=T)
a22<- round(apply(a2[1:end,2:l],2,mean),2)
a3 <- read.csv("/Users/zhangyuanyiGO/Desktop/table_5/inv.csv", sep=",", header=T)
a33<- round(apply(a3[1:end,2:l],2,mean),2)
a4 <- read.csv("/Users/zhangyuanyiGO/Desktop/table_5/ni.csv", sep=",", header=T)
a44<- round(apply(a4[1:end,2:l],2,mean),2)


#便于输出格式
a111<- as.matrix(a11)
a1_1 <- a111[1:7]
for(i in 2:(length(a111)/7)){
  a1_1<- rbind(a1_1,a111[(7*i-6):(7*i)])
}

a222<- as.matrix(a22)
a2_2 <- a222[1:7]
for(i in 2:(length(a222)/7)){
  a2_2<- rbind(a2_2,a222[(7*i-6):(7*i)])
}
a333<- as.matrix(a33)
a3_3 <- a333[1:7]
for(i in 2:(length(a333)/7)){
  a3_3<- rbind(a3_3,a333[(7*i-6):(7*i)])
}
a444<- as.matrix(a44)
a4_4 <- a444[1:7]
for(i in 2:(length(a444)/7)){
  a4_4 <- rbind(a4_4,a444[(7*i-6):(7*i)])
}
aa <- cbind(mean_0, sd_0, a1_1,a2_2,a3_3,a4_4)
#输出csv
write.csv(aa,"/Users/zhangyuanyiGO/Desktop/aa.csv")


