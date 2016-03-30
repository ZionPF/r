library(forecast)
library(ggthemes)
library(ggplot2)

spark_data <- read.csv(file="~/r/ganglia/summary/slave03.omnilab.sjtu.edu.cn.csv",head=FALSE,sep=":")

data_slave <- spark_data[-1,]
series <- data_slave$V2[seq(1, nrow(data_slave), 10)]
series <- series[series>300000 & !is.na(series)]
plot(series)

hist(series)

a <- series01[2100:2500]
a<- a[450:1000]
a<-a[230:320]

plot(a)

p <- ggplot(data=a)
p + geom_point()
