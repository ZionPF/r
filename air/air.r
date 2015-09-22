#Air data
airdata <- read.table(file="~/r/air/air.csv",head=TRUE,sep=",")
plot(airdata$PM2.5分指数)
