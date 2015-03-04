library(RMySQL)
library(ggplot2)
require(ggmap)
require(maps)
require(plyr)
library(scatterplot3d)

mydb = dbConnect(MySQL(), user='weimeng', password='weimeng_location', dbname='bd_community', host='202.121.178.239')

#get gps history for top 1000 users
gps =  dbSendQuery(mydb, "select * from historyposition201501_top1000_copy")
gps_data = fetch(gps, n=-1)

# get student and elder people info
old =  dbSendQuery(mydb, "select * from oldpeople")
old = fetch(old, n=-1)
student =  dbSendQuery(mydb, "select * from student")
student = fetch(student, n=-1)

# Merge student and old to get identity match imei:old/student
student <- data.frame(imei = student$fk_imei, identity = rep("student",nrow(student)))
old <- data.frame(imei = old$fk_imei, identity = rep("old",nrow(old)))
identity <- merge(student,old,by=c("imei","identity"),all=TRUE)

#Merge gps history with identity, so we know who's student
gps_data <- merge(gps_data,identity, by="imei",all.x=TRUE)

range(gps_data$longitude)
range(gps_data$latitude)

gps_data$locateTime <- strptime(gps_data$locateTime, format='%Y-%m-%d %H:%M:%S')

valid_gps <- gps_data[(gps_data$longitude < 150) & (gps_data$longitude > 100),]
valid_gps <- valid_gps[(valid_gps$latitude < 35) & (valid_gps$latitude > 28),]
valid_gps$hour <- valid_gps$locateTime$hour
valid_gps$day <- valid_gps$locateTime$mday

# 对全月gps记录，去除多余信息，仅保留需要的
simple_gps <- valid_gps[,c("imei","longitude","latitude","hour","day", "identity")]

#按小时将GPS点取平均
mean_hour <- ddply(simple_gps,c("imei","day","hour","identity"),summarise, longitude=mean(longitude), latitude=mean(latitude))

# Now get top 5 old and student, see traces
samples <- ddply(mean_hour,c("imei","identity"),summarise, num=length(longitude))

old_samples <- samples[samples$identity =="old",]
old_samples <- old_samples[order(-old_samples$num),]
student_samples <- samples[samples$identity =="student",]
student_samples <- student_samples[order(-student_samples$num),]

top_old <- old_samples[1:5,]$imei
top_student <- student_samples[1:5,]$imei
tops <- c(top_old,top_student)

top_users_trace <- mean_hour[mean_hour$imei %in% tops,]
top_student_trace <- mean_hour[mean_hour$imei %in% top_student,]
top_old_trace <- mean_hour[mean_hour$imei %in% top_old,]

p <- ggplot(data=top_old_trace, aes(x = longitude)) 
p + geom_point(aes(y=latitude, color = imei))


#以下对一个用户轨迹进行一个画图
user_1 <- valid_gps[valid_gps$imei=="862950021421956",]

#plot 2 dim graph for user, in one day
plot_user_location <- function (user_id){
  user_sample <- valid_gps[valid_gps$imei== toString(user_id),]
  print(nrow(user_sample))
  p <- ggplot(data=user_sample, aes(x = longitude))
  p + 
    geom_point(aes(y=latitude, color = user_sample$locateTime$hour) )
    #geom_line(aes(y=latitude), linetype = "dotted", size = 0.1) +
    #scale_colour_gradientn(colours=rainbow(4))
}
user_list <- c(862950021421956,  862950021202075, 862950021191377, 862950021413532)

lapply(user_list, plot_user_location)

# 3-d graph for user
s3d <- scatterplot3d(x = user$longitude, y = user$latitude, z = user$relative_time, type="p",  angle=55, scale.y=0.7, pch=3, main="scatterplot3d - 5")

