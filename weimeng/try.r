library(RMySQL)
library(ggplot2)
require(ggmap)
require(maps)
require(plyr)
library(scatterplot3d)
library(kernlab)
library(animation)

mydb = dbConnect(MySQL(), user='weimeng', password='weimeng_location', dbname='bd_community', host='202.121.178.239')
dbSendQuery(mydb,'SET NAMES utf8') 

#get gps history for top 1000 users
gps =  dbSendQuery(mydb, "select * from historyposition201501_top1000_copy")
gps_data = fetch(gps, n=-1)

# get student and elder people info
old =  dbSendQuery(mydb, "select * from oldpeople")
old = fetch(old, n=-1)
student =  dbSendQuery(mydb, "select * from student")
student_info = fetch(student, n=-1)

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




#以下读取汇聚过的信息进行分析
merged_data = read.table("~/r/weimeng/r3_f2_merge.txt", sep="\t",header = TRUE)[,-1]
#names(merged_data) <- tail(names(merged_data))
merged_data <- merged_data[merged_data$cl_lng>30 &merged_data$cl_lng<125 & merged_data$cl_lat>30,]

merged_trace <- merge(merged_data,identity, by="imei",all.x=TRUE)
merged_trace$st <- strptime(merged_trace$st, format='%Y/%m/%d %H:%M:%S')
merged_trace$et <- strptime(merged_trace$et, format='%Y/%m/%d %H:%M:%S')
merged_trace$duration <- as.numeric(merged_trace$et - merged_trace$st)


old_trace <- merged_trace[(merged_trace$identity == "old") & !is.na(merged_trace$identity),]
student_trace <- merged_trace[(merged_trace$identity == "student") & !is.na(merged_trace$identity),]

#Merge student gender, school into student trace
student <- data.frame(imei = student_info$fk_imei, school = student_info$school)
student_detail <- merge(student_trace,student, by="imei",all.x=TRUE)

#
student_stay <- student_detail[student_detail$duration > 60,]
#sort this by st
student_order <- student_stay[order(student_stay$st),]

student_1 <- student_detail[student_detail$imei == 862950021201390,]
student_1_order <- student_1[order(student_1$st),]

old_1 <- old_trace[old_trace$imei == 357698861534352,]


#select one school from student_detail 淀山湖幼儿园
school_1 <- student_stay[student_stay$school == "淀山湖幼儿园" | student_stay$school == "淀山湖小学",]

s3d <- scatterplot3d(x = student_1$cl_lng[student_1$st$mday==3], y = student_1$cl_lat[student_1$st$mday==3], z = student_1$st$hour[student_1$st$mday==3], type="h",  angle=55, scale.y=0.7, pch=16, main="scatterplot3d - 5")



#For each user, we get a location center represents it's normal activity
student_daytime <- student_detail[student_detail$st$wday>0 & student_detail$st$wday<6 & student_detail$st$hour>10 & student_detail$st$hour<12,]

user_center <- function(imei){
  student_trace <- student_daytime[student_daytime$imei == imei,]
  weight <- as.integer(student_trace$duration/60) +1
  center_lng <- sum(student_trace$cl_lng * weight, na.rm = TRUE)/sum(weight)
  center_lat <- sum(student_trace$cl_lat * weight, na.rm = TRUE)/sum(weight)
  c(imei=imei,c_lng=center_lng,c_lat=center_lat)
}

#得到打了学校label的学生feature
student_feature <- t(sapply(as.numeric(levels(factor(student_detail$imei))), user_center ))
student_feature_info <- merge(student_feature,student, by="imei",all.x=TRUE)

valid_feature <- student_feature_info[!is.na(student_feature_info$c_lng) & !is.na(student_feature_info$c_lat),]
#得到529个 valid的学生条目，现对其进行nn分类学习
studentdata <- valid_feature[,-1]

studentdata$school[studentdata$school=="商榻幼儿园"] <- "商榻小学"
studentdata$school[studentdata$school=="淀山湖小学"] <- "淀山湖小学"

studentdata$school <- as.factor(studentdata$school)
studentTrainData = sample(1:529,370)
studentValData = setdiff(1:529,studentTrainData)
ideal <- class.ind(valid_feature$school)

nn_evaluate <- function(size){
  studentANN = nnet(studentdata[studentTrainData,-3], ideal[studentTrainData,], size=size, softmax=TRUE)
  prediction <- predict(studentANN, studentdata[studentValData,-3], type="class")
  
  sum(prediction == studentdata[studentValData,]$school)/length(studentValData)
  
}

nn_result <- sapply(1:30, nn_evaluate)
plot(nn_result)

studentANN = nnet(studentdata[studentTrainData,-3], ideal[studentTrainData,], size=16, softmax=TRUE)
prediction <- predict(studentANN, studentdata[studentValData,-3], type="class")


#以上，需要调整NN size，15时效果尚可，其他不好

#下面，用SVM试一下


rbf <- rbfdot(sigma=0.2)
studentSVM <- ksvm(school~.,data=studentdata[studentTrainData,],type="C-bsvc",kernel=rbf,C=10,prob.model=TRUE)
fitted(studentSVM)
predict(studentSVM, studentdata[studentTrainData,-3], type="probabilities")




#以下，划出几个经典情景的trace情况

valid_traces <- valid_gps[,c("imei","identity","longitude","latitude","locateTime")]
valid_traces$day <- valid_traces$locateTime$mday
valid_traces$wday <- valid_traces$locateTime$wday
valid_traces$wday[valid_traces$wday == 0] <- 7
valid_traces$hour <- valid_traces$locateTime$hour
valid_traces <- valid_traces[,-5]



movie_weekday <- function(id){
  student_1 <- valid_traces[valid_traces$imei ==id & valid_traces$wday<6,]
  student_trace <- ddply(student_1,c("longitude","latitude","hour","day","wday"),summarise, frequency=length(longitude))
  
  saveHTML({
    for (i in sort(unique(student_trace$day))) {
      print(ggplot(data=student_trace[student_trace$day==i,], aes(x = longitude)) +
              geom_point(aes(y=latitude, color=hour, size=frequency+1)) +
              scale_colour_gradientn(colours=rainbow(24, 0.6)) +
              ggtitle(paste("User Trace(Weekdays) :", toString(id)," Day: ", toString(i))) +
              xlim(range(student_trace$longitude)) +
              ylim(range(student_trace$latitude))
      )
    }
  }, interval = 0.5, htmlfile = paste("Weekdays_",toString(id),".html",sep=""), ani.width = 600, ani.height = 600)
}


movie_weekend <- function(id){
  student_1 <- valid_traces[valid_traces$imei ==id & valid_traces$wday>5,]
  student_trace <- ddply(student_1,c("longitude","latitude","hour","day","wday"),summarise, frequency=length(longitude))
  
  saveHTML({
    for (i in sort(unique(student_trace$day))) {
      print(ggplot(data=student_trace[student_trace$day==i,], aes(x = longitude)) +
              geom_point(aes(y=latitude, color=hour, size=frequency+1)) +
              scale_colour_gradientn(colours=rainbow(24, 0.6)) +
              ggtitle(paste("User Trace(Weekend) :", toString(id)," Day: ",toString(i))) +
              xlim(range(student_trace$longitude)) +
              ylim(range(student_trace$latitude))
      )
    }
  }, interval = 0.5, htmlfile = paste("Weekend_",toString(id),".html",sep=""), ani.width = 600, ani.height = 600)
}

movie_weekend(862950021406379)
movie_weekday(862950021406379)


gif_weekday <- function(id){
  student_1 <- valid_traces[valid_traces$imei ==id & valid_traces$wday<6,]
  student_trace <- ddply(student_1,c("longitude","latitude","hour","day","wday"),summarise, frequency=length(longitude))
  
  saveGIF({
    for (i in sort(unique(student_trace$day))) {
      print(ggplot(data=student_trace[student_trace$day==i,], aes(x = longitude)) +
              geom_point(aes(y=latitude, color=hour, size=frequency+1)) +
              scale_colour_gradientn(colours=rainbow(24, 0.6)) +
              ggtitle(paste("User Trace(Weekdays) :", toString(id)," Day: ", toString(i))) +
              xlim(range(student_trace$longitude)) +
              ylim(range(student_trace$latitude))
      )
    }
  }, interval = 2, movie.name = paste("Weekdays_",toString(id),".gif",sep=""), ani.width = 600, ani.height = 600)
}


gif_weekend <- function(id){
  student_1 <- valid_traces[valid_traces$imei ==id & valid_traces$wday>5,]
  student_trace <- ddply(student_1,c("longitude","latitude","hour","day","wday"),summarise, frequency=length(longitude))
  
  saveGIF({
    for (i in sort(unique(student_trace$day))) {
      print(ggplot(data=student_trace[student_trace$day==i,], aes(x = longitude)) +
              geom_point(aes(y=latitude, color=hour, size=frequency+1)) +
              scale_colour_gradientn(colours=rainbow(24, 0.6)) +
              ggtitle(paste("User Trace(Weekends) :", toString(id)," Day: ", toString(i))) +
              xlim(range(student_trace$longitude)) +
              ylim(range(student_trace$latitude))
      )
    }
  }, interval = 2, movie.name = paste("Weekends_",toString(id),".gif",sep=""), ani.width = 600, ani.height = 600)
}

gif_weekday(862950021418861)
gif_weekend(862950021418861)



person_list <- data.frame(imei=c(862950021635977,862950021635977,862950021185445,862950021685568,862950021627974,357698861543601,862950021682177,862950021634301,862950021196244,862950021202208,357698861527158,357698861544013,357698861543171,357698861530855,357698861547065,357698861542918,357698861521961,357698861529378))
person_list <- merge(person_list,identity,by="imei",all.x=TRUE)

lapply(person_list$imei,movie_trace)



#针对用户生成静态图

student_1 <- valid_traces[valid_traces$imei ==862950021406379 & valid_traces$day==17,]
student_trace <- ddply(student_1,c("longitude","latitude","day"),summarise, frequency=length(longitude), hour=min(hour))
p <- ggplot(data=student_trace, aes(x = longitude)) 
p + geom_point(aes(y=latitude, color=hour, size=frequency), position="dodge") +
  scale_colour_gradientn(colours=rainbow(24, 0.5)) +
  xlim(range(student_trace$longitude)) +
  ylim(range(student_trace$latitude)) +
  ggtitle(paste(toString(862950021406379)," Day: ", toString(17)))
