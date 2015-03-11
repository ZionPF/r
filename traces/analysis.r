#for(file_name in list.files("./univ1_csv")){
#	file_dir <- paste("~/r/traces/univ1_csv/",file_name,sep="")
#	print(file_dir)
#	csv_data <- read.csv(file=file_dir,row.names=NULL,head=TRUE,sep=",")
#	print(head(csv_data))
#}

#csv_data <- read.csv(file="~/r/traces/univ1_csv/univ1_pt1.csv",row.names=NULL,head=TRUE,sep=",")

#select flows for certain pair
#csv_data[csv_data$ip.src=="41.177.117.184" & csv_data$ip.dst=="41.177.3.224",]

#To turn a colume into real number (no factor number),do it like this:
#time_vector <- as.numeric(as.character(csv_data$frame.time_relative))


#To read big tables, do like this:
#http://www.biostat.jhsph.edu/~rpeng/docs/R-large-tables.html

#library(stringr)
#dat <- "~/r/traces/univ1_csv/univ1_pt1.csv"
#tab5rows <- read.csv(dat, header = TRUE, nrows = 5, sep=",")
#classes <- sapply(tab5rows,class)
#lines <- readLines(dat)
#Now delete the lines with wrong colume number
#lines <- lines[str_count(lines,",")<4]
#csv_data <- read.csv(text=lines, header = TRUE, sep=",", colClasses=classes)

csv_data <- read.csv(file="~/r/traces/univ1_csv/univ1_pt1.csv",row.names=NULL,head=TRUE,sep=",")
#Data formating
csv_data$frame.time_relative = as.numeric(as.character(csv_data$frame.time_relative))
csv_data$frame.len = as.integer(as.character(csv_data$frame.len))
#Data cleaning
csv_data <- csv_data[complete.cases(csv_data),]
empty_entries <- which(csv_data$ip.src==""| csv_data$ip.dst=="")
csv_data <- csv_data[-empty_entries,]

library(data.table)
library(ggplot2)
library(grid)
library(gridExtra)
#flow.record = data.table(csv_data[csv_data$ip.src=="XX" & csv_data$ip.dst=="XX",])
flow_src <- data.table(csv_data[csv_data$ip.src=="244.3.160.239",])
dst_list <- names(summary(flow_src$ip.dst))
time_gap <- 20


traffic_sec <- csv_data

traffic_sec$frame.time_relative <- as.integer(traffic_sec$frame.time_relative)

traffic_rate <- ddply(csv_data,c("ip.src","ip.dst","frame.time_relative"),summarise, byte=sum(frame.len))


src_traffic <- flow_src[,list(summ=sum(frame.len)),by=frame.time_relative]


#let's plot one of the flows comparing with normal flow
flow.plot <- function(dst){
  flow.entry <- flow_src[flow_src$ip.dst == dst,]
  flow.entry$frame.time_relative <- as.integer(flow.entry$frame.time_relative)
  flow_traffic <- flow.entry[,list(summ=sum(frame.len)),by=frame.time_relative]
  flow_traffic$summ <- flow_traffic$summ / 1000
  #flow_traffic <- flow_traffic[1:25]
  plot(flow_traffic$summ)
  print(mean(flow_traffic$summ))
  flow.norm <- rnorm(nrow(flow_traffic),mean=mean(flow_traffic$summ),sd=10)
  time <- 1:nrow(flow_traffic)
  flow.comp <- data.frame(time,flow_traffic$summ,flow.norm)
  p <- ggplot(data=flow.comp, aes(x = time,color=Variables))
  p + geom_point(aes(y=flow_traffic.summ,color="Noisy Flow")) +
    #geom_line(aes(y=flow.norm,color="Steady Flow")) +
    xlab("Time (s)") +
    ylab("Bandwidth Consumption (MB/s)")
}

flow.plot("244.3.224.127")

lapply(dst_list[1:5],flow.plot)

#Now given a flow, we would like to get it's histgram

flow.plot <- function(dst){
  flow.entry <- flow_src[flow_src$ip.dst == dst,]
  flow.entry$frame.time_relative <- as.integer(flow.entry$frame.time_relative)
  flow_traffic <- flow.entry[,list(summ=sum(frame.len)),by=frame.time_relative]
  flow_traffic$summ <- flow_traffic$summ / 1000
  #flow_traffic <- flow_traffic[1:25]
  #plot(flow_traffic$summ)
  print(mean(flow_traffic$summ))
  flow.norm <- rnorm(nrow(flow_traffic),mean=mean(flow_traffic$summ),sd=10)
  time <- 1:nrow(flow_traffic)
  flow.comp <- data.frame(time,flow_traffic$summ,flow.norm)
  hist(flow_traffic$summ)
  p <- ggplot(data=flow.comp, aes(x = time,color=Variables))
  p + geom_point(aes(y=flow_traffic.summ,color="Noisy Flow")) +
    #geom_line(aes(y=flow.norm,color="Steady Flow")) +
    xlab("Time (s)") +
    ylab("Bandwidth Consumption (MB/s)")
}





for(i in dst_list[dst_list != "(Other)"]){
  print(i)
  #flow_record is a flow, with each packet_length and timestamp:
  flow_record <- flow_src[flow_src$ip.dst == i,]
  flow_record$frame.time_relative <- as.integer(flow_record$frame.time_relative)
  #split the flow trace into several segments
  if(max(flow_record$frame.time_relative)-min(flow_record$frame.time_relative) > time_gap){
    range <- seq(min(flow_record$frame.time_relative),max(flow_record$frame.time_relative),time_gap)
    #flow_seg_list is several lists cut from the flow record based on time gap
    flow_seg_list <- split(flow_record, cut(flow_record$frame.time_relative,range))
    mean_list <- numeric()
    median_list <- numeric()
    for(i in flow_seg_list){
      flow_traffic <- i[,list(summ=sum(frame.len)),by=frame.time_relative]
      #print("******")
      #print(flow_traffic)
      mean_list <- c(mean_list,mean(flow_traffic$summ,na.rm=TRUE))
      median_list <- c(median_list,median(flow_traffic$summ,na.rm=TRUE))
      #ggplot(mean_list)
    }
    
    # Flow_traffic is a flow, with bytes number for every second
    flow_traffic <- flow_record[,list(summ=sum(frame.len)),by=frame.time_relative]
    
    p <- ggplot(data=flow_traffic, aes(x = frame.time_relative)) 
    p + geom_point(aes(y=summ),color="sienna")
    
    print("******")
    print(mean_list)
    print(median_list)
  }
 
#try edit and see Rstudio  
  
#  #For each segment flow, draw picture
#  attach(mtcars)
#  #par(mfrow=c(length(flow_seg_list),1))
#  par(mfcol=c(length(flow_seg_list),1), oma=c(1,1,0,0), mar=c(1,1,1,0), tcl=-0.1, mgp=c(0,0,0))
#  for(i in flow_seg_list){
#    print("******")
#    flow_traffic <- i[,list(summ=sum(frame.len)),by=frame.time_relative]
#    hist(flow_traffic$summ)
#    cat ("Press [enter] to continue")
#    line <- readline()
#  }
  
  #Get a list of flow_records
#  split_flow_list <- split(flow_record,flow_record$frame.time_relative)
#  flow_traffic <- flow_record[,list(summ=sum(frame.len)),by=frame.time_relative]
#  hist(flow_traffic$summ)
#  cat ("Press [enter] to continue")
#  line <- readline()
}
#flow_traffic <- flow.record[,list(summ=sum(frame.len)),by=frame.time_relative]




