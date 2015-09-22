#Read from logs and generate a CSV file

#list files and select log files
library(ggplot2)
library(grid)
library(gridExtra)
library(scales)
#setwd("./cpnr/logs")


## Function: Get Number from the log string
get_num <- function(str){
  number_str <- sub(".*=","",str)
  a <- gsub("[^0-9.]","",number_str)
  b <- as.numeric(a)
  if(length(b)==1){
    return(b)
  }
  else{
    return(NA)
  }
}

## Function : read log files from certain folder and form dataframe
read_logs <- function(path){
  setwd(path)
  filenames <- list.files()
  filenames <- filenames[grep("log",filenames)]
  id_list <- gsub(".log","",filenames)
  dataset <- data.frame()
  
  ## Loop read each log file and extract data frame
  for (file in filenames){
    log <- readLines(file)
    test_id <- as.numeric(gsub(".log","",file))
    print(file)
    avg_time <- get_num(log[grep("Average time to receive DHCP IP address",log)])
    min_time <- get_num(log[grep("Minimum time to receive DHCP IP address",log)])
    med_time <- get_num(log[grep("Median time needed to receive DHCP IP",log)])
    max_time <- get_num(log[grep("Maximum time to receive DHCP IP address",log)])
    drop_num <- get_num(log[grep("Total number of packets dropped",log)])
    drop_percentage <- get_num(log[grep("Packet drop",log)])
    #print(drop_percentage)
    agent_cpu <- get_num(log[grep("neutron-dhcp-agent average CPU usage",log)])
    agent_mem <- get_num(log[grep("neutron-dhcp-agent average memory usage",log)])/1000
    total_mem <- get_num(log[grep("Average total system memory free",log)])/1000/1000
    total_cpu <- get_num(log[grep("Average total system CPU used",log)])
    #print(total_cpu)
    df <- data.frame(test_id,avg_time,min_time,max_time,drop_num,drop_percentage,agent_cpu,agent_mem,total_cpu,total_mem)
    dataset <- rbind(dataset,df)
  }
  
  # Sort the Dataset by test id
  dataset <- dataset[order(dataset$test_id) , ]
  return(dataset)
}

dataset <- read_logs("~/r/perf/baseline/cpnr_port_scaling_pc0_nc0/")


#small <- subset(dataset, test_id < 10000)
write.csv(dataset, file="../test.csv")

p <- ggplot(data=dataset, aes(x = test_id,color=Variables)) + xlab("Number of Ports")

#Draw total cpu and mem
p.total.cpu <- p + 
  geom_line(aes(y=total_cpu),color="sienna") +
  geom_point(aes(y=total_cpu),color="sienna") +
  ylab("CPU Used (%)") +
  ggtitle("Total System Resource Usage") 
p.total.mem <- p + 
  geom_line(aes(y=total_mem),color="slateblue4") +
  geom_point(aes(y=total_mem),color="slateblue4") +
  ylab("Free Memory (GB)") 
grid.arrange(p.total.cpu,p.total.mem,ncol=1)



#Draw CPU for neutron server and agent
p.cpu <- p + 
  geom_point(aes(y=agent_cpu),color="sienna") +
  geom_line(aes(y=agent_cpu),color="sienna") +
  ylab("CPU Used (%)") +
  ggtitle("CPU and Memory Usage of DHCP Agent") 
p.mem <- p + 
  geom_point(aes(y=agent_mem),color="slateblue4") +
  geom_line(aes(y=agent_mem),color="slateblue4") +
  ylab("Memory Used (MB)")
grid.arrange(p.cpu,p.mem,ncol=1)



#Draw statistic for time
p + geom_point(aes(y=min_time,colour="Min")) + 
  geom_point(aes(y=avg_time,colour="Average")) + 
  geom_point(aes(y=max_time,colour="Max")) + 
  geom_line(aes(y=min_time,colour="Min")) + 
  geom_line(aes(y=avg_time,colour="Average")) + 
  geom_line(aes(y=max_time,colour="Max")) + 
  #geom_smooth(aes(y=avg_time, col="Average")) + 
  geom_smooth(aes(y=max_time, col="Max")) + 
  geom_smooth(aes(y=min_time, col="Min")) +
  ylab("DHCP ACK time (ms)") +
  scale_y_continuous(labels = comma) +
  ggtitle("DHCP Response Time")



#Draw graph for Packet Drops

p + geom_point(aes(y=drop_num ),color="slateblue4") +
  ylab("Packet Drop") +
  geom_smooth(aes(y=drop_num ), col="slateblue4") + 
  ggtitle("DHCP Packet Drops") 


#Draw graph for Packet Drop Percentage

p + geom_point(aes(y=drop_percentage ),color="slateblue4") +
  geom_line(aes(y=drop_percentage ),color="slateblue4") +
  ylab("Packet Drop (%)") +
  #geom_smooth(aes(y=drop_num ), col="slateblue4") + 
  ggtitle("DHCP Packet Drops") 


#Draw CPU Graph
p + 
  geom_point(aes(y=agent_cpu, color="DHCP Agent")) +
  geom_line(aes(y=agent_cpu, color="DHCP Agent")) +
  geom_point(aes(y=total_cpu , color="Total Usage")) +
  geom_line(aes(y=total_cpu , color="Total Usage")) +
  ylab("CPU Used (%)") +
  ggtitle("CPU Usage") 


#Draw Mem Graph
p + 
  geom_point(aes(y=agent_mem , color="Agent Mem Usage")) +
  geom_line(aes(y=agent_mem , color="Agent Mem Usage")) +
  geom_point(aes(y=total_mem * 1000 , color="Total Free Mem")) +
  geom_line(aes(y=total_mem * 1000, color="Total Free Mem")) +
  ylab("Memory Usage(MB)") +
  ggtitle("Memory Usage") 















# Comparing two different churning scenario data


churn_1 <- read_logs("~/r/perf/drop/port_scaling_logs_pc5_nc6")
churn_0 <- read_logs("~/r/perf/zero-churn/port_scaling_logs_pc0_nc0")

churn_1$churn <- rep(TRUE,times = nrow(churn_1))
churn_0$churn <- rep(FALSE,times = nrow(churn_0))

test_dataset <- rbind(churn_1,churn_0)


#Get time list from data set: time/type/churn

extract_time <- function(row){
  #print(row)
  avg <- data.frame(test_id = row$test_id, time=row$avg_time, type="Avg", churn=row$churn)
  max <- data.frame(test_id = row$test_id, time=row$max_time, type="Max", churn=row$churn)
  min <- data.frame(test_id = row$test_id, time=row$min_time, type="Min", churn=row$churn)
  rbind(avg,max,min)
}

time_compare <- extract_time(test_dataset)

p <- ggplot(data=time_compare, aes(x = test_id)) + xlab("Number of Ports") 

#Draw graph for Packet Drops
p + geom_point(aes(y=time, color=type )) +
  geom_line(aes(y=time, color=type, linetype=churn )) +
  ylab("DHCP ACK time (ms)") +
  scale_y_continuous(labels = comma) +
  ggtitle("Comparing DHCP Response Time")





#Draw graph for Packet Drop Percentage
p <- ggplot(data=test_dataset, aes(x = test_id)) + xlab("Number of Ports") 

p + geom_point(aes(y=drop_percentage ,color=churn)) +
  geom_line(aes(y=drop_percentage ,color= churn)) +
  ylab("Packet Drop (%)") +
  ggtitle("Comparing DHCP Packet Drops")

