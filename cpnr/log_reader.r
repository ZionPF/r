#Read from logs and generate a CSV file

#list files and select log files
library(ggplot2)
library(grid)
library(gridExtra)
#setwd("./cpnr/logs")
filenames <- list.files()
filenames <- filenames[grep("log",filenames)]

id_list <- gsub(".log","",filenames)

dataset <- data.frame()

## Function: Get Number from the log string
get_num <- function(str){
	a <- gsub("[^0-9.]","",str)
	b <- as.numeric(a)
	if(length(b)==1){
		return(b)
	}
	else{
		return(NA)
	}
	
}

## Loop read each log file and extract data frame
for (file in filenames){
	log <- readLines(file)

	test_id <- as.numeric(gsub(".log","",file))
	print(test_id)
	avg_time <- get_num(log[grep("Average time needed to receive DHCP IP",log)])
	min_time <- get_num(log[grep("Min time needed to receive DHCP IP",log)])
	med_time <- get_num(log[grep("Median time needed to receive DHCP IP",log)])
	max_time <- get_num(log[grep("Max time needed to receive DHCP IP",log)])
	retry_num <- get_num(log[grep("Total number of DHCPDISCOVER retries",log)])

	server_cpu <- get_num(log[grep("neutron-server average CPU usage",log)])
	server_mem <- get_num(log[grep("neutron-server average memory usage",log)])
	agent_cpu <- get_num(log[grep("neutron-dhcp-agent average CPU usage",log)])
	agent_mem <- get_num(log[grep("neutron-dhcp-agent average memory usage",log)])
	total_mem <- get_num(log[grep("Average total system memory free",log)])
	total_cpu <- get_num(log[grep("Average total system CPU idle",log)])

	df <- data.frame(test_id,avg_time,min_time,med_time,max_time,retry_num,server_cpu,server_mem,agent_cpu,agent_mem,total_cpu,total_mem)
	dataset <- rbind(dataset,df)
	
}

# Sort the Dataset by test id
dataset <- dataset[order(dataset$test_id) , ]
small <- subset(dataset, test_id < 5100)

p <- ggplot(data=small, aes(x = test_id,color=Variables)) + xlab("Port Number")

#Draw total cpu and mem
p1 <- p + geom_point(aes(y=total_cpu),color="sienna") +
  ylab("Idle CPU")
p2 <- p + geom_point(aes(y=total_mem),color="slateblue4") +
  ylab("Free Mem")
grid.arrange(p1,p2,ncol=1)

#Draw statistic for time
p + geom_point(aes(y=min_time,colour="Min")) + 
  geom_point(aes(y=avg_time,colour="Average")) + 
  geom_point(aes(y=max_time,colour="Max")) + 
  geom_point(aes(y=med_time,colour="Median")) + 
  geom_smooth(aes(y=avg_time, col="Average")) + 
  geom_smooth(aes(y=max_time, col="Max")) + 
  geom_smooth(aes(y=min_time, col="Min")) +
  geom_smooth(aes(y=med_time, col="Median")) +
  ylab("Time (s)")

