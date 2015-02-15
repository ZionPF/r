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
  
	server_mem <- get_num(log[grep("neutron-server average memory usage",log)])/1000
	agent_cpu <- get_num(log[grep("neutron-dhcp-agent average CPU usage",log)])
	agent_mem <- get_num(log[grep("neutron-dhcp-agent average memory usage",log)])/1000
	total_mem <- get_num(log[grep("Average total system memory free",log)])/1000
	total_cpu <- get_num(log[grep("Average total system CPU used ",log)])

  print(total_cpu)
	df <- data.frame(test_id,avg_time,min_time,med_time,max_time,retry_num,server_cpu,server_mem,agent_cpu,agent_mem,total_cpu,total_mem)
	dataset <- rbind(dataset,df)
	
}

# Sort the Dataset by test id
dataset <- dataset[order(dataset$test_id) , ]
small <- subset(dataset, test_id < 10000)
write.csv(small, file="../test.csv")
p <- ggplot(data=small, aes(x = test_id,color=Variables)) + xlab("Number of Ports")

#Draw total cpu and mem
p.total.cpu <- p + 
  geom_line(aes(y=total_cpu),color="sienna") +
  geom_point(aes(y=total_cpu),color="sienna") +
  ylab("CPU Used (%)") +
  ggtitle("Total System Resource Usage")
p.total.mem <- p + 
  geom_line(aes(y=total_mem),color="slateblue4") +
  geom_point(aes(y=total_mem),color="slateblue4") +
  ylab("Free Memory (MB)")
grid.arrange(p.total.cpu,p.total.mem,ncol=1)

#Draw CPU for neutron server and agent
p.cpu <- p + 
  geom_point(aes(y=server_cpu,color="Neutron Server")) +
  geom_point(aes(y=agent_cpu,color="DHCP Agent")) +
  geom_line(aes(y=server_cpu,color="Neutron Server")) +
  geom_line(aes(y=agent_cpu,color="DHCP Agent")) +
  ylab("CPU Used (%)") +
  ggtitle("CPU and Memory Usage of Neutron Server and DHCP Agent")

p.mem <- p + 
  geom_point(aes(y=server_mem,color="Neutron Server")) +
  geom_point(aes(y=agent_mem,color="DHCP Agent")) +
  geom_line(aes(y=server_mem,color="Neutron Server")) +
  geom_line(aes(y=agent_mem,color="DHCP Agent")) +
  ylab("Memory Used (MB)")

grid.arrange(p.cpu,p.mem,ncol=1)



#Draw statistic for time
p + geom_point(aes(y=min_time,colour="Min")) + 
  geom_point(aes(y=avg_time,colour="Average")) + 
  geom_point(aes(y=max_time,colour="Max")) + 
  geom_point(aes(y=med_time,colour="Median")) + 
  geom_smooth(aes(y=avg_time, col="Average")) + 
  geom_smooth(aes(y=max_time, col="Max")) + 
  geom_smooth(aes(y=min_time, col="Min")) +
#  geom_smooth(aes(y=med_time, col="Median")) +
  ylab("DHCP ACK time (seconds)") +
  ggtitle("DHCP Response Time")

#Draw graph for DHCP Retries

p + geom_point(aes(y=retry_num),color="slateblue4") +
  ylab("Number of DHCP retries") +
  geom_smooth(aes(y=retry_num), col="slateblue4") + 
  ggtitle("DHCP Retries")
