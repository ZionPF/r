# Read from csv files of ceilometer, and process to generate matrix for time slots, then do statistical analysis
library(forecast)

csv_data <- read.csv(file="~/r/netflow/output/1",head=TRUE,sep=",")
src_ids <- csv_data$src_id

#get number of nodes in the cluster
node_num <- max(length(levels(csv_data$src_id)),length(levels(csv_data$dst_id)))
max_flow <- max(csv_data$network.flow.bytes)
#generate matrix as node*node
x<-matrix(nrow=node_num,ncol=node_num)
#make a list for matrix snapshot
matrix_snap <- list(x)


#get a list of node ids
node_list <- levels(factor(c(levels(csv_data$src_id), levels(csv_data$dst_id))))
#form a src_list and a dst_list to merge
src_list <- data.frame(src_key=1:node_num,src_id=node_list)
dst_list <- data.frame(dst_key=1:node_num,dst_id=node_list)

#After merge, matrix_csv is all data with right matrix key to form a matrix
matrix_csv <- merge(src_list,merge(dst_list,csv_data))
#order csv by time stamp:
matrix_csv <- matrix_csv[order(matrix_csv$time_in_secs),]
#Drop the original src/dst ID, just use number as ID
matrix_csv <- id_matrix <- matrix_csv[c("src_key","dst_key","time_in_secs","network.flow.bytes")]

#Gather the dataframe into per src/dst pair and remove duplicate meter entry

flow_data <- ddply(matrix_csv,c("src_key","dst_key","time_in_secs"),summarise, rate=max(network.flow.bytes))

#select a flow :

src <- 7
dst <- 6
flow.entry <- subset(flow_data, src_key == src & dst_key == dst)
plot(flow.entry$rate)

#given the flow entry (time series), get hist for every 100s
breaks <- c(0,75428904,144525535,5061188761)
flow.distribution <- function(series){
  flow.hist <- hist(series,breaks,plot = FALSE)
  flow.hist$counts/sum(flow.hist$counts)
}

#split one flow entry into groups of 100 samples
interval <- 50
sequence.split <- function(sequence, interval){
  chunks <- as.integer(length(sequence)/interval)
  split(flow.entry$rate,rep(1:chunks,rep(interval,chunks)))
}
#for each segment, do hist, then form a matrix ,each column is a hist for a interval
flow.stat <- sapply(sequence.split(flow.entry$rate, interval), flow.distribution)

#matrix_sequence <- data.frame(time_stamp=NULL,matrix=slot_matrix)
#This is low in effeciency, but for now， who cares ~_~
#slot_matrix is the matrix for a certain second slot

#time interval for each matrix slot
interval <- 30

last_time <- csv_data$time_in_secs[1]
matrix_seg_list <- list(matrix(0,nrow=node_num,ncol=node_num))
for (time_stamp in levels(factor(matrix_csv$time_in_secs))){
	cat(time_stamp)
	
	#slot is all the entries in csv with the current
	slot <- matrix_csv[matrix_csv$time_in_secs== time_stamp,]
	print(slot)
	slot_matrix <- matrix(0,nrow=node_num,ncol=node_num)

	#Form a matrix of the same time_stamp
	for (i in 1:nrow(slot)){
		slot_matrix[slot$src_key[i],slot$dst_key[i]] = slot$network.flow.bytes[i]

	}
	#print(slot_matrix)
	#persp(slot_matrix,expand=0.2,zlim=c(0,max_flow))
	if (time_stamp > last_time + interval ){ #exceeded last interval, going into next one
		print("******Entering next interval *********")
		list_matrix <- NULL
		#Now transfer each matrix as a list, so you get a list of lists - -
		#list_matrix: each list is a n*n=361 numbers vector, represents all traffic for that timestamp
		for(snap_matrix in matrix_seg_list){
			print(snap_matrix)
			snap_list <- unlist(as.list(snap_matrix))
			print(snap_list)
			list_matrix <- rbind(list_matrix,snap_list)
			#list_matrix[[length(list_matrix)+1]] <- snap_list
		}
		#hist(list_matrix[,346])
    
    #list_matrix 是这段时间中每一秒的matrix的list，这里matrix已经化为19*19长度的list了
    #下面两个list是对这段时间的数据统计
    mean_list <- apply(list_matrix, 2, function(x) mean(x[x!=0]))
    percentile_list <- apply(list_matrix, 2, function(x) quantile(x[x!=0], c(.7)))
		
    percentile_list[is.na(percentile_list)] <- 0
		mean_list[is.na(mean_list)] <- 0
    
		last_time = as.numeric(time_stamp)
		matrix_seg_list <- list(slot_matrix) #Use the slot_matrix as the first one of the list
	}
	else{
		matrix_seg_list[[length(matrix_seg_list)+1]] <- slot_matrix
	}
}

