


csv_data <- read.csv(file="netflow/output/1",head=TRUE,sep=",")
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

#matrix_sequence <- data.frame(time_stamp=NULL,matrix=slot_matrix)
#This is low in effeciency, but for now， who cares ~_~
#slot_matrix is the matrix for a certain second slot

#time interval for each matrix slot
interval <- 5

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
		list_matrix <- list()
		#Now transfer each matrix as a list, so you get a list of lists - -
		#each list is a n*n=361 numbers vector, represents all traffic for that timestamp
		for(snap_matrix in matrix_seg_list){
			print(snap_matrix)
			snap_list <- unlist(as.list(snap_matrix))
			print(snap_list)
			list_matrix[[length(list_matrix)+1]] <- snap_list
			print(list_matrix)
		}
		head(list_matrix)
		last_time = as.numeric(time_stamp)
		matrix_seg_list <- list(slot_matrix) #Use the slot_matrix as the first one of the list
	}
	else{
		matrix_seg_list[[length(matrix_seg_list)+1]] <- slot_matrix
	}
}

