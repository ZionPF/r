
csv_data <- read.csv(file="netflow/output/1",head=TRUE,sep=",")
src_ids <- csv_data$src_id

#get number of nodes in the cluster
node_num <- max(length(levels(csv_data$src_id)),length(levels(csv_data$dst_id)))
#generate matrix as node*node
x<-matrix(nrow=node_num,ncol=node_num)
#make a list for matrix snapshot
matrix_snap <- list(x)

start_time <- csv_data[1,3]
slot_count <- 1

#get a list of node ids
node_list <- levels(factor(c(levels(csv_data$src_id), levels(csv_data$dst_id))))
#form a src_list and a dst_list to merge
src_list <- data.frame(src_key=1:node_num,src_id=node_list)
dst_list <- data.frame(dst_key=1:node_num,dst_id=node_list)

#After merge, matrix_csv is all data with right matrix key to form a matrix
matrix_csv <- merge(src_list,merge(dst_list,csv_data))

	
#This is low in effeciency, but for now， who cares ~_~
for (time_stamp in levels(factor(csv_data$time_in_secs))){
	cat(time_stamp)
	slot <- csv_data[csv_data$time_in_secs== time_stamp,]
	print(slot)
	for (i in 1:nrow(slot)){
		slot_matrix <- matrix(nrow=node_num,ncol=node_num)
		slot_matrix[]
	}
}
	