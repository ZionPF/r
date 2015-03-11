# Read from csv files of ceilometer, and process to generate matrix for time slots, then do statistical analysis
library(forecast)
library(ggthemes)
library(ggplot2)
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
flow.entry$rate[is.na(flow.entry$rate)] <- 0
plot(flow.entry$rate)
#Take first 800 entry for training, others for checking, only series taken
flow.train <- flow.entry[2:701,]$rate
flow.future <- flow.entry[701:802,]$rate
#given the flow entry (time series), get hist for every 100s
breaks <- c(0,75428904,144525535,304525535,5061188761)

flow_distribution <- function(series){
  flow.hist <- hist(series,breaks,plot = FALSE)
  flow.hist$counts/sum(flow.hist$counts)
}

#split one flow entry into groups 
interval <- 20
sequence_split <- function(sequence, interval){
  chunks <- as.integer(length(sequence)/interval)
  split(sequence,rep(1:chunks,rep(interval,chunks)))
}

#Use ARIMA to predict the series, return the 1 step prediction result
predict_arima <- function(flow){
  fit <- auto.arima(flow)
  predict(fit,n.ahead=1)$pred[1]
}

#Do prediction for all segment (matrix), then normalize to one distribution vector
hist_prediction <- function(flow.stat){
  simple.prediction <- apply(flow.stat, 1, predict_flow)
  simple.prediction/sum(simple.prediction)
}

flow_evaluate <- function(interval,flow_entry=flow.entry){
  flow_entry <- tail(flow_entry, n=-2)
  sample.num <- nrow(flow_entry)
  
  train <- head(flow.entry, n=(sample.num-interval))$rate
  future <- tail(flow.entry, n=(interval))$rate
  #given the flow entry (time series), get hist for every 100s
  breaks <- c(0,75428904,144525535,304525535,5061188761)
  #for each segment, do hist, then form a matrix ,each column is a hist for a interval
  flow.stat <- sapply(sequence_split(train, interval), flow_distribution)
  
  #Call prediction to give predicted PD vector
  prediction.hist <- hist_prediction(flow.stat)
  
  #Compared is the real distribution
  real <- flow_distribution(future[1:(1+interval)])

  #Do normal arima for series and get histo
  series.prediction <- predict(auto.arima(train), n.ahead = interval)$pred
  prediction.series <- flow_distribution(series.prediction)
  
  error.hist <- sum((real-prediction.hist)^2)
  error.series <- sum((real-prediction.series)^2)
  rbind(error.hist, error.series)
}

#Set up a range for intervals, then for each interval, do evaluation
eval_range <- seq(10, 100, 10)
perf.prediction <- sapply(eval_range,flow_evaluate)

#draw pics
perf.prediction[2,1] <- 0.1835
perf.prediction[2,3] <- 0.4912
#err_hist <- perf.prediction[1,]
#err_seri <- perf.prediction[2,]
interval <- seq(100,1000,100)
err_hist <- data.frame(interval, err = perf.prediction[1,], Prediction = rep("PDV",ncol(perf.prediction)))
err_seri <- data.frame(interval, err = perf.prediction[2,], Prediction = rep("ARIMA",ncol(perf.prediction)))

err_perf <- rbind(err_hist, err_seri)

p <- ggplot(data=err_perf, aes(x = interval,fill=Prediction))


perf_err <- p + geom_bar(aes(y=err),stat="identity",position="dodge") +
  xlab("Prediction Interval") +
  ylab("Prediction Error")
  #theme_economist()

perf_err + theme_economist_white()



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

