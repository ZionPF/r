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

flow_data <- ddply(matrix_csv,c("src_key","dst_key","time_in_secs"),summarise, rate=max(network.flow.bytes,na.rm=TRUE))
flow_data$rate[is.na(flow_data$rate)] <- 0


#select a flow :

src <- 10
dst <- 16
flow.entry <- subset(flow_data, src_key == src & dst_key == dst)
flow.entry$rate[is.na(flow.entry$rate)] <- 0
plot(flow.entry$rate)
#Take first 800 entry for training, others for checking, only series taken
flow.train <- flow.entry[2:701,]$rate
flow.future <- flow.entry[701:802,]$rate
#given the flow entry (time series), get hist for every 100s
breaks <- c(0,75428904,144525535,304525535,5061188761)

flow_distribution <- function(series,break_points){
  flow.hist <- hist(series,breaks=break_points,plot = FALSE)
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
  prediction <- predict(fit,n.ahead=1)$pred[1]
  if(prediction < 0) prediction <- 0
  return(prediction)
}

#Do prediction for all segment (matrix), then normalize to one distribution vector
hist_prediction <- function(flow.stat){
  simple.prediction <- apply(flow.stat, 1, predict_arima)
  simple.prediction/sum(simple.prediction)
}

#给定perdiction interval，给出对应的两种预测error
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

#draw pics of prediction error MSE
perf.prediction[2,1] <- 0.1835
perf.prediction[2,3] <- 0.4912
#err_hist <- perf.prediction[1,]
#err_seri <- perf.prediction[2,]
interval <- seq(100,1000,100)
err_hist <- data.frame(interval, err = perf.prediction[1,], Prediction = rep("PDV Prediction",ncol(perf.prediction)))
err_seri <- data.frame(interval, err = perf.prediction[2,], Prediction = rep("Time Series with ARIMA",ncol(perf.prediction)))

err_perf <- rbind(err_hist, err_seri)
err_perf$err <- err_perf$err / 4

p <- ggplot(data=err_perf, aes(x = interval,fill=Prediction))
perf_err <- p + geom_bar(aes(y=err),stat="identity",position="dodge") +
  xlab("Prediction Interval (s)") +
  ylab("Prediction Error MSE")

perf_err + theme_bw() + 
  scale_fill_manual(values=c("deepskyblue4", "lightblue")) +
  theme(legend.justification=c(0,1), legend.position=c(0,1)) +
  theme(legend.title=element_blank()) 



#对上述的情况，加上Noisy Neighbour的定义判断，给出False Positive 和 False Negative的比例（需要对所有流进行）




#给定timestamp & rate，给出我们预测的positive/negative
noisy_pdv <- function(threshold,rate){
  a <- 0
  prediction.hist<-NA
  interval=20
  if(length(rate)>interval*10){
    rate <- tail(rate, n=-2)
    sample.num <- length(rate)
    train <- head(rate, n=(sample.num-interval))
    future <- tail(rate, n=(interval))
    #given the flow entry (time series), get hist for every 100s
    #breaks <- c(0,75428904,144525535,304525535,5061188761)
    breaks <- c(0,threshold,60000000)
    #for each segment, do hist, then form a matrix ,each column is a hist for a interval
    flow.stat <- sapply(sequence_split(train, interval), flow_distribution, break_points=breaks)
    #Call prediction to give predicted PD vector
    tryCatch({prediction.hist <- hist_prediction(flow.stat)
         #Compared is the real distribution
         real <- flow_distribution(future[1:(1+interval)],breaks)
         if(judge_noisy(real)==judge_noisy(prediction.hist))
           a <- 0
         else if(judge_noisy(prediction.hist))
           a <- 1
         else
           a <- -1
         },error = function(err){
           a <- 2
         })
  }
  return(prediction.hist[2])
}

#给定rate interval, 给出时序预测的false positive/negative结果
noisy_time <- function(threshold, rate){
  a <- 0
  prediction.seri <- NA
  interval=20
  if(length(rate)>interval*10){
    rate <- tail(rate, n=-2)
    sample.num <- length(rate)
    train <- head(rate, n=(sample.num-interval))
    future <- tail(rate, n=(interval))
    #given the flow entry (time series), get hist for every 100s
    breaks <- c(0,threshold,60000000)
    #for each segment, do hist, then form a matrix ,each column is a hist for a interval
    flow.stat <- sapply(sequence_split(train, interval), flow_distribution, break_points=breaks)
    tryCatch({
      prediction.hist <- hist_prediction(flow.stat)
      series.prediction <- predict(auto.arima(train), n.ahead = interval)$pred
      prediction.seri <- flow_distribution(series.prediction,breaks)
      #Compared is the real distribution
      real <- flow_distribution(future[1:(1+interval)],breaks)
      judgement <- 0
      if(judge_noisy(real)==judge_noisy(prediction.seri))
        a <- 0
      else if(judge_noisy(prediction.seri))
        a <- 1
      else
        a <- -1
    },error = function(err){
      a <- 2
    })
  }
  return(prediction.seri[2])
}


#noisy.result <- ddply(flow_data,c("src_key","dst_key"), summarise,pdv=noisy_pdv(100000000,rate))

evaluate.noisy <- function(a, trace = flow_k){
  print(a)
  noisy.result <- ddply(trace,c("src_key","dst_key"),summarise, pdv=noisy_pdv(a,rate), series=noisy_time(a,rate))
  #noisy.result <- ddply(flow_k,c("src_key","dst_key"),summarise, pdv=noisy_pdv(a,rate), series=noisy_time(a,rate))
  pdv.positive <- sum(noisy.result$pdv == 1)
  pdv.negative <- sum(noisy.result$pdv == -1)
  series.positive <- sum(noisy.result$series == 1)
  series.negative <- sum(noisy.result$series == -1)
  err <- sum(noisy.result$series == 2)
  rbind(a/1000000, pdv.positive, pdv.positive, series.positive, series.negative, err)
}


#judge 一个pdv是不是noisy
judge_noisy <- function(pdv){
  (pdv[2]) > 0.5
}
#breaks <- c(0,75428904,144525535,304525535,5061188761)
interval <- 50
flow_k <- flow_data
flow_k$rate <- flow_k$rate/1000
noisy.all <- sapply(c(70000,100000,200000,300000),evaluate.noisy,trace=flow_k)


#--------------------- A more decent way of giving real/pdv/time result
#给定rate interval, 给出各种预测的大于threshold的概率结果
noisy_probability <- function(threshold, rate){
  a <- 0
  prediction.hist <- NA
  prediction.seri <- NA
  real <- NA
  interval=20
  if(length(rate)>interval*10){
    rate <- tail(rate, n=-2)
    sample.num <- length(rate)
    train <- head(rate, n=(as.integer(sample.num/interval)-1)*interval)
    future <- tail(rate, n=(interval))
    #given the flow entry (time series), get hist for every 100s
    breaks <- c(0,threshold,60000000)
    #for each segment, do hist, then form a matrix ,each column is a hist for a interval
    flow.stat <- sapply(sequence_split(train, interval), flow_distribution, break_points=breaks)
    tryCatch({
      prediction.hist <- hist_prediction(flow.stat)
      series.prediction <- predict(auto.arima(train), n.ahead = interval)$pred
      series.prediction[series.prediction <0 ] <- 0
      prediction.seri <- flow_distribution(series.prediction,breaks)
      #Compared is the real distribution
      real <- flow_distribution(future[1:(1+interval)],breaks)
    })
  }
  return(cbind(prediction.hist[2],prediction.seri[2],real[2]))
}


noisy.result <- ddply(flow_k,c("src_key","dst_key"),summarise, results=noisy_probability(10000,rate))








#---------------------------------------
#Try the same on <<Wild>> trace 用外面的trace来进行验证
read_csv <- function(x){
  print("reading")
  print(x)
  csv_data <- read.csv(file=paste("~/r/traces/univ1_csv/univ1_pt",as.character(x),".csv",sep=''),row.names=NULL,head=TRUE,sep=",")
  #Data formating
  csv_data$frame.time_relative = as.numeric(as.character(csv_data$frame.time_relative)) + 500*x
  csv_data$frame.len = as.integer(as.character(csv_data$frame.len)) 
  #Data cleaning
  csv_data <- csv_data[complete.cases(csv_data),]
  empty_entries <- which(csv_data$ip.src==""| csv_data$ip.dst=="")
  flow_trace <- csv_data[-empty_entries,]
  colnames(flow_trace) <- c("src_key","dst_key","byte","time_in_secs")
  flow_trace$time_in_secs <- as.integer(flow_trace$time_in_secs)
  flow_trace <- ddply(flow_trace,c("src_key","dst_key","time_in_secs"),summarise, rate=sum(byte))
  flow_trace <- flow_trace[order(flow_trace$time_in_secs),]
  return(flow_trace)
}

flow_all <- do.call(rbind,lapply(1:10,read_csv))

noisy.all <- sapply(c(7000,10000,100000),evaluate.noisy,trace=flow_all)







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

