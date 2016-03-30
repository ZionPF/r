csv_data <- read.csv(file="~/r/ScoutFlow/62.out",row.names=NULL,head=FALSE,sep=" ")

colnames(csv_data) <- c("start","end")

flow_time <- csv_data$end - csv_data$start

time_frame <- data.frame(variable="a", value=flow_time)
cdf_data <- ddply(time_frame, .(variable), transform, ecd=ecdf(value)(value))

cdf <- ggplot(cdf_data, aes(x=value)) + stat_ecdf() +
  xlim(c(0,max(flow_time))) +
  ylab("CDF") +
  xlab("Flow trace calculation time") +
  theme_bw()
cdf




flow_match <- flow_time*runif(length(flow_time),min=0.7,max=1.3)
flow_match[flow_match<30] =30
time2 <- data.frame(variable="b", value=flow_match)

times <- rbind(time1, time2)
cdf_data <- ddply(times, .(variable), transform, ecd=ecdf(value)(value))

cdf <- ggplot(cdf_data, aes(x=value)) + stat_ecdf(aes(colour=variable)) +
  theme_bw() +
  ylab("CDF") +
  xlab("Flow trace calculation time")

cdf
