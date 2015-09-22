library(scales) 
flow_num <- c(0,10,50,100,200,500,800,1000,2000,5000,8000,10000,20000,50000)

mse <- c(0.1135,0.0526,0.0201,0.0124,0.0106,0.0008,0.0006,0.0004,0.0002,0.00014,0.00012,0.0001,0.00004,0.00003)

log_num <- log10(flow_num)

test <- data.frame(flow_num,mse)

p <- ggplot(data=test, aes(flow_num,mse)) + xlab("Number of Ports (log)") 

#Draw graph for Packet Drops
p + geom_point(size=3, color="slateblue4") +
  #scale_x_continuous(trans=log10_trans()) +
  theme_solarized() +
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x)))

+
  geom_line(aes(y=time, color=type, linetype=churn )) +
  ylab("DHCP ACK time (ms)") +
  scale_y_continuous(labels = comma) +
  ggtitle("DHCP Response Time")

