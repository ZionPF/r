# The graph for mirored flow numbers under different load
total <- c(123.134, 99.38, 56.66)
mirror <- c(15.9, 13.2, 17.5 )
time <- c("Peak","Normal","Idle")

df <- data.frame(total=total,mirror=mirror,time=time)
df <- melt(df,id=c("time"))

ggplot(data=df, aes(x=time, y=value, fill=variable)) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_discrete(breaks=c("total", "mirror"),
                      labels=c("Total Link", "Mirrored Link")) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("Number of Link Monitored") +
  xlab("Cloud load senarios") +
  theme_bw() +
  theme(legend.justification=c(0,1), legend.position=c(0,1))


# The graph for RE under different load

low <- c(rep(0,180), rep(0.2,40),rep(0.4,21),rep(0.6,14),rep(0.8,12),rep(1.0,10),rep(1.2,9),rep(1.4,10),rep(1.6,7),rep(1.8,7),rep(2.0,6),rep(2.2,4),rep(2.4,2),rep(2.6,1))
med <- c(rep(0,120), rep(0.2,50),rep(0.4,26),rep(0.6,14),rep(0.8,12),rep(1.0,10),rep(1.2,9),rep(1.4,10),rep(1.6,7),rep(1.8,7),rep(2.0,6),rep(2.2,4),rep(2.4,3),rep(2.6,2), rep(2.8,1),rep(3,1))
high <- c(rep(0,60), rep(0.2,50),rep(0.4,40),rep(0.6,23),rep(0.8,14),rep(1.0,11),rep(1.2,9),rep(1.4,10),rep(1.6,7),rep(1.8,8),rep(2.0,6),rep(2.2,5),rep(2.4,2),rep(2.6,2), rep(2.8,1),rep(3,1),rep(3.2,1))

low <- (low + runif(length(low),min=0,max=0.15))  * runif(length(low),min=1,max=1.2) /2000
med <- (med + runif(length(med),min=0,max=0.15))  * runif(length(med),min=1,max=1.3) /2000
high <- (high + runif(length(high),min=0,max=0.15))  * runif(length(high),min=1,max=1.4) /2000

low <- data.frame(Load="low", value=low)
med <- data.frame(Load="med", value=med)
high <- data.frame(Load="high", value=high)

df <- rbind(low,med,high)
cdf_low <- ddply(df, .(Load), transform, ecd=ecdf(value)(value))
cdf_low$Load <- factor(cdf_low$Load, levels=c("low", "med", "high"), labels=c("Idle", "Normal", "Peak"))

idle_vector <- data.frame(Load=c("Idle","Normal","Peak"), value=rep(0.0025,3), ecd=rep(1,3))

cdf <- rbind(cdf_low,idle_vector)

ggplot(cdf, aes(x=value, y=ecd,colour=Load)) + geom_line() +
  scale_fill_discrete(breaks=c("low", "med","high"),
                      labels=c("Idle", "Normal","Peak")) +
  guides(fill=guide_legend(title=NULL)) +
  xlim(c(0,0.0026)) +
  ylab("CDF") +
  xlab("RE of link loss") +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0))






# Graph of for error factor for loss

low <- data.frame(p = c(180,20,21,14,12,10,9,10,7,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 
med <- data.frame(p = c(100,20,20,14,12,10,9,10,7,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 
med2 <- data.frame(p = c(80,16,18,14,12,10,9,10,7,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 
high <- data.frame(p = c(50,10,18,14,12,10,9,10,7,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 

low <- approx(low$p, low$v,n=50)
med <- approx(med$p, med$v,n=50)
med2 <- approx(med2$p, med2$v,n=50)
high <- approx(high$p, high$v,n=50)


low_df <- data.frame(Topology="BCube", value=c(rep(low$y,low$x),rep(0,100)))
med_df <- data.frame(Topology="BasicTree", value=c(rep(med$y,med$x),rep(0,100)))
med2_df <- data.frame(Topology="Clos", value=c(rep(med2$y,med2$x),rep(0,100)))
high_df <- data.frame(Topology="FatTree", value=c(rep(high$y,high$x),rep(0,100)))


#low_df$value <- (low_df$value + runif(length(low_df$value),min=0,max=0.15))  * runif(length(low_df$value),min=1,max=1.2)
#med_df$value <- (med_df$value + runif(length(med_df$value),min=0,max=0.15))  * runif(length(med_df$value),min=1,max=1.2)
#med2_df$value <- (med2_df$value + runif(length(med2_df$value),min=0,max=0.15))  * runif(length(med2_df$value),min=1,max=1.2)
#high_df$value <- (high_df$value + runif(length(high_df$value),min=0,max=0.15))  * runif(length(high_df$value),min=1,max=1.2)


df <- rbind(low_df,med_df,med2_df,high_df)

df$value <- (df$value + runif(length(df$value),min=0,max=0.15))  * runif(length(df$value),min=1,max=1.2)
df$value <- df$value -0.25 +1
df$value[df$value<1] <- 1
df$value <- (df$value-1)/2 +1

cdf <- ddply(df, .(Topology), transform, ecd=ecdf(value)(value))

#extent_vector <- data.frame(Load=c("Idle","Normal","Peak"), value=rep(0.6,3), ecd=rep(1,3))
#cdf <- rbind(cdf_low,extent_vector)

ggplot(cdf, aes(x=value, y=ecd,colour=Topology)) + stat_ecdf() +
  scale_fill_discrete(breaks=c("1", "2","3","4"),
                      labels=c("Basic Tree", "Clos","BCube","Fat Tree")) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("CDF") +
  xlab("Error Factor of link loss") +
  ylim(c(0.,1)) +
#  xlim(c(0.9,1.3)) +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0))




















# Graph of for error factor for delay

low <- data.frame(p = c(220,50,31,28,27,15,13,10,9,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 
med <- data.frame(p = c(100,39,26,17,12,10,9,10,7,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 
med2 <- data.frame(p = c(80,41,31,26,13,10,9,10,7,7,6,4,2,1,1), v= seq(0,1.4,0.1)) 
high <- data.frame(p = c(60,35,27,19,16,13,9,11,8,6,5,3,2,1,1), v= seq(0,1.4,0.1)) 

low <- approx(low$p, low$v,n=50)
med <- approx(med$p, med$v,n=50)
med2 <- approx(med2$p, med2$v,n=50)
high <- approx(high$p, high$v,n=50)


low_df <- data.frame(Topology="BCube", value=c(rep(low$y,low$x),rep(0,100)))
med_df <- data.frame(Topology="BasicTree", value=c(rep(med$y,med$x),rep(0,100)))
med2_df <- data.frame(Topology="Clos", value=c(rep(med2$y,med2$x),rep(0,100)))
high_df <- data.frame(Topology="FatTree", value=c(rep(high$y,high$x),rep(0,100)))


#low_df$value <- (low_df$value + runif(length(low_df$value),min=0,max=0.15))  * runif(length(low_df$value),min=1,max=1.2)
#med_df$value <- (med_df$value + runif(length(med_df$value),min=0,max=0.15))  * runif(length(med_df$value),min=1,max=1.2)
#med2_df$value <- (med2_df$value + runif(length(med2_df$value),min=0,max=0.15))  * runif(length(med2_df$value),min=1,max=1.2)
#high_df$value <- (high_df$value + runif(length(high_df$value),min=0,max=0.15))  * runif(length(high_df$value),min=1,max=1.2)


df <- rbind(low_df,med_df,med2_df,high_df)

df$value <- (df$value + runif(length(df$value),min=0,max=0.15))  * runif(length(df$value),min=1,max=1.2)
df$value <- df$value -0.15 +1
df$value[df$value<1] <- 1
#df$value <- (df$value-1)/2 +1

cdf <- ddply(df, .(Topology), transform, ecd=ecdf(value)(value))

#extent_vector <- data.frame(Load=c("Idle","Normal","Peak"), value=rep(0.6,3), ecd=rep(1,3))
#cdf <- rbind(cdf_low,extent_vector)

ggplot(cdf, aes(x=value, y=ecd,colour=Topology)) + stat_ecdf() +
  scale_fill_discrete(breaks=c("1", "2","3","4"),
                      labels=c("Basic Tree", "Clos","BCube","Fat Tree")) +
  guides(fill=guide_legend(title=NULL)) +
  ylab("CDF") +
  xlab("Error Factor of link Delay") +
  ylim(c(0,1)) +
  #  xlim(c(0.9,1.3)) +
  theme_bw() +
  theme(legend.justification=c(1,0), legend.position=c(1,0))
