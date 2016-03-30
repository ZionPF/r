library(reshape)
library(ggplot2)
library(plyr)
library(reshape2)
library(scales) 

#Extra when flow not full mesh
n <- c(4,6,8,10,12,14,18,24,30,36,42,48,64,80,96)
x1 <- 0.2
x2 <- 0.5
x3 <- 0.8

e1 <- n*(n-1)/2*x1^2*(1-x1)^(n-2) + n*x1*(1-x1)^(n-1) + (1-x1)^n
e2 <- n*(n-1)/2*x2^2*(1-x2)^(n-2) + n*x2*(1-x2)^(n-1) + (1-x2)^n
e3 <- n*(n-1)/2*x3^2*(1-x3)^(n-2) + n*x3*(1-x3)^(n-1) + (1-x3)^n


plot_exp <- function(df){
  
  exp_df <- melt(df,id.vars = "node")
  colnames(exp_df)[2] <- "Methods"
  exp_df$Methods <- factor(exp_df$Methods, levels=c("l", "m", "m1", "m2"), labels=c("Total Link", "ScoutFlow (Ideal)", "ScoutFlow (20% load)", "ScoutFlow (50% load)"))
  p <- ggplot(data=exp_df, aes(x = node, y=value, shape=Methods, color=Methods,linetype=Methods)) +
    geom_line() +  geom_point(size=3) +
    
    scale_y_continuous(trans = log10_trans(),
                       breaks = trans_breaks("log10", function(x) 10^x),
                       labels = trans_format("log10", math_format(10^.x))) +
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_fill_discrete(name="Measurement Methods",
                        breaks=c("link", "mirror"),
                        labels=c("All Link", "ScoutFLow")) +
    ylab("Number of Link Monitored") +
    xlab("Number of Servers")
  p + theme_bw() +
    theme(legend.justification=c(0,1), legend.position=c(0,1))
}




#Basic 3-level Tree
n <- c(4,6,8,10,12,14,18,24,30,36,42,48,64,80,96)
N <- (n-1)^3
l <- n*(n-1)^2 * runif(length(n),min=0.95,max=1)
#f <- N^2/2
m <- 5+0.2*n*runif(length(n),min=0.85,max=1)
m1 <- m + e1*n
m2 <- m + e2*n
m3 <- m + e3*n
df <- data.frame(node = N, l=l, m=m, m1=m1, m2=m2)
plot_exp(df)


# Fat Tree

N <- n^3/4
l <- N*log(N/2,base=n/2) * runif(length(n),min=0.95,max=1)
m <- (n/4)^2/2*runif(length(n),min=0.85,max=1) + 4
m1 <- m + e1*n^2/4 + e1*n/2
m2 <- m + e2*n^2/4 + e1*n/2
m3 <- m + e3*n^2/4 + e1*n/2
df <- data.frame(node = N, l=l, m=m, m1=m1, m2=m2)
plot_exp(df)


# Clos Network

N <- (n^2/4)^2
l <- (1/2+1)*N* runif(length(n),min=0.95,max=1)
m <- (n/2) *runif(length(n),min=0.85,max=1) + 3
m1 <- m + e1*n/2 +1
m2 <- m + e2*n/2 +1
m3 <- m + e3*n/2 +1
df <- data.frame(node = N, l=l, m=m, m1=m1, m2=m2)
plot_exp(df)

# BCube

N <- n^2
l <- N*log(N,base=n)
m<-1
m1 <- m + 2*e1*n
m2 <- m + 2*e2*n
m3 <- m + 2*e3*n
df <- data.frame(node = N, l=l, m=m, m1=m1, m2=m2)
plot_exp(df)




# Now we evaluate the overhead before and after matrix decomposition
#_________________________________________________________________

#Basic 3-level Tree
n <- c(100,1000,1000,10000,100000)

#t : basic tree
#f : fat tree
#c : clos
#b : bcube



N <- (n-1)^3
l <- n*(n-1)^2 * runif(length(n),min=0.95,max=1)
#f <- N^2/2
m <- 5+0.2*n*runif(length(n),min=0.85,max=1)
m1 <- 2*n^2
m2 <- m + e2*n
m3 <- m + e3*n
df <- data.frame(node = N, l=l, m=m, m1=m1, m2=m2)
plot_exp(df)






