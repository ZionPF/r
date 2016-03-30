
# Now we evaluate the overhead before and after matrix decomposition
#_________________________________________________________________

#Basic 3-level Tree
n <- c(100,1000,10000,100000)


#c : clos
#t : basic tree
#f : fat tree
#b : bcube 




c1 <- c(0.063, 0.195, 5.073, 8.732)
t1 <- c(0.031, 0.154, 1.484, 9.47)
f1 <- c(0.0)

a <- matrix(sample(15,100*100,T),100)

ptm <- proc.time()
a <- qr(matrix(sample(15,10000*10000,T),10000))$rank
proc.time() - ptm