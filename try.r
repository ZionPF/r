Examples:

     m <- cbind(1, 1:7) # the '1' (= shorter vector) is recycled
     m
     m <- cbind(m, 8:14)[, c(1, 3, 2)] # insert a column
     m
     cbind(1:7, diag(3)) # vector is subset -> warning
     
     cbind(0, rbind(1, 1:3))
     cbind(I = 0, X = rbind(a = 1, b = 1:3))  # use some names
     xx <- data.frame(I = rep(0,2))
     cbind(xx, X = rbind(a = 1, b = 1:3))   # named differently
     
     cbind(0, matrix(1, nrow = 0, ncol = 4)) #> Warning (making sense)
     dim(cbind(0, matrix(1, nrow = 2, ncol = 0))) #-> 2 x 1
     
     ## deparse.level
     dd <- 10
     rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 0) # middle 2 rownames
     rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 1) # 3 rownames (default)
     rbind(1:4, c = 2, "a++" = 10, dd, deparse.level = 2) # 4 rownames
