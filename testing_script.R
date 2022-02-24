library(hash)

test_reagents <- data.frame(A=matrix(0,96,1))

empty_plate <- matrix(0,8,12)

test_reagents$A <- rep(0,96)
test_reagents$B <- rep(0,96)
test_reagents$C <- rep(0,96)
test_reagents$A[1:9] <- c(1:9)
test_reagents$B[25:36] <- c(1:9)

names(test_reagents) <- c("ReagentA","ReagentB","ReagentC")



x=c(10)
y=c(0,NA )
z <- c(x,y)
z <- t(z)

outfile <- "test.txt"
 write.table(z,file=outfile,col.names=FALSE,row.names=FALSE,na="")
