### Homework 3
## Brett Scroggins

library(lpSolve)

########################################################################

## Problem 1
# Original Setup
c = c(-1,4)
A = matrix(c(-10,5,1,20,10,0),3,2)
dir = rep('<=',3)
b = c(22,49,5)

sol = lp("max",c,A,dir,b, compute.sens = TRUE)
sol$solution
sol$objval

# x1 >= 4
A2 = matrix(c(-10,5,1,-1,20,10,0,0),4,2)
dir2 = rep('<=',4)
b2 = c(22,49,5,-4)

sol2 = lp("max",c,A2,dir2,b2, compute.sens = TRUE)
sol2$solution
sol2$objval

# x1 >= 4, x2 >= 3
A3 = matrix(c(-10,5,1,-1,0,20,10,0,0,-1),5,2)
dir3 = rep('<=',5)
b3 = c(22,49,5,-4, -3)

sol3 = lp("max",c,A3,dir3,b3, compute.sens = TRUE)
sol3$status

# x1 >= 4, x2 <= 2
A4 = matrix(c(-10,5,1,-1,0,20,10,0,0,1),5,2)
dir4 = rep('<=',5)
b4 = c(22,49,5,-4, 2)

sol4 = lp("max",c,A4,dir4,b4, compute.sens = TRUE)
sol4$status
sol4$solution
sol4$objval


# x1 <= 3
A5 = matrix(c(-10,5,1,1,20,10,0,0),4,2)
dir5 = rep('<=',4)
b5 = c(22,49,5,3)

sol5 = lp("max",c,A5,dir5,b5, compute.sens = TRUE)
sol5$solution
sol5$objval

# x1 <= 3, x2 >= 3
A6 = matrix(c(-10,5,1,1,0,20,10,0,0,-1),5,2)
dir6 = rep('<=',5)
b6 = c(22,49,5,3,-3)

sol6 = lp("max",c,A6,dir6,b6, compute.sens = TRUE)
sol6$status

# x1 <= 3, x2 <= 2
A7 = matrix(c(-10,5,1,1,0,20,10,0,0,1),5,2)
dir7 = rep('<=',5)
b7 = c(22,49,5,3,2)

sol7 = lp("max",c,A7,dir7,b7, compute.sens = TRUE)
sol7$solution
sol7$objval

# x1 <= 3, x2 <= 2, x1 <= 1
A8 = matrix(c(-10,5,1,1,0,1,20,10,0,0,1,0),6,2)
dir8 = rep('<=',6)
b8 = c(22,49,5,3,2,1)

sol8 = lp("max",c,A8,dir8,b8, compute.sens = TRUE)
sol8$solution
sol8$objval

# x1 <= 3, x2 <= 2, x1 <= 1
A9 = matrix(c(-10,5,1,1,0,-1,20,10,0,0,1,0),6,2)
dir9 = rep('<=',6)
b9 = c(22,49,5,3,2,-2)

sol9 = lp("max",c,A9,dir9,b9, compute.sens = TRUE)
sol9$solution
sol9$objval

# Check with int.vec
c = c(-1,4)
A = matrix(c(-10,5,1,20,10,0),3,2)
dir = rep('<=',3)
b = c(22,49,5)

sol = lp("max",c,A,dir,b, int.vec = 1:2)
sol$solution
sol$objval

########################################################################

## Problem 2
c = c(9,5,6,4)
A = matrix(0,8,4)
A[1,] = c(6,3,5,2)
A[2,] = c(rep(1,4))
A[3:6,] = diag(1,4)
A[7,] = c(1,1,0,0)
A[8,] = c(0,0,1,1)
dir = c(rep('<=',6),rep('>=',2))
b = c(11,4,1,1,1,1,1,1)

sol = lp("max",c,A,dir,b, int.vec = 1:4)
sol$solution
sol$objval

########################################################################

## Problem 3
c = rep(1,12)
b = rep(1,12)
dir = rep('>=',12)

A = matrix(0,12,12)
A[1,] = c(1,0,1,0,1,0,1,1,1,0,0,0)
A[2,] = c(0,1,0,0,0,0,0,1,1,0,0,0)
A[3,] = c(1,0,1,0,0,0,1,1,1,0,0,0)
A[4,] = c(0,0,0,1,0,0,0,0,0,1,0,0)
A[5,] = c(1,0,0,0,1,0,1,0,0,0,0,0)
A[6,] = c(0,0,0,0,0,1,0,0,0,1,1,0)
A[7,] = c(1,0,1,0,1,0,1,0,0,0,0,0)
A[8,] = c(1,1,1,0,0,0,0,1,1,0,0,0)
A[9,] = c(1,1,1,0,0,0,0,1,1,0,0,0)
A[10,]= c(0,0,0,1,0,1,0,0,0,1,1,1)
A[11,]= c(0,0,0,0,0,1,0,0,0,1,1,1)
A[12,]= c(0,0,0,0,0,0,0,0,0,1,1,1)

sol = lp("min",c,A,dir,b, int.vec = 1:12)
sol$solution
sol$objval
 
########################################################################

## Problem 4
c = c(20,9,12,4,16,21,8)
b = c(233,148,106)
dir = c(rep('>=',3))

A = matrix(0,3,7)
A[1,] = c(4,0,0,1,2,1,3)
A[2,] = c(0,3,0,1,0,2,1)
A[3,] = c(0,0,3,1,1,0,0)

sol = lp("min",c,A,dir,b, int.vec = 1:16)
sol$solution
sol$objval

########################################################################

## Problem 5
c = c(330,300,330,rep(360,4))
b = c(5,13,12,10,14,8,6)
dir = c(rep('>=',7))

A = matrix(1,7,7)
A[1,2:3] = 0
A[2,3:4] = 0
A[3,4:5] = 0
A[4,5:6] = 0
A[5,6:7] = 0
A[6,] = c(0,1,1,1,1,1,0)
A[7,1:2] = 0

sol = lp("min",c,A,dir,b, int.vec = 1:7)
sol$solution
sol$objval
