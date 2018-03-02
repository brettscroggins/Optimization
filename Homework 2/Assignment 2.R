library(lpSolve)

################################################

# 1A
c1 = c(4,5)
A1 = matrix(c(2,3),1,2)
b1 = 60
dir1 = '<='

sol1 = lp('max',c1,A1,dir1,b1)

sol1$status
sol1
sol1$solution

# 1B
A1b = matrix(c(2,1,3,-1),2,2)
b1b = c(60,0)
dir1b = rep('<=',2)

sol1b = lp('max',c1,A1b,dir1b,b1b)

sol1b$status
sol1b
sol1b$solution

################################################

# 2

# 2b
c2b = c(2000,3000)
A2b = matrix(c(3,2,1,2,4,1),3,2)
b2b = c(1000,1200,450)
dir2b = rep('<=',3)

sol2b = lp('max',c2b,A2b,dir2b,b2b)

sol2b
sol2b$solution

# 2c
fert = 200
sol_matrix = matrix(0,21,3)
i = 1

while(fert <= 2200){
  b2c = c(1000,fert,450)
  sol2c = lp('max',c2b,A2b,dir2b,b2c)
  sol_matrix[i,1] = sol2c$solution[1]
  sol_matrix[i,2] = sol2c$solution[2]
  sol_matrix[i,3] = fert
  fert = fert + 100
  i = i + 1
}

sol_matrix

################################################

# 3

c3 = rep(c(13,16,16,14,39),2)
A3 = matrix(0,14,10)

A3[1,1:5] = 1
A3[2,6:10] = 1
A3[3,1:5] = c(11,53,5,5,29)
A3[4,6:10] = c(3,6,5,1,34)
A3[5:9,1:5] = (diag(5)*40)
A3[10:14,6:10] = (diag(5)*20)
A3

b3 = c(1,1,40,20,11,53,5,5,29,3,6,5,1,34)
dir3 = rep('<=',14)

sol3 = lp('max',c3,A3,dir3,b3)

sol3
sol3$solution

time0 = sol3$solution[1:5]*40
time0
time1 = sol3$solution[6:10]*20
time1

################################################

# 4

c4 = c(.18,.23,.05)
A4 = matrix(0,7,3)
A4

A4[1,1:3] = c(107,500,0)
A4[2,1:3] = A4[1,1:3]*-1

A4[3,1:3] = c(72,121,65)
A4[4,1:3] = A4[3,1:3]*-1

A4[5:7,1:3] = diag(3)
A4

b4 = c(50000,-5000,2250,-2000,10,10,10)
dir4 = rep('<=',7)

sol4 = lp('min',c4,A4,dir4,b4)

sol4
sol4$solution

################################################

# 5

c5 = rep(1,6)
A5 = matrix(0,12,6)

A5[1,1:6] = c(1,0,0,1,0,0)
A5[2,1:6] = A5[1,1:6]*-1
A5[3,1:6] = c(0,1,0,0,1,0)
A5[4,1:6] = A5[3,1:6]*-1
A5[5,1:6] = c(0,0,1,0,0,1)
A5[6,1:6] = A5[5,1:6]*-1
A5[7:12,1:6] = diag(6)
A5

b5 = c(2,-1.2,2,-1.5,3,-2,1,1.3,1.4,1,1.2,1.6)
b5

dir5 = rep('<=',12)

sol5 = lp('max',c5,A5,dir5,b5)

sol5
sol5$solution
