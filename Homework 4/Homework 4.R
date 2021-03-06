# Homework 4
## Brett Scroggins

###############################################################

setwd('/Users/brettscroggins/Downloads')
library(quadprog)

###############################################################

# Problem 1

machines = function(K){

  L = (100000-15*K)/12
  machines = 0.05*L^(2/3)*K^(1/3)
  return(-machines)
  
}

optim(200, machines, method='BFGS')

###############################################################

# Problem 2

stocks = read.csv('homework4stocks.csv')

# Get info from the stocks data set
num = ncol(stocks)
mean = colMeans(stocks[,2:num])
std = sqrt(apply(stocks[,2:num],2,var))

corr = cor(stocks[,2:num], use = 'pairwise.complete.obs')
cov = cov(stocks[,2:28], use='pairwise.complete.obs')


# Create the A, b, d, and D components
A_mat = matrix(c(rep(1,27), diag(27), mean),27)
b_vec = c(1,rep(0,27),.01)
d_vec = rep(0,27)
D_mat = 2*cov

# Solution
sol2 = solve.QP(D_mat, d_vec, A_mat, b_vec)
sol2
perc_return = sum(sol2$solution*mean)
perc_return
variance = sum(sol2$solution*(std^2))
variance
stnddev = sum(sol2$solution*std)
stnddev

###############################################################

# Problem 3

vars = read.csv('variable_selection.csv')

lm1 = lm(vars$y ~ vars$x1)
lm2 = lm(vars$y ~ vars$x2)
lm3 = lm(vars$y ~ vars$x3)
lm4 = lm(vars$y ~ vars$x1 + vars$x2)
lm5 = lm(vars$y ~ vars$x1 + vars$x3)
lm6 = lm(vars$y ~ vars$x2 + vars$x3)

r1 = sum(resid(lm1)^2)
r1
r2 = sum(resid(lm2)^2)
r2
r3 = sum(resid(lm3)^2)
r3
r4 = sum(resid(lm4)^2)
r4
# r4 is lowest with 26.19 sum of square residuals
r5 = sum(resid(lm5)^2)
r5
r6 = sum(resid(lm6)^2)
r6

###############################################################

# Problem 4

D_mat = matrix(0,5,5)
D_mat[1,1] = 1*2
D_mat[2,2] = 3*2
D_mat[3,3] = 4*2
D_mat[4,4] = 6*2
D_mat[5,5] = 12*2
d_vec = rep(0,5)
A_mat = matrix(c(1,0,1,0,0,-1,0,0,1,1,0,1,-1,-1,0),5,3)
b_vec = c(710,0,0)

solve.QP(D_mat,d_vec,A_mat,b_vec)

###############################################################

# Problem 5

nfl_ratings = read.csv('nflratings.csv', header = FALSE)

nfl_ratings$spread = nfl_ratings$V4 - nfl_ratings$V5

error = function(scores){
  
  num = nrow(nfl_ratings)
  pred_spread = 0
  
  for(i in 1:num){
    pred_spread = pred_spread + (nfl_ratings$spread[i] - (scores[nfl_ratings[i,2]] - scores[nfl_ratings[i,3]] + scores[33]))^2
  }
  return(pred_spread)
}

start = rep(0,33)
sol5 = optim(start, error, method='BFGS')
normalized = sol5$par[1:32] + (85 - mean(sol5$par[1:32]))
normalized = c(normalized, sol5$par[33])
normalized
