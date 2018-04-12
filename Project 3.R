# Project 3

library(slam)
library(gurobi)
library(glmnet)

setwd('/Users/brettscroggins/Downloads/')

load("data.Rdata")

# Question 1

## MIQP with Gurobi
p = ncol(X)
k=8
M = .25
sol = list() 
sol$x = c(M)

while (M %in% sol$x) {
  M = M*2
  Q = matrix(0, ncol=2*p, nrow=2*p)
  Q[1:p,1:p] = t(X) %*% X
  c = -2 * c(t(y) %*% X, rep(0,p))
  
  A = matrix(0, nrow=2*p+1, ncol=2*p)
  #all zi sum to k
  A[1,] = c(rep(0,p), rep(1,p))
  
  #all -Mzi<=Bi
  A[2:(p+1),1:p] = diag(-1,p)
  A[2:(p+1),(p+1):ncol(A)] = diag(-M,p)
  
  A[(p+2):(2*p+1),1:p] = diag(-1,p)
  A[(p+2):(2*p+1),(p+1):ncol(A)] = diag(M,p)
  
  dir = c(rep("<=", (p+1)), rep(">=", p))
  b = c(k, rep(0,2*p))
  
  # define model
  model = list()
  model$A = A
  model$obj = c
  model$sense = dir
  model$rhs = b
  model$vtype = c(rep("C", p), rep("B", p))
  model$Q = Q
  
  sol = gurobi(model) 

}
pred_gurobi = X %*% sol$x[1:p]

cat("Gurobi prediction RMSE:", sqrt(mean((y-pred_gurobi)^2))) ## Gurobi prediction RMSE: 1.107163

# Question 2

## Lasso with Glmnet

glmmod = glmnet(X, y=y, alpha=1)

plot(glmmod$lambda, glmmod$dev.ratio, ylab="% variance explained",
     xlab="Lambda penalty", main="Model Accuracy versus Penalty")

glmmod
### Here, we can see each of the 72 different models have a different 
### number of non-zero coefficients (first column), produced by different 
### lambda values.

cat("Selected model: ", which.min(glmmod$lambda[glmmod$df<=8])) 
### We selected the 23rd model, as it has 8 non-zero coefficients and has 
### the smallest lambda value of all models with 8 non-zero coefficients (matching our k from MIQP).

pred_ridge = predict(glmmod, X)[,23]
cat("Lasso prediction RMSE:", sqrt(mean((y-pred_ridge)^2)))
### Lasso prediction RMSE: 1.195113
### It also produces the smallest RMSE of all models with 8 non-zero coefficients.


# Question 3

## Error

norm_vec_sq <- function(x) {return(sum(x^2))}
error_MIQP = norm_vec_sq(X %*% sol$x[1:p] - X %*% beta_real) / norm_vec_sq(X %*% beta_real)
cat("Error from MIQP: ", error_MIQP)
### Error from MIQP:  0.004456055

error_lasso = norm_vec_sq(X %*% glmmod$beta[,23] - X %*% beta_real) / norm_vec_sq(X %*% beta_real)
cat("Error from Lasso: ", error_lasso)
### Error from Lasso:  0.02446623
### MIQP has a smaller prediction error than the Lasso model we selected.