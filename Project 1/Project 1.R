# Project 1 Work

################################################

P = c(102,99,101,98,98,104,100,101,102,94)
C = c(5,3.5,5,3.5,4,9,6,8,9,7)
M = c(1,2,2,3,4,5,5,6,7,8)
L = c(12000,18000,20000,20000,16000,15000,12000,10000)


dedicate_g3 <- function(P,C,M,L){
  
  library(lpSolve)

  # Initialize obj, b, dir and A values
  obj = P
  A = matrix(0,length(L), length(C))
  dir = rep('=', length(L))
  b = L
  
  # Fill in matrix A with appropriate coupon payments
  for (i in seq_along(C)){
    A[1:M[i]-1,i] = C[i]
    
    # Matrix with maturity and final coupon payments
    A[M[i],i] = 100 + C[i]
  }
  
  # Return the optimal solution
  sol = lp("min",obj,A,dir,b, compute.sens = TRUE)
  return(sol)
}


# Call the function and output the optimal solution
sol_3 = dedicate_g3(P,C,M,L)

cat('\n Objective Value:',sol_3$objval)

for (i in seq(1,length(sol_3$solution))){
  cat(paste('Bond ',i,' amt',':',sep=''), sol_3$solution[i],'\n')
}

########################################################################

# Question 4
Bonds = read.csv('/Users/brettscroggins/Desktop/Bonds.csv')

Liabilities = read.csv('/Users/brettscroggins/Desktop/Liabilities.csv')

dates = Liabilities$Date

Bonds = Bonds[Bonds$Maturity %in% dates,]

# Initialize variables
P = Bonds$Asked
C = Bonds$Coupon/2
L = Liabilities$Liability

# To initialize M appropriately
periods = seq_along(dates)
names(periods) = dates
M = rep(0,length(Bonds$Maturity))

#Add period number corresponding to each maturity date to maturities vector
for (i in seq_along(M)){
  date = toString(Bonds$Maturity[i])
  period = periods[date]
  
  M[i] = period
}

# Call the function to solve and display the output
sol_4 = dedicate_g3(P, C, M, L)

cat('\n Objective Value:',sol_4$objval)

for (i in seq(1,length(sol_4$solution))){
  cat(paste('Bond ',i,' amt',':',sep=''), sol_4$solution[i],'\n')
}

plot(seq(1,length(L)), sol_4$duals[1:12], xlab = 'Time Period', ylab = 'Duals',
     title(main = 'Time Period vs. Duals'))
