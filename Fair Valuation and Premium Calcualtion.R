#####################################
### Question 1
#####################################

## Simulate Sample Paths ##
rm(list=ls())
set.seed(1)

## define model parameters
mu0 <- 0.0015
theta_mu <- 0
k_mu <- -0.08
beta_mu <- 0.008
C = 100000
interest = 0.01



## simulate short rate paths
n <- 1000    # MC simulation trials
T <- 20    # total time
m <- 2000   # subintervals
dt <- T/m  # difference in time each subinterval

# Brownian Motions
rho=-0.5
  
e1 = matrix(rnorm(m*n,mean=0,sd=1),m,n)
e2 = matrix(rnorm(m*n,mean=0,sd=1),m,n)

z1 = e1 # rates
z2 = rho*e1 + sqrt(1-rho^2)*e2 # Fund F


mu <- matrix(0,m+1,n)  # matrix to hold short rate paths
mu[1,] <- mu0

for(j in 1:n){
  for(i in 2:(m+1)){
    dmu <- k_mu*(theta_mu-mu[i-1,j])*dt + beta_mu*sqrt(dt)*z1[i,j]
    mu[i,j] <- abs(mu[i-1,j] + dmu)
  }
} 

### calculate premium
premium_matrix <- matrix(nrow=1,ncol=n)
for(i in 1:n){
  premium_matrix[1,i] <- C*exp(-sum(mu[,i]*dt))*(1/(1+interest)^T)
}

mean(premium_matrix)

## define model parameters
r0 <- 0.01
theta_r <- 0.02
k_r <- 0.2
beta_r <- 0.012

r <- matrix(0,m+1,n)  # matrix to hold short rate paths
r[1,] <- r0

for(j in 1:n){
  for(i in 2:(m+1)){
    dr <- k_r*(theta_r-r[i-1,j])*dt + beta_r*sqrt(dt)*z1[i,j]
    r[i,j] <- r[i-1,j] + dr
  }
} 

### calculate fair value
FV_matrix <- matrix(nrow=1,ncol=n)
for(i in 1:n){
  FV_matrix[1,i] <- C*exp(-sum(mu[,i]*dt))*exp(-sum(r[,i]*dt))
}

mean(FV_matrix)


#-----------------
# QUESTION 2
#-----------------

#a) Premium

## define model parameters
interest = 0.01
T <- 20
C <- 100000

### calculate premium
premium <- C*(1/(1+interest))^T

#b) Fair Value  

F0 = 1
# returns
F_matrix = matrix(0,m+1,n)
F_matrix[1,] <- 1

# fund value
F_endvalue = matrix(0,m+1,n)
F_endvalue[1,] <- premium

dF_matrix = matrix(0,m+1,n)

a = 0.04
b = 0.05


for(j in 1:n){
  for(i in 2:(m+1)){
    dF_matrix <- a*F_matrix[i-1,j]*dt + b*sqrt(dt)*z2[i,j]
    F_matrix[i,j] <- F_matrix[i-1,j] + dF_matrix
    F_endvalue[i,j] <- F_endvalue[1,j]*F_matrix[i,j]
  }
} 

# Relative Bonus Returns
F_bonus <- matrix(0,1,n)

for(j in 1:n)
  {
    F_bonus[1,j] = max(0,1-(F_matrix[2001,j]-1.01^T))
  }

### calculate fair value
FV_guar_matrix <- matrix(nrow=1,ncol=n)
FV_bonus_matrix <- matrix(nrow=1,ncol=n)

for(i in 1:n){
  FV_guar_matrix[1,i] <- C*exp(-sum(r[,i]*dt))*(1+interest)^T
  FV_bonus_matrix[1,i] <- C*exp(-sum(r[,i]*dt))*F_bonus[1,i]
}

mean(FV_guar_matrix)
mean(FV_bonus_matrix)





# Absolute Bonus
#F_bonus <- matrix(0,1,n)

#for(j in 1:n)
 # {
  #  F_bonus[1,j] = max(0,0.85*(F_endvalue[2001,j]-100000))
  #}

#mean(F_bonus)




# En voor 5 en 6 heb ik 48318 en 65961 voor duration van 20yr

