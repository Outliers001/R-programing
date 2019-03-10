#Poisson Regression example
#-------------------------------------------------
#Prepared by: Md Pear Hossain
#Date of Preparation: 25 February, 2018
# Date of Modification: 25 February, 2018
#-----------------------------------------------
x=c(rep(-1,2),rep(0,4),rep(1,3))
y=c(2,3,6,7,8,9,10,12,15)
X=matrix(c(rep(1,9),rep(-1,2),rep(0,4),rep(1,3)),ncol = 2, byrow = F)
b_0=matrix(c(7,5), ncol = 1, byrow = F)

#1st iteration
library(Matrix)
W = Diagonal(n=length(x), x=1/X%*%b_0)
#J = t(X)%*%W%*%X
z=y
#R = t(X)%*%W%*%z
b_1 = solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z  #solve(J)%*%R
b_1

b_1=matrix(b_1, ncol = 1, byrow = F)

#2nd iteration
library(Matrix)
W = Diagonal(n=length(x), x=1/X%*%b_1)
#J = t(X)%*%W%*%X
z=y
#R = t(X)%*%W%*%z
b_2 = solve(t(X)%*%W%*%X)%*%t(X)%*%W%*%z
b_2

# R code for Poisson Regression
#-------------------------------
res.p = glm(y~x, family = poisson(link = "identity"))
summary(res.p)