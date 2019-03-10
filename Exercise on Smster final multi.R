n <- 4
p <- 2
mu <- c(7,11)
X <- matrix(c(2,8,6,8,12,9,9,10),nrow=n,ncol=p)
C <- matrix(c(1,1,-1,1),nrow=2,ncol=2)
Z <- t(C%*%t(X))
zbar <- colMeans(Z)
muz <- C%*%mu
Sz <- cov(Z)
Szinv <- solve(Sz)
s = matrix(c(8,-3.333333,-3.333333,2),nrow = 2)

sp = solve(C%*%s%*%t(C))
Tstar <- n*t(zbar-muz) %*% sp %*% (zbar-muz)

