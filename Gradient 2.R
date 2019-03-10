GradD <- function(x, y, alpha = 0.0005, epsilon = 10^-10){
  iter <- 0
  i <- 0
  x <- cbind(rep(1,nrow(x)), x)
  theta <- matrix(c(1,1),ncol(x),1)
  cost <- (1/(2*nrow(x))) * t(x %*% theta - y) %*% (x %*% theta - y)
  delta <- 1
  while(delta > epsilon){
    i <- i + 1
    theta <- theta - (alpha / nrow(x)) * (t(x) %*% (x %*% theta - y))
    cval <- (1/(2*nrow(x))) * t(x %*% theta - y) %*% (x %*% theta - y)
    cost <- append(cost, cval)
    delta <- abs(cost[i+1] - cost[i])
    if((cost[i+1] - cost[i]) > 0){
      print("The cost is increasing.  Try reducing alpha.")
      return()
    }
    iter <- append(iter, i)
  }
  print(sprintf("Completed in %i iterations.", i))
  return(theta)
}
x = c(13,14,24,35)
y = c(27,30,76,90)
x = as.matrix(x)
y = as.matrix(y)
theta <- GradD(x, y, alpha = 0.006, epsilon = 10^-10)
theta