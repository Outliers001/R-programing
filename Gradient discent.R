x = c(1,2,3,7)
y = c(2,10,5,13)
# Define the squared error cost function	
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
   }
alpha <- 0.01 # Specify the learning rate
num_iters <- 10000 # Specify the number of iterations 
theta_history <- list(num_iters) # will be used to store the value of theta after every iteration 


theta <-  c(0,0) # Initial values of theta
X <- cbind(1,x) # Add a column vector with all values  to be 1 to x so that hypothesis function has an intercept 
for (i in 1:num_iters) {
theta[1] <- theta[1] - alpha * (1/length(y)) * sum(((X%*%theta)- y))
theta[2] <- theta[2] - alpha * (1/length(y)) * sum(((X%*%theta)- y)*X[,2])

theta_history[[i]] <- theta
 } 
print(theta)

lm(y~x)
