#Markov chain transition matrix 

mc = function(x,y,z,n){
  P = matrix(c(x,1-x,y,1-y),2,2,byrow = TRUE)
  S = c(z,1-z)
  for (i in 1:n) {
    S1 = S%*%P
    
    S=S1
  }
  print(S1)
  print(P)
}
mc(.2,1,.1,10000)