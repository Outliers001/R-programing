library(mvtnorm)
library(MASS)

#---------------------------------------------
m=2                    #No. of Classes or Populations
p=10                   #No. of Variables
N<-round(rnorm(m,40,2))# Sample sizes for m-classes respectively
Mu0<-rnorm(p,1,.1)     # Mean Vector
#Mu0<-rep(0,p)
K<-rep(1:2)
#k<-seq(1,5,by=.2)
#---------------------------------------------
Mu<-matrix(0,nr=p,nc=length(K))
for(i in 1:length(K))
{
Mu[,i]<-Mu0+(K[i]-1)
#Mu[,i]<-Mu0+(k[i])
}
Mu
round(colMeans(Mu))
#----------------------------------------------
A<-matrix(runif(p*p,0,1),nr=p)
V<-t(A)%*%A
#
for (ii in 1:dim(Mu)[2])
{
Data<-NULL ;
for (k in 1:m)
{
Muk<-Mu[,ii]+Mu0;Mu0<-Muk
Data0<-cbind(matrix(rmvnorm(N[k], Mu0, V),nr=N[k],nc=p),k)
Data=rbind(Data,Data0)
}}
Data
rownames(Data)<-rownames(Data, do.NULL = FALSE, prefix = "Ind.")
colnames(Data)<-colnames(Data, do.NULL = FALSE, prefix = "Var.")
data.org<-Data[,-11]



###


library(mvtnorm)
library(MASS)

#---------------------------------------------
m=2                    
#No. of Classes or Populations
p=10                   
#No. of Variables
N<-round(rnorm(m,40,2))
# Sample sizes for m-classes respectively
Mu0<-rnorm(p,1,.1)    
 # Mean Vector

Mu0<-rep(0,p)
K<-rep(1:2)
k<-seq(1,5,by=.2)
#---------------------------------------------
Mu<-matrix(0,nr=p,nc=length(K))
for(i in 1:length(K))
{
Mu[,i]<-Mu0+(K[i]-1)
Mu[,i]<-Mu0+(k[i])
}
Mu
round(colMeans(Mu))
#----------------------------------------------
A<-matrix(runif(p*p,0,1),nr=p)
V<-t(A)%*%A
k=10
y<-vector('list',k)
data.org<-vector('list',k)
for(i in 1:k){
         for (ii in 1:2)
         {
             Data<-NULL ;
          for (k in 1:2) #m =2 
{
Muk<-Mu[,ii]+Mu0
Mu0<-Muk
Data0<-cbind(matrix(rmvnorm(N[k], Mu0, V),nr=N[k],nc=p),k)
Data=rbind(Data,Data0)
}}
Data
y[[i]]<-Data

rownames(y[[i]])<-rownames(y[[i]], do.NULL = FALSE, prefix = "Ind.")
colnames(y[[i]])<-colnames(y[[i]], do.NULL = FALSE, prefix = "Var.")
data.org[[i]]<-y[[i]][,-11]
}
data.org
