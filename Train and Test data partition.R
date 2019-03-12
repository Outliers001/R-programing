dat = read.csv("Dat.csv")

#row show in vector
v = c(500,40,56)
dat[v,]

# row show in a range
v1 = 100:108
dat[v1,]

#Taking sample from data
ind = sample(1000,100) #sample size 100 and population 1000
ind
dat[ind,]

#train model
train = dat[ind,]
test = dat[-ind,]

library("ggplot2")
ggplot(train,aes(x,y))+
  geom_point()+
  ggtitle("Train Set")

ggplot(test,aes(x,y))+
  geom_point()+
  ggtitle("Test Set")

#plotting least square line

model = lm(y~x,data = train)
range(x)
x = c(1,8)
y=predict(model,data.frame(x))

endpoint = data.frame(x,y)

ggplot(train,aes(x,y))+
  geom_point()+
  geom_line(data = endpoint, aes(x=x,y=y), color = "red")+
  ggtitle("Train Set")

# Alsooo

ggplot(train,aes(x,y))+
  geom_point()+
  stat_smooth(method = "lm", col = "red")+
  ggtitle("Train Set")

# or a function
ggplotRegression <- function (fit) {
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red") +
    labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                       "Intercept =",signif(fit$coef[[1]],5 ),
                       " Slope =",signif(fit$coef[[2]], 5),
                       " P =",signif(summary(fit)$coef[2,4], 5)))
}

ggplotRegression(lm(y~x, data = train))



