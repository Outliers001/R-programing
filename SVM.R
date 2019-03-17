data = read.csv("cancer.csv")
data = data[-1]  #removes the first variable(id) from the data set

normalize <- function(x) ((x - mean(x)) / sd(x) )
data.nor <- as.data.frame(lapply(data[2:9],normalize))
ran = sample(1:nrow(data),.75*nrow(data)) #generating random sample

train = data.nor[ran,]
test = data.nor[-ran,]
can_target_category <- data[ran,1] #target valiable cancer is benign or not.
can_test_category <- data[-ran,1]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(3233)

# Linear Support Vector Machine

svm_Linear <- train(x=train,y= can_target_category, method = "svmLinear",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)
pred = predict(svm_Linear,test)
confusionMatrix(pred, can_test_category )

#Confusion Matrix and Statistics

# Reference
# Prediction  B  M
#           B  8  1
#           M  4 12

#Accuracy : 0.8 

# Non-Linear Support Vector Machine

svm_Linear_r <- train(x=train,y= can_target_category, method = "svmRadial",trControl=trctrl,preProcess = c("center", "scale"),tuneLength = 10)

pred = predict(svm_Linear_r,test)
confusionMatrix(pred, can_test_category )

#Confusion Matrix and Statistics

#Reference
#Prediction  B  M
#         B 10  0
#         M  2 13

#Accuracy : 0.92  

