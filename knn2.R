data = read.csv("cancer.csv")

head(data)
#id diagnosis_result radius texture perimeter area smoothness compactness symmetry fractal_dimension
#1  1                M     23      12       151  954      0.143       0.278    0.242             0.079
#2  2                B      9      13       133 1326      0.143       0.079    0.181             0.057
#3  3                M     21      27       130 1203      0.125       0.160    0.207             0.060
#4  4                M     14      16        78  386      0.070       0.284    0.260             0.097
#5  5                M      9      19       135 1297      0.141       0.133    0.181             0.059
#6  6                B     25      25        83  477      0.128       0.170    0.209             0.076

data = data[-1]  #removes the first variable(id) from the data set

#diagnosis_result radius texture perimeter area smoothness compactness symmetry fractal_dimension
#1                M     23      12       151  954      0.143       0.278    0.242             0.079
#2                B      9      13       133 1326      0.143       0.079    0.181             0.057
#3                M     21      27       130 1203      0.125       0.160    0.207             0.060
#4                M     14      16        78  386      0.070       0.284    0.260             0.097
#5                M      9      19       135 1297      0.141       0.133    0.181             0.059
#6                B     25      25        83  477      0.128       0.170    0.209             0.076


prop.table(table(data$diagnosis_result))*100
# B  M 
# 38 62 

normalize <- function(x) ((x - mean(x)) / sd(x) )
data.nor <- as.data.frame(lapply(data[2:9],normalize))
ran = sample(1:nrow(data),.75*nrow(data)) #generating random sample

train = data.nor[ran,]
test = data.nor[-ran,]

library(class)
can_target_category <- data[ran,1] #target valiable cancer is benign or not.
can_test_category <- data[-ran,1]
pr <- knn(train,test,cl=can_target_category,k=10)

confusionMatrix(can_test_category,pr)

#Confusion Matrix and Statistics

#Reference
# Prediction  B  M
# B  5  4
# M  0 16

# Accuracy : 0.84            
        



