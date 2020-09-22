library(ranger)
library(caret)
library(data.table)

data<- read.csv("C:\\Users\\Evlin Emmanuel\\Desktop\\MSC DATA ANALYTICS\\Data Mining and Machine Learning 1\\project datasets\\credit card code +data\\creditcard.csv")
#explore
data.table(data)

#doing random statistics
summary(data)
table(data$Class) #to check how many o's and 1's
names(data)

#summary of amount
summary(data$Amount)

#normalizing amount
data$Amount<-scale(data$Amount)

#removing time from  data
data1<-data[,-c(1)]
head(data1)

#data patition
set.seed(123)
library(caTools)
part_data<-sample.split(data1$Class,SplitRatio = 0.80)
training_data<-subset(data1,part_data==TRUE)
testing_data<-subset(data1,part_data==FALSE)


#checking diamension
dim(training_data)
dim(testing_data)



#############logistic regression##########


logistic_model1<-glm(Class~.,training_data,family = binomial())
summary(logistic_model1)#v21,v20,v14,v13,v10,v8,v4  are stat sig at 0.001
logistic_model<-glm(Class~.,testing_data,family = binomial())
summary(logistic_model)#only intercept and v8 are stat significant at p value =0.01 
plot(logistic_model)




#plotting roc curve
 library(pROC)
predict<- predict(logistic_model1,testing_data,probability=TRUE)
auc<-roc(testing_data$Class,predict,plot=TRUE,col="red")
#accuracy of the model is 90% after analysing the roc curve (*)


################Random Forest####################################

library(readr)
data<- read.csv("C:\\Users\\Evlin Emmanuel\\Desktop\\MSC DATA ANALYTICS\\Data Mining and Machine Learning 1\\project datasets\\credit card code +data\\creditcard.csv", nrows = 10000)
str(data)
#raw.data   #Displaying the data 


#Analysing the data enterted.
# Total number of Fraud and Non-Fraud Rows in data
rowsTotal <- nrow(data)
fraudRowsTotal <- nrow(data[data$Class == 1,])
nonFraudRowsTotal <- rowsTotal - fraudRowsTotal

rowsTotal
fraudRowsTotal
nonFraudRowsTotal


#partitioning data into training and testing subsets
set.seed(123)
part<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
train<-data[part==1,]
test<-data[part==2,]


#performing random forest
library(randomForest)
set.seed(222)
#training the model using train data
model <- randomForest(Class~.,data=train)
print(model)
plot(model)

#here we got a regression model, printing the attributes of the model for checking
attributes(model)


library(caret)
predict(model,train)
head(train$Class)

#prediction with test data
p2<-predict(model,test)



library(ggplot2)
plot(model)#plotting the error rate of the model
# printing the number of  nodes for the trees
hist(treesize(model),main="number of nodes for the trees",col = "blue")

#printing the variable of high importance
varImpPlot(model)
#printing the most used variables
varUsed(model)

#partialdependence plot
partialPlot(model,train,V12,"1")
conditionalTree<- ctree(Class~.,data=data)
plot(conditionalTree)
#extract single tree
getTree(model,1,labelVar = TRUE)

#Evaluating the model to find out accuracy using rsquare and rmse since regression is performed
model$rsq#rsquared values of training data
rmse<-sqrt(mean(data$Class)^2)
rmse   #the rmse value is 0.0038

rmse1<-sqrt(mean(test$Class)^2)
rmse1   #0.002

rmse2<-sqrt(mean(train$Class)^2)
rmse2  #0.004
        #the rmse values of test and train data are almost similar, so the model should fit well
