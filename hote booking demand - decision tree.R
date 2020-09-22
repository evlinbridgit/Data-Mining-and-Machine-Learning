
#reading the data

data<-read.csv("C:\\Users\\Evlin Emmanuel\\Desktop\\MSC DATA ANALYTICS\\Data Mining and Machine Learning 1\\project sample\\hotel booking demand\\hotel_bookings.csv",nrows = 10000)

##################decision tree###########

str(data)
data$is_canceled<-as.factor(data$is_canceled)
str(data$is_canceled)
data$reserved_room_type<-as.integer(data$reserved_room_type)
data$assigned_room_type<-as.integer(data$assigned_room_type)
data$previous_cancellations<-as.integer(data$previous_cancellations)

data$booking_changes<-as.integer(data$booking_changes)
str(data$reserved_room_type)
str(data$assigned_room_type)
str(data$previous_cancellations)
str(data$booking_changes)

str(data)
names(data)



#Partitioning the data
set.seed(2)
library(caTools)
partition<-sample.split(data,SplitRatio = 0.7)


#testing and training data
train<-subset(data,partition=="TRUE")
test<-subset(data,partition=="FALSE")



#decision tree using party library
library(party)
tree<-ctree(is_canceled~assigned_room_type+booking_changes+previous_cancellations+reserved_room_type,data = train)

tree
plot(tree)

#predict
test_tree<-predict(tree,test)
plot(test_tree)

#evaluating the fit of model using misclassification
#misclassification error for train data
tab<-table(predict(tree),train$is_canceled)
print(tab)    
1-sum(diag(tab))/sum(tab)   #0.9809 is the miss classification error of train data

#misclassification error for test data
test_pred<-predict(tree,newdata=test)
tab1<-table(test_pred,test$is_canceled)
print(tab1)
1-sum(diag(tab1))/sum(tab1)  #0.98496 is the miss classificatioon error of test data







 






























