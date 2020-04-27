
library("Metrics")
library("DAAG")
data1=read.csv(file = file.choose())
View(data1)

data=data1[-1]
head(data)
dim(data)

train_dataset=data[1:140,]
head(train_dataset)

test_dataset=data[141:200,]
head(test_dataset)

S=cbind("TV","Newspaper","Radio")

TV1=lm(Sales~TV,data=train_dataset)
TV1

Radio1=lm(Sales~Radio,data=train_dataset)
Radio1

Newspaper1=lm(Sales~Newspaper,data=train_dataset)
Newspaper1


plot(train_dataset$Sales~train_dataset$TV,xlab="TV",ylab = "Sales")
abline(TV1)

plot(train_dataset$Sales~train_dataset$Radio,xlab="Radio",ylab = "Sales")
abline(Radio1)

plot(train_dataset$Sales~train_dataset$Newspaper,xlab="Newspaper",ylab="Sales")
abline(Newspaper1)

Tvp=predict(TV1,train_dataset)
Radiop=predict(Radio1,train_dataset)
Newspaperp=predict(Newspaper1,train_dataset)


Tvt=predict(TV1,test_dataset)
Radiot=predict(Radio1,test_dataset)
Newspapert=predict(Newspaper1,test_dataset)


TVtrain_mse=mse(train_dataset$Sales,Tvp)
TVtrain_mse

Radiotrain_mse=mse(train_dataset$Sales,Radiop)
Radiotrain_mse

Newspapertrain_mse=mse(train_dataset$Sales,Newspaperp)
Newspapertrain_mse

TVtest_mse=mse(test_dataset$Sales,Tvt)
TVtest_mse

Radiotest_mse=mse(test_dataset$Sales,Radiot)
Radiotest_mse

Newspapertest_mse=mse(test_dataset$Newspaper,Newspapert)
Newspapertest_mse

TrainMSE=c(TVtrain_mse,Radiotrain_mse,Newspapertrain_mse)  
TrainMSE

TestMSE=c(TVtest_mse,Radiotest_mse,Newspapertest_mse)
TestMSE

barplot(TrainMSE,width = 0.02,xlab="data",ylab="error",main="Training Error")
barplot(TestMSE,width=0.02,xlab = "data",ylab="error",main="Testing Error")

model1=cv.lm(data,(Sales~TV),m=5)
