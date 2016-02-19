

setwd("I:/R Data/141/hw3")
Wholedata = read.csv("digitsTrain.csv")
Wholedata[1]
data1<-get(data[1])  
for(i in 2:length(names))  
         data1<-rbind(data1,get(data[i]))  

library(e1071)   
m <- svm(Wholedata[1:4500,-1],Wholedata[1:4500,1],cross=10,type="C-classification")  
m  
summary(m)  
pred<-fitted(m)  
table(pred,Wholedata[,1])  
   
data2 = Wholedata[4501:5000,-1]
labeltest = Wholedata[4501:5000,1]
pred<-predict(m,data2)  
   
table(pred,labeltest)  