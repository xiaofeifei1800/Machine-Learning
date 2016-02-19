library("rpart")
library("rpart.plot")	
library(ggplot2)
# read data
train <- read.csv("H:/新建文件夹 (2)/Machine-Learning-Demo/data/decision tree.csv")
# data description
#survival        Survival
#                (0 = No; 1 = Yes)
#pclass          Passenger Class
#                (1 = 1st; 2 = 2nd; 3 = 3rd)
#name            Name
#sex             Sex
#age             Age
#sibsp           Number of Siblings/Spouses Aboard
#parch           Number of Parents/Children Aboard
#ticket          Ticket Number
#fare            Passenger Fare
#cabin           Cabin
#embarked        Port of Embarkation
#                (C = Cherbourg; Q = Queenstown; S = Southampton)

#####################################
# check the data first 
lapply(1:ncol(train), function(x) sum(is.na(train[,x])))
colnames(train)[6]

# age has 177 missing value
# using decision tree to replace those missing value

# get the labled age
age = train[!is.na(train[,6]),]

# model the age
model_age = rpart(Age ~ Survived + Pclass + Sex + Parch + Ticket + Fare + Cabin + 
              Embarked, data = age, method = "anova", control = rpart.control(cp = 0.05))
# get the NA age value
pred_age = train[is.na(train[,6]),]
pred_age = pred_age[,-c(1,4,6,7,13)]
head(pred_age)

my_prediction <- predict(model_age, pred_age)

# put the value back to the data
train$Age[is.na(train$Age)] = my_prediction

##############################
# get the title from the name

# look at the class of the name and change it to the character
class(train$Name)
train$Name = as.character(train$Name)

# try to extract the title from the name
test = train$Name[1:5]
test

# use gsub to get the title
title = gsub("([A-z]*,) (\\w{0,4})(.*)" , "\\2", test)

# use strsplit to get the title
test = strsplit(test, "[,.]")
title = sapply(test, function(x) x[2])

# uss the first way to get the title
title = gsub("([A-z]*,) (\\w{0,4})(.*)" , "\\2", train$Name)

train$title = title

######################
# family size

familysize = train$SibSp+train$Parch
train$familysize = familysize

# do a bar plot by age
# cut the age group into every 20
a = cut(train$Age, c(0,10,20,30,40,50,60,70,80))
group = as.character(levels(a))
a = as.numeric(a)

#
barplot(table(a))

# use ggplot to do it
ggplot(dataf, aes(x = group, y = Freq)) + 
  geom_bar(stat = "identity") + 
  geom_text(aes(x = group, y = Freq+50, label = Freq)) +
  xlab("Age group") +
  ggtitle("Distribution of the age")

# show the ratio between survived and not survived
test = cbind(train,a)

ggplot(test, aes(factor(a), fill = factor(Survived))) + 
  geom_bar() 
  
################### model selection #############
# use the crose vaildation to test the model and set different parameter and cp to selet the best model
crosstrain = train[101:nrow(train),]
crosstest = train[1:100,]

# write a function to test the different cp value
error_rate = function(value)
{ 
  my_tree <- rpart(Survived ~ Age + Sex + Pclass, data = crosstrain, method = "class", control=rpart.control(cp=0.04))
  my_prediction <- predict(my_tree, crosstest, type = "class")
  my_prediction = as.numeric(my_prediction)
  my_prediction = my_prediction-1
  error = sum(crosstest$Survived - my_prediction)/100
  return(error)
}  

# find the error between 0.002 to 0.2
error = numeric()
for(i in 1:100)
{  
  x = i*0.002
  error[i] = error_rate(x)
}
plot(error)
# chose 0.04


# select the varable
#for(i in 1:7)
#x = sample(0:1,7, replace = T)
#x = combn(7,6)

#data = train[,c(2,4,5,8,9,10,11)]

#for(i in 1:ncol(x))
#{
#  my_tree <- rpart(Survived ~ Age + Sex + Pclass , data = crosstrain, method = "class", control=rpart.control(cp=0.04))
  
# my_prediction <- predict(my_tree, crosstest, type = "class")
# my_prediction = as.numeric(my_prediction)
# my_prediction = my_prediction-1
# error = sum(crosstest$Survived - my_prediction)/100
#}

# confusion matrix
my_tree <- rpart(Survived ~ Age + Sex + Pclass, data = crosstrain, method = "class", control=rpart.control(cp=0.04))
my_prediction <- predict(my_tree, crosstest, type = "class")
my_prediction = as.numeric(my_prediction)
my_prediction = my_prediction-1

table(crosstest$Survived, my_prediction)


#########################################
# Using randomForest method

library(randomForest)

# fit the model
rf<-randomForest(Survived ~ Pclass + Sex + SibSp + Parch + Fare + Embarked, data = crosstrain,importance=TRUE,ntree=500)

# get the prediction
p = predict(rf,crosstest)

p[p<0.5] = 0
p[p>0.5] = 1
p = as.numeric(p)

error = sum(crosstest$Survived - my_prediction)/100
# add the predict value into solution
my_solution = cbind(my_solution,p)

# comparing the two different methods
for (i in 1:nrow(my_solution))
{
  if (my_solution[i,2] == my_solution[i,3])
  {
    my_solution$compare[i] = 1
  
  }else{
         my_solution$compare[i] = 0
       }
}

# check the number of the differences
sum(my_solution$compare == 0)

