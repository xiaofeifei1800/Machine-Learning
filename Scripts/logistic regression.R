#######################
# Purpose for this code is to do the classification by using logistic regression. 
# I use iris data and try to classify two type of species (versicolor and virginica)
# by Petal.Width and Petal.Length.

log_plot = function(col)
{
a = split(iris,iris[,col])
total = lapply(1:length(a), function(x) nrow(a[[x]]))
TD = lapply(1:length(a), function(x) TD = sum(a[[x]][,5] == "virginica"))
newdata = cbind(unlist(total),unlist(TD),as.numeric(names(a)))
newdata = data.frame(newdata)
temp = names(iris)[col]

names(newdata) = c("total", "virginica", temp)

glm.out = glm(cbind(newdata[,2], newdata[,1]-newdata[,2]) ~ newdata[,3], family=binomial(logit))
plot(newdata[,2]/newdata[,1] ~ newdata[,3], xlab = temp, ylab = "Probability of Vir" )
lines(newdata[,3], glm.out$fitted, type="l", col="red")
}

par(mfrow=c(1,2))

for(i in 1:4)
{
log_plot(col = i)
}

### 

logit.fit <- glm(Species~Petal.Width+Petal.Length+Sepal.Length+Sepal.Width, family = binomial(link = 'logit'), data = iris[51:150,])
summary(logit.fit)

# fit the model
logit.fit <- glm(Species~Petal.Width+Petal.Length, family = binomial(link = 'logit'), data = iris[51:150,])
logit.fit <-loglm(Species~Petal.Width+Petal.Length, data = iris[51:150,])

# get the predictions from model
logit.predictions <- ifelse(predict(logit.fit) > 0,'virginica', 'versicolor')

# confussion matrix 
table(iris[51:150,5],logit.predictions)

# test the significant of model
anova(logit.fit, test="Chisq")

# look at parameter significant, also get parameter for odd ratio
summary(logit.fit)

# odds ratio
# Petal.Width
exp(10.447)

# Petal.Length
exp(5.755)

# goodness of fit.
library(ResourceSelection)

change = iris[51:150,5]
change = as.character(change)
change[change == "virginica"] = 1
change[change == "versicolor"] = 0
change = as.factor(change)
hoslem.test(change,fitted(logit.fit),g = 10)
# model Diagnostics including four plots
# residuals vs fitted, normal qq, scale-location, residuals vs leverage
library(MASS)
plot(logit.fit)


# here is the plot for our model classify the true data
library(ggplot2)

# plot data
qplot(Petal.Width, Petal.Length, colour = Species,  data = iris[51:150,], main = "Iris classification")

# get slope and intercept from model
slope1 <- coef(logit.fit)[2]/(-coef(logit.fit)[3])
intercept1 <- coef(logit.fit)[1]/(-coef(logit.fit)[3])

# add the regression line
ggplot(iris[51:150,], aes(x=Petal.Width, y=Petal.Length, colour = Species)) + 
    geom_point() +    
    geom_abline(intercept = intercept1, slope = slope1) +
    ggtitle("Iris classification")

# Doing a residual analysis and identifying outliers or influential observations.
library(binomTools)
Residuals(logit.fit, type = "pearson")


