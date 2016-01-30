# Plot the data
pairs(iris[1:4], main = "Anderson's Iris Data -- 3 species", pch = 21, bg = c("red", "green3", "blue")[unclass(iris$Species)])

# Use linear discriminant analysis
library(MASS)
iris.lda <- lda(Species ~ ., data = iris)
summary(iris.lda)

# load the package
library(VGAM)
# load data
data(iris)
# fit model
fit1 <- vglm(Species~ Petal.Width, family=multinomial, data=iris)
fit2 <- vglm(Species~ Petal.Width+Petal.Length, family=multinomial, data=iris)
fit3 <- vglm(Species~ Petal.Width+Petal.Length+Sepal.Length, family=multinomial, data=iris)
fit4 <- vglm(Species~., family=multinomial, data=iris)
AIC(fit1)
AIC(fit2)
AIC(fit3)
AIC(fit4)

# summarize the fit
summary(fit3)

# make predictions
probabilities <- predict(fit3, iris[,1:4], type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]
# summarize accuracy
table(predictions, iris$Species)

##### diag
class1 = iris[1:50,]
class1 = rbind(class1, iris[101:150,])
class2 = iris[51:150,]


fit3.1 <- loglm(Species~ Petal.Width+Petal.Length+Sepal.Length, data=class1)
fit3.2 <- loglm(Species~ Petal.Width+Petal.Length+Sepal.Length, data=class2)
plot(fit3.1)
plot(fit3.2)
summary(fit3.2)

### residual
library(binomTools)
Residuals(fit3.2, type = c("pearson"))[c(34,57,84)]
Residuals(fit3.2, type = c("deviance"))[c(34,57,84)]
Residuals(fit3.2, type = c("standard.deviance"))[c(34,57,84)]
Residuals(fit3.2, type = c("standard.pearson"))[c(34,57,84)]



# summarize the fit
summary(fit3)
# make predictions
probabilities <- predict(fit3, iris[,1:4], type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]
# summarize accuracy
table(predictions, iris$Species)

