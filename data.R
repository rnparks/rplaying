library(caret)
library(e1071)
data(iris)
split=0.50
trainIndex <- createDataPartition(iris$Species, p=split, list=FALSE)
data_train <- iris[ trainIndex,]
data_test <- iris[-trainIndex,]
model <- train(Species~., data=data_train, method="glmnet")
results <- predict(model, data_test)
summary(results)
mean(results == data_test$Species)

fit <- vglm(Species~., family=multinomial, data=iris)
# summarize the fit
summary(fit)
# make predictions
probabilities <- predict(fit, iris, type="response")
predictions <- apply(probabilities, 1, which.max)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]


mean(predictions == iris$Species)
    


