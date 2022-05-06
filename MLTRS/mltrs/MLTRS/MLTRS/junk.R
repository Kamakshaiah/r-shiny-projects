x <- matrix(rnorm(35), 7, 5)
y <- matrix(rnorm(25), 5, 5)

cl <- sample(c("t", "f"), 7, replace = TRUE)
cl_test <- sample(c("t", "f"), 5, replace = TRUE)
out <- knn(x, y, cl, 5)
out
CrossTable(cl_test, out)

# caret

intrain <- createDataPartition(y = heartdf[, "num"], p= 0.7, list = FALSE)
training <- heartdf[intrain,]
testing <- heartdf[-intrain,]

dim(training); dim(testing)

training[,"num"] = factor(training[,"num"])

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

svm_Linear <- train(num ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear
test_pred <- predict(svm_Linear, newdata = testing)
confusionMatrix(test_pred, testing[, "num"] )
