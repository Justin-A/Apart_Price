setwd("~/Desktop/Justin/2018-1/DataMining/Project")
set.seed(1130)
library(caret)

load("model_xgb1.R") # xgb.model
load("model_xgb2.R") # model

data <- read.csv("NA_Sample2.csv")

idx <- createDataPartition(data$price, p = 0.8, list = F)
train <- data[idx,]
test <- data[-idx,]

control <- trainControl(method = 'cv', search = 'grid', number = 5)

# model
xgb.grid = expand.grid(
  .nrounds = 500,
  .max_depth = 5,
  .eta = 0.3,
  .gamma = 0,
  .colsample_bytree = 0.8,
  .min_child_weight = 1,
  .subsample = 0.94
)

xgb.model <- train(
  price ~ .,
  data = train,
  tuneGrid = xgb.grid,
  tuneLength = 1,
  trControl = control,
  method = 'xgbTree'
)

predict.xgb <- predict(xgb.model, test)
RMSE(test$price, predict.xgb)
R2(test$price, predict.xgb)

setwd("~/Desktop/Justin/2018-1/DataMining/Project/")
data.train <- read.csv("NA_Sample2.csv")
data.train <- data.train[,-1]
data.test <- read.csv("NA_Sample_Test2.csv")

# Model
entitle_xgb.model <- train(
  price ~ .,
  data = data.train,
  tuneGrid = xgb.grid,
  tuneLength = 1,
  trControl = control,
  method = 'xgbTree'
)

# Predict, RMSE, R2
# test <- test[, -1]
predict.xgb <- predict(entitle_xgb.model, test)
RMSE(test$price, predict.xgb)
R2(test$price, predict.xgb)

# Final Prediction
entitle_predict.xgb <- predict(entitle_xgb.model, data.test)
result <- data.frame('price' = entitle_predict.xgb)
setwd("~/Desktop/Justin/2018-1/DataMining/Project/Submission/") ; write.csv(result, "result.csv")

#
data.train <- read.csv("NA_Sample1.csv")
data.test <- read.csv("NA_Sample_Test1.csv")

control <- trainControl(method = 'cv', search = 'grid', number = 5)

xgb.grid = expand.grid(
  .nrounds = 1000,
  .max_depth = c(1:5),
  .eta = c(0.2, 0.3, 0.4, 0.5),
  .gamma = 1,
  .colsample_bytree = 1,
  .min_child_weight = 1,
  .subsample = 1
)

xgb.model <- train(
  price ~ .,
  data = data.train,
  tuneGrid = xgb.grid,
  tuneLength = 1,
  trControl = control,
  method = 'xgbTree'
)


xgb.model <- train(
  price ~ .,
  data = data.train,
  tuneLength = 10,
  trControl = control,
  method = 'xgbTree'
)


xgb.model

price.xgb = predict(xgb.model, data.test)
RMSE(data.test$price, price.xgb)
R2(data.test$price, price.xgb)


save(xgb.model, file = "xgb.model2.R")
load("xgb.model.R")
result <- data.frame(price.xgb)
colnames(result) <- c("price")
View(result)
    ### RMSE : / R2 : 
# nrounds = 1000
# max_depth = c(1:10)
# eta = c(seq(0.1, 1, 0.1))