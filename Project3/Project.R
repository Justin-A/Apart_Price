set.seed(1130)
library(caret)

setwd("~/Desktop/Justin/2018-1/DataMining/Project/")
data <- read.csv("apt.csv")
test <- read.csv("mid_X.csv")
levels(data[,2]) <- c(NA, 1, 2, 3, 4)
levels(data[,29]) <- c(NA, 1, 2)
levels(data[,33]) <- c(NA, 1, 2, 3, 4)
levels(data[,34]) <- c(NA, 1, 2, 3)

# Factor -> Numeric
for (i in c(2, 29, 33, 34)){
  data[,i] <- as.numeric(data[,i])
}
data <- data[,-1]
# library(mice)
# imp.m <- mice(data, m = 10)
# data <- complete(imp.m, 7)

library(Amelia)
imp.a <- amelia(data, m = 4)
sa1 <- imp.a$imputations[[1]] ; write.csv(sa1, "NA_Sample1.csv")
sa2 <- imp.a$imputations[[2]] ; write.csv(sa2, "NA_Sample2.csv")
sa3 <- imp.a$imputations[[3]] ; write.csv(sa3, "NA_Sample3.csv")
sa4 <- imp.a$imputations[[4]] ; write.csv(sa4, "NA_Sample4.csv")

# Test Setting
test <- read.csv("mid_X.csv")
levels(test[,1]) <- c(NA, 1, 2, 3, 4)
levels(test[,28]) <- c(NA, 1, 2)
levels(test[,32]) <- c(NA, 1, 2, 3)
levels(test[,33]) <- c(NA, 1, 2, 3)

# Factor -> Numeric
for (i in c(1, 28, 32, 33)){
  test[,i] <- as.numeric(test[,i])
}

library(Amelia)
imp.a <- amelia(test, m = 4)
sa1 <- imp.a$imputations[[1]] ; write.csv(sa1, "NA_Sample_Test1.csv")
sa2 <- imp.a$imputations[[2]] ; write.csv(sa2, "NA_Sample_Test2.csv")
sa3 <- imp.a$imputations[[3]] ; write.csv(sa3, "NA_Sample_Test3.csv")
sa4 <- imp.a$imputations[[4]] ; write.csv(sa4, "NA_Sample_Test4.csv")



data <- sa1
idx <- createDataPartition(data$price, p = 0.8, list = F)
data.train <- data[idx,]
data.test <- data[-idx,]

control <- trainControl(method = 'cv', search = 'grid', number = 5)

xgb.grid = expand.grid(
  .nrounds = 10000,
  .max_depth = c(3, 5, 7, 9),
  .eta = c(0.1, 0.3, 0.5, 0.7, 0.9),
  .gamma = 1,
  .colsample_bytree = 1,
  .min_child_weight = 1,
  .subsample = 1
)

xgb.model <- train(
  price ~ .,
  data = data.train,
  tuneGrid = xgb.grid,
  trControl = control,
  method = 'xgbTree'
)

price.xgb = predict(xgb.model, data.test)
RMSE(data.test$price, price.xgb)
R2(data.test$price, price.xgb)
