Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/DataMining/Pro")
# Setting
set.seed(1234)
library(caret)

data <- read.csv("data1.csv") ;data <- data[, -1]
idx <- createDataPartition(data$price, p = 0.8, list = F)

data.train <- data[idx, ]
data.test <- data[-idx,]

control <- trainControl(method = 'cv', search = 'grid', number = 5)

# XGBoost
# Validation
xgb.grid = expand.grid(
  .nrounds = c(1300, 1400, 1500, 1600, 1700),
  .max_depth = c(5, 6, 7),
  .eta = c(0.05, 0.1),
  .gamma = 0,
  .colsample_bytree = 1,
  .min_child_weight = c(0.5, 0.75, 1),
  .subsample = c(0.5, 0.6, 0.7, 0.8, 0.9, 1.0)
)

xgb.model <- train(
  price ~ .,
  data = data.train,
  tuneGrid = xgb.grid,
  tuneLength = 1,
  trControl = control,
  method = 'xgbTree'
)

price.xgb <- predict(xgb.model, data.test)
RMSE(data.test$price, price.xgb)
RMSE(exp(data.test$price), exp(price.xgb))
R2(data.test$price, price.xgb)

save(xgb.model, file = "Model_Xgb_3.R") # Save Model
load("#####.R") # Load Model

# Real

xgb.model_real <- train(
  price ~ .,
  data = data,
  tuneGrid = xgb.grid,
  tuneLength = 1,
  trControl = control,
  method = 'xgbTree'
)

save(xgb.model_real, file = "#####.R") # Save Model
load("#####.R") # Load Model

price.xgb_real <- predict(rf.model_real, test)
# Check
RMSE(data.test$price, price.xgb_real)
R2(data.test$price, price.xgb_real)

result <- data.frame('price' = price.xgb_real)

setwd("~/Desktop/Justin/2018-1/DataMining/Pro/Submission/")
write.csv(result, "#####.csv")
setwd("~/Desktop/Justin/2018-1/DataMining/Pro/)
