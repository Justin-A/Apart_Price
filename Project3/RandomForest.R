setwd("~/Desktop/Justin/2018-1/DataMining/Project")
set.seed(1130)
library(caret)

data <- read.csv("NA_Sample2.csv")

idx <- createDataPartition(data$price, p = 0.8, list = F)
train <- data[idx,]
test <- data[-idx,]

control <- trainControl(method = 'cv', search = 'grid', number = 5)

rf.model = train(
  price ~ .,
  data = train,
  tuneGrid = data.frame(.mtry = 30),
  trControl = control,
  method = 'rf'
)

# mtry  RMSE      Rsquared   MAE     
# 1     17461.02  0.8962059  9999.581
# 2     14120.63  0.9291393  7961.394
# 3     12786.16  0.9415561  7205.675
# 4     12104.75  0.9472775  6855.068
# 5     11662.23  0.9507319  6627.503
# 6     11433.67  0.9522618  6525.177
# 7     11266.34  0.9534237  6451.300
# 8     11126.47  0.9543025  6401.710
# 9     11035.40  0.9548034  6365.611
# 10    10971.96  0.9550592  6351.712

price.rf <- predict(rf.model, test)
RMSE(test$price, price.rf)
R2(test$price, price.rf)

# Training Entire Set
entire_rf.model <- train(
  price ~ .,
  data = data,
  tuneGrid = data.frame(.mtry = 10),
  trControl = control,
  method = 'rf'
)

# Predict, RMSE, R2
# test <- test[, -1]
predict.rf <- predict(entire_rf.model, test)
RMSE(test$price, predict.rf)
R2(test$price, predict.rf)

# Final Prediction
data.test <- read.csv("NA_Sample1.csv")
entitle_predict.rf <- predict(entire_rf.model, data.test)
result <- data.frame('price' = entitle_predict.rf)
setwd("~/Desktop/Justin/2018-1/DataMining/Project/Submission/") ;write.csv(result, "rf_1(4280.067).csv")
