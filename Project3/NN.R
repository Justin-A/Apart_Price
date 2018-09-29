set.seed(1130)
library(caret)

setwd("~/Desktop/Justin/2018-1/DataMining/Project/")
data.train <- read.csv("NA_Sample1.csv")
data.test <- read.csv("NA_Sample_Test1.csv")

control <- trainControl(method = 'cv', search = 'grid', number = 5)

# install.packages('keras')
# install_keras()
library(keras)

model <- keras_model_sequential()
model %>%
  layer_dense(units = 1, activation = 'sigmoid', input_shape = c(NX))

model %>% compile(
  loss = 'binary_crossentropy',
  optimizer = optimizer_adam(),
  metrics = c('accuracy')
)

history <- model %>% fit(
  x.train
)



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