setwd("~/Desktop/Justin/2018-1/DataMining/Pro")
# Setting
set.seed(1234)
library(caret)

data <- read.csv("data1.csv") ;data <- data[, -1]
idx <- createDataPartition(data$price, p = 0.8, list = F)

data.train <- data[idx, ]
data.test <- data[-idx,]

control <- trainControl(method = 'cv', search = 'grid', number = 5)

# RandomForest
# Validation
rf.model <- train(
	price ~ .,
	data = data.train,
	tuneGrid = data.frame(.mtry = c(25, 30, 35, 40)),
	trControl = control,
	method = 'rf'
)

price.rf <- predict(rf.model, data.test)
RMSE(data.test$price, price.rf)
RMSE(exp(data.test$price), exp(price.rf))
R2(data.test$price, price.rf)

save(rf.model, file = "#####.R") # Save Model
load("#####.R") # Load Model

# Real
rf.model_real <- train(
	price ~ .,
	data = data,
	tuneGrid = data.frame(.mtry = c(10, 15, 20)),
	trControl = control,
	method = 'rf'
)

save(rf.model_real, file = "#####.R") # Save Model
load("#####.R") # Load Model

price.rf_real <- predict(rf.model_real, test)

# Check
RMSE(data.test$price, price.rf_real)
R2(data.test$price, price.rf_real)


result <- data.frame('price' = price.rf_real)

setwd("~/Desktop/Justin/2018-1/DataMining/Pro/Submission/")
write.csv(result, "#####.csv")
setwd("~/Desktop/Justin/2018-1/DataMining/Pro/")
