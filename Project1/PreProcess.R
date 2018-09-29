Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/DataMining/Pro/")
data <- read.csv("apt.csv") ; data <- data[,c(-1, -5, -12, -20, -25, -49)]
test <- read.csv("mid_X.csv") ; test <- test[,c(-4, -11, -19, -24, -48)]
# ########################################################################################
# # Day
# # Train
# y <- substr(data$permission_date, 1, 4)
# m <- substr(data$permission_date, 5, 6)
# d <- substr(data$permission_date, 7, 8)
# 
# day <- paste(y, m, d, sep = '-')
# day[day=="NA-NA-NA"] = NA
# data$permission_date <- as.Date(day)
# 
# # Test
# y <- substr(test$permission_date, 1, 4)
# m <- substr(test$permission_date, 5, 6)
# d <- substr(test$permission_date, 7, 8)
# 
# day <- paste(y, m, d, sep = '-')
# day[day=="NA-NA-NA"] = NA
# test$permission_date <- as.Date(day)
# ########################################################################################
# Factor & Numeric - Train
levels(data[,1]) <- c(NA, "A", "M", "S", "T")
levels(data[,24]) <- c(NA, "N", "Y")
levels(data[,28]) <- c(NA, "CHP", "LNG", "LPG", "OIL")
levels(data[,29]) <- c(NA, "C", "I", "R")

data_factor <- data[, c(1, 24, 28, 29)] ; head(data_factor, 20) # Factor
data_numeric <- data[, c(-1, -24, -28, -29)] ; head(data_numeric, 20) # Numeric

# Factor & Numeric - Test
levels(test[,1]) <- c(NA, "A", "M", "S", "T")
levels(test[,24]) <- c(NA, "N", "Y")
levels(test[,28]) <- c(NA, "CHP", "LNG", "LPG", "OIL")
levels(test[,29]) <- c(NA, "C", "I", "R")

test_factor <- test[, c(1, 24, 28, 29)] ; head(test_factor, 20) # Factor
test_numeric <- test[, c(-1, -24, -28, -29)] ; head(test_numeric, 20) # Numeric
########################################################################################
# NA
# Factor
library(mice)
data_factor_NA <- mice(data_factor, 5)
df1 <- complete(data_factor_NA, 1) ; head(df1, 20)
df2 <- complete(data_factor_NA, 2) ; head(df2, 20)
df3 <- complete(data_factor_NA, 3) ; head(df3, 20)
df4 <- complete(data_factor_NA, 4) ; head(df4, 20)
df5 <- complete(data_factor_NA, 5) ; head(df5, 20)

test_factor_NA <- mice(test_factor, 5)
tdf1 <- complete(test_factor_NA, 1) ; head(tdf1, 20)
tdf2 <- complete(test_factor_NA, 2) ; head(tdf2, 20)
tdf3 <- complete(test_factor_NA, 3) ; head(tdf3, 20)
tdf4 <- complete(test_factor_NA, 4) ; head(tdf4, 20)
tdf5 <- complete(test_factor_NA, 5) ; head(tdf5, 20)

########################################################################################
# Numeric
library(Amelia)

data_numeric_NA <- amelia(data_numeric, m = 5)
adf1 = data_numeric_NA$imputations[[1]] ; adf1[adf1 < 0] = 0 ; head(adf1)
adf2 = data_numeric_NA$imputations[[2]] ; adf2[adf2 < 0] = 0 ; head(adf2)
adf3 = data_numeric_NA$imputations[[3]] ; adf3[adf3 < 0] = 0 ; head(adf3)
adf4 = data_numeric_NA$imputations[[4]] ; adf4[adf4 < 0] = 0 ; head(adf4)
adf5 = data_numeric_NA$imputations[[5]] ; adf5[adf5 < 0] = 0 ; head(adf5)

test_numeric_NA <- amelia(test_numeric, m = 5)
tadf1 = test_numeric_NA$imputations[[1]] ; tadf1[tadf1 < 0] = 0 ; head(tadf1)
tadf2 = test_numeric_NA$imputations[[2]] ; tadf2[tadf2 < 0] = 0 ; head(tadf2)
tadf3 = test_numeric_NA$imputations[[3]] ; tadf3[tadf3 < 0] = 0 ; head(tadf3)
tadf4 = test_numeric_NA$imputations[[4]] ; tadf4[tadf4 < 0] = 0 ; head(tadf4)
tadf5 = test_numeric_NA$imputations[[5]] ; tadf5[tadf5 < 0] = 0 ; head(tadf5)

########################################################################################
# Merge
data1 <- cbind(df1, adf1) ; test1 <- cbind(tdf1, tadf1)
data2 <- cbind(df2, adf2) ; test2 <- cbind(tdf2, tadf2)
data3 <- cbind(df3, adf3) ; test3 <- cbind(tdf3, tadf3)
data4 <- cbind(df4, adf4) ; test4 <- cbind(tdf4, tadf4)
data5 <- cbind(df5, adf5) ; test5 <- cbind(tdf5, tadf5)

########################################################################################
# Preprocess - Train
data <- data5
# Outlier
w1 <- which(data[,6] == 651.9200)
w2 <- which(data[,27] == 249024.5600)
w3 <- which(data[,27] == 201802.7800)
w4 <- which(data[,28] == 2222)
w5 <- which(data[,28] == 1113)
data <- data[-c(w1, w2, w3, w4, w5),]

# Log
for (i in c(5, 27, 28, 29, 32, 36, 38, 39, 40, 41, 42, 44))(
  data[,i] = log(data[,i]))

for (i in c(5, 27, 28, 29, 32, 36, 38, 39, 40, 41, 42, 44))(
  data[,i][is.infinite(data[,i])] = 0
)

# Preprocess - Test
test <- test5
# Outlier
t1 <- which(test[,27] == 249024.5600)
t2 <- which(test[,27] == 201802.7800)

# Log
for (i in c(5, 27, 28, 29, 32, 36, 38, 39, 40, 41, 42))(
  test[,i] <- log(test[,i])
)

for (i in c(5, 27, 28, 29, 32, 36, 38, 39, 40, 41, 42))(
  test[,i][is.infinite(test[,i])] = 0
)


########################################################################################
price <- data$price
data <- data[, -44]
data$price <- price
########################################################################################

write.csv(data, file = "data5.csv")
write.csv(test, file = "test5.csv")
