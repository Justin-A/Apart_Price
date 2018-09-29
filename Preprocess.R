Sys.setlocale(category = "LC_CTYPE", locale = "ko_KR.UTF-8")
setwd("~/Desktop/Justin/2018-1/DataMining/Project/")
dir()
data <- read.csv("NA_Sample1.csv")
colnames(data)

# asile_type, eqarthquake, heat_source, heat_type
# 3, 30, 34, 35
for (i in 3:54)(
  hist(data[,i], main = paste("Histogram of", i))
)
summary(data)

plot(data)

# Y ~ X 관계 파악 (powertransform in r)
# psych
# 31, 32, 36, 38, 39, 40, 41, 42, 45
# log
# 원래 데이터에서 전처리 -> Train / Test Split 하여 확인
# 전체 데이터에 대하여 학습
# 중간 테스트에 전처리 한 과정을 똑같이 진행하여, 학습한 데이터와 테스트 데이터의 형식을 맞춘다.
# Test 하여 predict