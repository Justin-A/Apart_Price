actions <- c("A", "B")
rewards <- c(A = 0, B = 0)
counts <- c(A = 0, B = 0)
values <- c(A = 0, B = 9)
mean <- c(A = 1, B = 2)
sd <- c(A = 0.5, B = 1)

epsilon <- 0.3

for(i in 1:100){
  if(epsilon < runif(1)){
    best = names(which.max(values))
  }
  else {
    best = sample(actions, 1) 
  }
  
  print(best)
  R = rnorm(1, mean[best], sd[best])
  rewards[best] = rewards[best] + R
  counts[best] = counts[best] + 1
  values[best] = rewards[best] / counts[best]
}
counts
values

actions <- c('A', 'B')
counts <- list(A = 0, B = 0)
values <- list(A = 0, B = 0)
mean <- list(A = 1, B = 2)
sd <- list(A = 0.5, B = 1)

epsilon <- 0.3 # 70%는 이용, 30%는 무작위

for(i in 1:100){
  if(epsilon < runif(1)){
    best = names(which.max(values))
  }
  else {
    best = sample(actions, 1)
  }
  print(best)
  
  R = rnorm(1, mean[best], sd[best])
  Q = values[best]
  
  counts[best] = counts[best] + 1
  values[best] = Q + (R - Q) / counts[best]
}
counts
values

actions = c('A', 'B')
counts = c(A = 0, B = 0)
values = c(A = 0, B = 0)
mean = c(A = 1, B = 2)
sd = c(A = 0.5, B = 1)
epsilon = 0.3

alpha = 0.1  # 가중치

for(i in 1:100){
  
  if(epsilon < runif(1)){
    best = names(which.max(values))
  }
  else {
    best = sample(actions, 1)
  }
  print(best)
  counts[best] = counts[best] + 1
  
  R = rnorm(1, mean[best], sd[best])
  Q = values[best]
  
  values[best] = Q + alpha * (R - Q) # 가치 업데이트
}
counts
values

actions = c('A', 'B')
counts = c(A = 0, B = 0)
values = c(A = 10, B = 10)  # 낙관적 초기화
mean = c(A = 1, B = 2)
sd = c(A = 0.5, B = 1)

epsilon = 0.01 # 입실론이 작아도 초반에는 탐색을 많이 한다

alpha = 0.1

for(i in 1:100){
  if(epsilon < runif(1)){
    best = names(which.max(values))
  }
  else {
    best = sample(actions, 1)
  }
  counts[best] = counts[best] + 1
  print(best)
  
  R = rnorm(1, mean[best], sd[best])
  Q = values[best]
  
  values[best] = Q + alpha * (R - Q)
}
counts

actions = c('A', 'B')
counts = c(A = 0, B = 0)
values = c(A = 0, B = 0)
mean = c(A = 1, B = 2)
sd = c(A = 0.5, B = 1)
control = 2

for(t in 2:101){
  # UCB 알고리즘으로 행동을 선택한다
  best = names(which.max(values + control * sqrt(log(t) / counts)))
  
  counts[best] = counts[best] + 1
  print(best)
  
  R = rnorm(1, mean[best], sd[best])
  Q = values[best]
  
  values[best] = Q + alpha * (R - Q)
}
counts
values

actions = c('A', 'B')
counts = c(A = 0, B = 0)
mean = c(A = 1, B = 2)
sd = c(A = 0.5, B = 1)
probs = c(A = .5, B = .5)  # A안과 B안에서 가치가 2일 확률

for(t in 1:100){
  # A와 B의 가치를 무작위 추출한다
  values = c(
    ifelse(probs['A'] < runif(1), 1, 2),
    ifelse(probs['B'] < runif(1), 1, 2))
  
  best = names(which.max(values))
  counts[best] = counts[best] + 1
  print(best)
  
  R = rnorm(1, mean[best], sd[best])
  
  # P(R|가치=1) * P(가치=1)
  p1 = dnorm(R, 1) * (1 - probs[best])
  
  # P(R|가치=2) * P(가치=2)
  p2 = dnorm(R, 2) * probs[best]
  
  # 베이즈 정리: P(가치=2|R) = P(R|가치=2) * P(가치=2) / P(R)
  probs[best] = p2 / (p1 + p2)
}
counts
probs

actions = c('A', 'B')
counts = c(A = 0, B = 0)
mean = c(A = 1, B = 2)
sd = c(A = 0.5, B = 1)

pref = c(A = 0, B = 0)  # A와 B의 선호도
mean_rewards = 0  # 지금까지 평균 보상
learning_rate = 0.1 # 학습률

for(t in 1:100){
  # 소프트맥스로 확률을 구한다
  e = exp(pref)
  p = e / sum(e)
  
  # 행동을 확률적으로 선택한다
  chosen = sample(actions, 1, prob=p)
  
  counts[chosen] = counts[chosen] + 1
  print(best)
  
  R = rnorm(1, mean[chosen], sd[chosen])
  
  # 평균 보상을 빼서 +이면 잘한 것이므로 선호도를 높인다
  advantage = R - mean_rewards
  
  # 경사하강법으로 선택된 행동의 선호도를 업데이트한다
  # 직관적으로 이해한다면 advantage가 크고 확률이 낮을 수록 선호도를 더 높인다
  pref[chosen] = pref[chosen] + learning_rate * advantage * (1 - p[chosen])
  
  # 경사하강법으로 선택되지 않은 행동의 선호도도 업데이트 한다
  nc = setdiff(actions, chosen)  # not chosen
  pref[nc] = pref[nc] - learning_rate * advantage * p[nc]
}
counts
pref



# 모의담금질은 Temperature 정의해서 나눠주는 방식을 진행하면 된다.