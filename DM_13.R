# 강화학습 (어떻게 하면 보상을 높일까?)
# Multi_Armed ~ (A / B Testing 현업완전많이)

# 마코프 결정 과정 (마코프 : 직전 단계 이전에는 영향을 미치지 않는다.)
# 시계열분석을 진행할 때, 주가가 마코프 프로세스다. (직전 주가만 영향을 주고 나머진 영향 X (직전, 현재))
# History가 쌓이면 처리하기 복잡. 단순하게 생각해서 현재와 직전의 시점을 고려하자
# State에 따라서 Action을 정할 수 있다.
# 그 Action을 진행한다면, 다음의 다양한 State로 넘어간다.

# State 마다 어떠한 Action이 가장 좋은가? (Policy)
# 보상 : 한번한번 행동마다 받는 값. 수익 : 장기간에 걸쳐 총 보상 (시점에 따라 할인값?)
# On policy -> 직접테스트 ; Off Policy : 남들이 하는걸 보고 배우는

# Off Policy -> Behavior policy / Target policy
# Importance Sampling (OFF) 남이 한걸 보고 하기 때문에 괜찮은방법
library(reticulate)
gym <- import('gym')

env <- gym$make('Blackjack-v0')
env$step(as.integer(0))

# 가치를 초기화
value.df = data.frame(
  player = rep(1:21, 10, each=4),
  dealer = rep(1:10, each=84),
  ace = rep(c(T, F), 210, each=2),
  action = rep(c(0, 1), 420),
  value = 0
)

available.actions <- function(value.df, status){
  cond <- value.df$player == status[[1]] & 
    value.df$dealer == status[[2]] & 
    value.df$ace == status[[3]]
  
  value.df$value[cond]
}

# 입실론 탐욕 알고리즘으로 하나의 에피소드를 수집한다
epsilon.greedy <- function (env, value.df, epsilon = 0.1){
  
  episode = list()
  status <- env$reset()
  done = FALSE
  
  while(!done){
    
    if(runif(1) < epsilon){
      action <- sample(c(0, 1), 1)
    } else {
      action <- which.max(available.actions(value.df, status)) - 1
    }
    
    # 상태와 행동을 에피소드에 기록한다
    t <- length(episode) + 1
    status$action <- action
    episode[[t]] <- status
    
    # 행동을 실행하고 결과를 확인한다
    result <- env$step(as.integer(action))
    
    status <- result[[1]]
    status$reward <- result[[2]]
    done <- result[[3]]
  }
  
  list(episode = episode, G = result[[2]])
}
action.value.cond <- function(value.df, status){
  value.df$player == status[[1]] &
    value.df$dealer == status[[2]] & 
    value.df$ace == status[[3]] &
    value.df$action == status$action
}

alpha = 0.1
for(i in 1:1000){
  # 한 게임의 에피소드를 수집한다
  ep <- epsilon.greedy(env, value.df)  
  
  # 가치를 업데이트 한다
  for(status in ep$episode){
    cond <- action.value.cond(value.df, status)
    V <- value.df$value[cond]
    value.df$value[cond] <- V + alpha * (ep$G - V)
  }
}

library(dplyr)

# 플레이어 점수에 따라 더이상 받지 않는 행동(0)과 더 받는 행동(1)의 평균 가치를 구한다
v <- value.df %>% group_by(player, action) %>% summarise(mean(value))
v0 <- v %>% filter(action == 0)
v1 <- v %>% filter(action == 1)

# 그래프로 그려서 비교해본다
plot(v0$player, v0$`mean(value)`, typ='l', xlab = 'player', ylab = 'value')
lines(v1$player, v1$`mean(value)`, col=2)
legend('topleft', legend = c('stay', 'hit'), fill = c(1, 2))
# MCTS 단점 : 끝날때까지 해봐야한다.
# 이는 게임 중간에 업데이트가 되지 않는다.
# 끝나지 않는 게임에 대해서 학습하지 못한다.

# 시간차 학습 : 몬테카를로 트리서치의 단점을 보완
# 에피소드가 끝나지 않아도, 다음상태로 넘어간다면 가치추정을 바꿀 수 있다.

# SARSA , Q_Learning ( A, A' ) 차이 (미세한)
# Q_Learning (Off Policy, 시간차학습)

gamma = 1  # 할인은 하지 않는다


# 가치를 초기화
qlearn.df = data.frame(
  player = rep(1:21, 10, each=4),
  dealer = rep(1:10, each=84),
  ace = rep(c(T, F), 210, each=2),
  action = rep(c(0, 1), 420),
  value = 0
)

gamma = 1
for(i in 1:1000){
  # 행동 정책으로 한 게임의 에피소드를 수집한다
  ep <- epsilon.greedy(env, value.df)  
  
  # 대상 정책의 가치를 업데이트 한다
  N = length(ep$episode)
  for(i in 1:N){
    # 현재 상태의 기존 가치 추정치를 찾는다
    cond1 <- action.value.cond(qlearn.df, ep$episode[[i]])
    V1 <- qlearn.df$value[cond1]
    
    if(i == N){  # 마지막 상태면
      R = ep$G  # 게임의 수익이 보상
      V2 = 0  # 다음 상태가 없으므로 가치도 0
    } else {  # 마지막 상태가 아니면
      R = 0  # 게임 중이므로 보상은 없다
      
      # 대상 정책 기준으로 다음 상태의 가치를 찾는다
      cond2 <- action.value.cond(qlearn.df, ep$episode[[i + 1]])
      V2 <- qlearn.df$value[cond2]
    }
    
    # 업데이트
    qlearn.df$value[cond1] <- V1 + alpha * (R + gamma * V2 - V1)
  }
}

v <- qlearn.df %>% group_by(player, action) %>% summarise(mean(value))
v0 <- v %>% filter(action == 0)
v1 <- v %>% filter(action == 1)

# 그래프로 그려서 비교해본다
plot(v0$player, v0$`mean(value)`, typ='l', xlab = 'player', ylab = 'value')
lines(v1$player, v1$`mean(value)`, col=2)
legend('topleft', legend = c('stay', 'hit'), fill = c(1, 2))

