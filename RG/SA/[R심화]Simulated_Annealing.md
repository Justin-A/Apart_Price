---
title: "Simulated_Anneling"
author: "권오성, 안상준, 임필령"
date: "5/24/2018"
output: html_document
---

수업시간에 여러 테스팅 중, 모의담금질 관련 코드가 없어서
저희조가 직접 구현해 보았습니다.
제시되어있는 코드 중, T값을 추가하여 SimulatedAnneling 함수로 정의하고,
T값이 올라갈 때마다 Action값이 동일한 분포를 이루는지 실험해보았습니다.
A, B 2개에서 A, B, C, D, E 수준으로 Action값을 확장하여 좀 더 명확히 살펴보았습니다.

```
SimulatedAnnealing <- function(x){
  actions = c('A', 'B', 'C', 'D', 'E')
  counts = c(A = 0, B = 0, C = 0, D = 0, E = 0)
  mean = c(A = 1, B = 2, C = 3, D = 4, E = 5)
  sd = c(A = 0.5, B = 1, C = 1.5, D = 2, E = 2.5)
  pref = c(A = 0, B = 0, C = 0, D = 0, E = 0)
  mean_rewards = 0  
  learning_rate = 0.1 
  
  T = x
  for(t in 1:10000){
    e = exp(pref/T) # T값을 추가하여 모의담금질 시도
    p = e / sum(e)
    chosen = sample(actions, 1, prob=p)
    counts[chosen] = counts[chosen] + 1
    print(chosen)
    R = rnorm(1, mean[chosen], sd[chosen])
    advantage = R - mean_rewards
    pref[chosen] = pref[chosen] + learning_rate * advantage * (1 - p[chosen])
    nc = setdiff(actions, chosen)  
    pref[nc] = pref[nc] - learning_rate * advantage * p[nc]
  }
  print(counts)
  print(pref)
}

```

```
SimulatedAnnealing(10)      # A : 89,   B : 107,  C : 168,  D : 390,  E : 9246
```

```
SimulatedAnnealing(100)     # A : 503,  B : 593,  C : 859,  D : 1114, E : 6931
```

```
SimulatedAnnealing(1000)    # A : 1605, B : 1709, C : 2045, D : 2158, E : 2483
```

```
SimulatedAnnealing(10000)   # A : 1972, B : 2009, C : 1987, D : 1910, E : 2122
```

```
SimulatedAnnealing(100000)  # A : 2049, B : 1967, C : 1917, D : 2063, E : 2004
```

```
SimulatedAnnealing(1000000) # A : 2001, B : 2017, C : 2065, D : 1970, E : 1947
```

모의담금질의 이론내용을 바탕으로 T값을 조정하여 알고리즘을 시행해보니, T값이 높아질수록 action A, B, C, D, E 모두 비슷한 수준으로 맞춰지는 것을 직접 확인할 수 있었습니다.