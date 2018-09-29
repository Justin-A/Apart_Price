수업시간 내 몬테 카를로 법은 4등분한 원의 외접인 정사각형과 비교해서 원주율을 구하는 방식으로 진행했습니다. 저희는 이 부분을 동전던지는 것과, 주사위를 던지는 것이 시뮬레이션을 많이 할수록 동일한 확률을 갖게 되는지 확인해보았습니다. 동전던지기는 toss_coins, 주사위던지기는 toss_dice 로 정의하였고, sample 함수를 통해 난수를 생성시킨 뒤, 시도횟수가 증가할 수록 어떻게 값이 나오는지 확인해 보았습니다.


# 동전던지기
```
toss_coins <- function(n_toss){
  experiment <- sample(c("H", "T"), n_toss, replace = TRUE)
  Head <- sum(experiment == "H") / n_toss
  Tail <- sum(experiment == "T") / n_toss
  df <- data.frame(Head, Tail)
  return(df)
}
```
# 실험
```
toss_coins(10)
toss_coins(100)
toss_coins(1000)
toss_coins(10000)
toss_coins(100000)
toss_coins(1000000)
toss_coins(10000000)
```

# 결과
> toss_coins(10)
> Head Tail
> 1  0.3  0.7
> toss_coins(100)
> Head Tail
> 1 0.49 0.51
> toss_coins(1000)
>  Head  Tail
> 1 0.505 0.495
> toss_coins(10000)
>   Head   Tail
> 1 0.5042 0.4958
> toss_coins(100000)
>   Head   Tail
> 1 0.4993 0.5007
> toss_coins(1000000)
>     Head     Tail
> 1 0.500166 0.499834
> toss_coins(10000000)
>      Head      Tail
> 1 0.5000134 0.4999866

# 시도 횟수가 증가할 수록, 동전을 던졌을 때 앞면과 뒷면이 0.5로 수렴하는 것을 확인할 수 있었습니다.


# 주사위 던지기
```
toss_dice <- function(n_toss){
  experiment <- sample(c(1, 2, 3, 4, 5, 6), n_toss, replace = TRUE)
  One <- sum(experiment == 1) / n_toss
  Two <- sum(experiment == 2) / n_toss
  Three <- sum(experiment == 3) / n_toss
  Four <- sum(experiment == 4) / n_toss
  Five <- sum(experiment == 5) / n_toss
  Six <- sum(experiment == 6) / n_toss
  df <- data.frame(One, Two, Three, Four, Five, Six)
  return(df)
}
```

# 실험
```
toss_dice(10)
toss_dice(100)
toss_dice(1000)
toss_dice(10000)
toss_dice(100000)
toss_dice(1000000)
toss_dice(10000000)
```

# 결과

> toss_dice(10)
> One Two Three Four Five Six
> 1 0.2 0.3   0.1  0.2    0 0.2
> toss_dice(100)
>  One  Two Three Four Five  Six
> 1 0.09 0.22  0.16  0.2 0.22 0.11
> toss_dice(1000)
>   One   Two Three  Four Five   Six
> 1 0.171 0.149 0.171 0.172 0.17 0.167
> toss_dice(10000)
>    One    Two  Three   Four  Five    Six
> 1 0.1612 0.1692 0.1678 0.1669 0.171 0.1639
> toss_dice(100000)
>     One     Two   Three    Four    Five     Six
> 1 0.16686 0.16685 0.16743 0.16678 0.16595 0.16613
> toss_dice(1000000)
>      One      Two    Three     Four     Five      Six
> 1 0.167226 0.166482 0.166665 0.166744 0.166468 0.166415
> toss_dice(10000000)
>       One       Two     Three      Four     Five       Six
> 1 0.1665732 0.1667096 0.1665237 0.1665663 0.166899 0.1667282

# 시도 횟수가 증가할 수록, 주사위를 던졌을 때 1부터 6까지 나오는 확률값이 0.16666.. 으로 수렴하는 것을 확인할 수 있었습니다.