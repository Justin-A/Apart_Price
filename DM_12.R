total <- 100000 # Trial
radius <- 1 # 원의 반지름

x <- runif(total) # X
y <- runif(total) # Y
dist <- x^2 + y^2 # Distance
hist(dist)
n <- sum(dist < radius) # count(Distance < radius)

n / total # Predict Area using Monte Carlo
pi / 4 # Real Area

# Visualize
plot(c(), c(), xlim = c(0, 1), ylim = c(0, 1))
points(x[1:10000], y[1:10000], col = ifelse(dist < radius, 2, 3))

# MC
total <- 100000
n <- 10
p <- 0.5

trial <- runif(total * n) < p # Check random number < p
m <- matrix(trial, ncol = 10) # 10 * (trial) Matrix
s <- rowSums(m)

sum(s <= 6) / total # < 6 Rate
pbinom(6, 10, 0.5) # Binary Distribution Proportion ( < 6)
hist(s)


# Cumulative Probability Distribution
total <- 100000
n <- 10
p <- 0.5

trial <- runif(total * n) < p # Check < p
m <- matrix(trial, ncol = 10) # 10 * (total) Matrix
s <- rowSums(m)

sum(s <= 6 ) / total # < 6 Rate
pbinom(6, 10, 0.5) # Binary Distribution Prediction ( < 6)
hist(s)

# Markov Chain Monte Carlo
x <- 0 # Start
n <- 0 
total <- 100000 # Trial
xs <- rep(0, total) # 0 0 0 0 0 0 ...

for (i in 1:total){
  #기존 값 대비 ±0.5 범위에서 후보 값을 정한다
  delta = runif(1) - 0.5
  x.new = x + delta
  
  # alpha (기존 값 - 후보 값)
  alpha = dnorm(x.new) / dnorm(x)
  
  # Stay or Next state
  # if alpha is bigger, go next state more
  x = ifelse(runif(1) < alpha,
             x.new,
             x)
  
  xs[i] = x
}

sum(xs < -2) / total # Cumulative Probability by MCMC
pnorm(-2) # Real Cumulative Probability
hist(xs)

# Bootstrap
data(iris)
x <- iris$Petal.Length

library(boot)
b <- boot(x, function(d, i){mean(d[i])}, R = 2000)
quantile(b$t, c(0.025, 0.975)) # 2.5% ~ 97.5%

# T-Distribution
n <- length(x)
q <- c(qt(0.025, df = n), qt(0.975, df = n))
mean(x) + q * sd(x) / sqrt(n)

# Iris
data(iris)
y <- iris$Petal.Length
x <- iris$Species

versicolor <- y[x == 'versicolor']
virginica <- y[x == 'virginica']

# Diff
diff <- mean(versicolor) - mean(virginica)

# t.test
t.test(versicolor, virginica)

# Bootstrap
library(boot)
n1 <- length(versicolor)
n2 <- length(virginica)
b <- boot(c(versicolor, virginica),
          function(x, i){
            g1 = i[1:n1]
            g2 = i[(n1+1):(n1+n2)]
            mean(x[g1]) - mean(x[g2])
          }, R = 1000)
b
quantile(b$t, c(0.025, 0.975))


# Tree Search
install.packages(c('dplyr', 'tidytext', 'janeaustenr'))
library(dplyr)
library(tidytext)
library(tidyr)
library(janeaustenr)

austen_bigrams <- austen_books() %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  count(bigram, sort = TRUE) %>%
  separate(bigram, c("word1", "word2"), sep = " ")
aus_bigrams

# 탐욕 알고리즘
start <- 'she' # Sentence 'she'
word <- start
words <- c()

for (i in 1:100){
  words <- c(words, word)
  rel <- austen_bigrams %>%
    filter(word1 == word) %>%
    filter(n == max(n))
  word <- rel$word2
}
words

# Concat Sentence
paste(words, collapse = ' ')

# Beam Search
beam.expand <- function(word, CLP, sentence){
  # word : Words List
  # CLP : Cumulative log probability
  # Sentence : Sentence before create
  
  # Explore Words
  d = auten_bigrams %>%
    filter(word1 == word)
  
  total = sum(d$n)
  
  # Select 3 Words
  d = d %>%
    arrange(desc(n)) %>%
    slice(1:3)
  
  # Log probability
  d$LP <- log(d$n / total)
  # Cumulative Log probability
  d$CLP <- CLP + d$LP
  
  d$sentence <- paste(sentence, d$word2)
  d
}
d$sentence[1]

# 순환신경망과 빔탐색을 이용하여 텍스트 예측

# Search
CLP <- 0
start <- 'she'
d <- beam.expand(start, 0, start)

for(j in 1:8){
  df = data.frame()
  
  # 3개의 후보 단어의 후보를 뽑아낸다
  for (i in 1:3){
    word <- d$wird2[i]
    CLP <- d$CLP[i]
    sentence <- d$sentence[i]
    branch = beam.expand(word, CLP, sentence)
    df = rbind(df, branch)
  }
  # 9개의 후보 중 CLP가 가장 높은 3개의 후보로 선정
  d <- df %>% arrange(CLP) %>% slice(1:3)
}

# 가장 확률값이 높은 문장
d$sentence[1]


# MCTS
uct.policy <- function(tree){
  node = 0
  for (i in 2:10){
    # Select
    children <- tree %>%
      filter(parent == node)
    
    # Stop
    if(nrow(children == 0){
      break
    }
    
    # Select which UCT is high
    selected <- children %>%
      mutate(uct = value + control * sqrt(log(1) / visit)) %>%
      arrange(desc(uct)) %>%
      slice(1)
    
    node <- selected$id
  }
  node
}

# Expand
mcts.expand <- function(word, CLP, parent){
  tree <- beam.expand(word, 0, '')
  tree = tree[c('word1', 'word2', 'CLP')]
  tree$value = 0
  tree$visit = 1
  tree$parent = parent
  tree
}

random.policy <- funcion(tree, node{
  depth <- tree$depth[node]
  
  for (i in depth:10){
    word <- tree$word2[node]
    CLP <_ tree$CLP[node]
    
    # Expand
    branch <- mcts.expand(word, CLP, parent = node)
    branch$id <- nrow(tree) + 1:3
    branch$depth <- i
    tree <- rbind(tree, branch)
    
    # 무작위 선택
    node <- sample(branch$id, 1)
  }
  tree
}

backprop <- function(tree{
  # 마지막 3개의 결과의 평균을 구한다.
  last = tail(as.numeric(rownames(tree)), 3)
  
  tree$value[last] = tree$CLP[last]
  R = sum(tree$value[last]) / 3
  parent <- tail(tree$parent, 1)
  
  # Policy
  while(parent != 0){
    n = tree$visit[parent]
    tree$visit[parent] <- n + 1
    Q = tree$value[parent]
    tree$value[parent] = Q + (R - Q) / n
    parnet <- tree$parent[parent]
  }
  tree
}

start <- 'she'
tree <- mcts.expand(start, 0, parent = 0)
tree$id <- 1:3
tree$depth <- 1
control <- 2

# Simulation 10
for (i in 1:10){
  node <- uct.policy(tree)
  tree <- random.policy(tree, node)
  tree <- backprop(tree)
}

# Select the best Result
best_end <- tree %>%
  filter(depth == 10) %>%
  filter(value == max(value))

# Find the Parent
nodes = c()
node <- as.numeric(best_end['id'])
tree$value[node]

while(node != 0){
  nodes = c(node, nodes)
  node <- tree$parent[node]
}

# 부모에서부터 끝까지 단어를 합쳐 문장을 만든다.
paste(c(start, tree$word2[nodes]), collapse = ' ')
