library(tidyverse)
library(BEST)
f <- "/Users/ad26693/Downloads/roubik_2002_coffe_yield.csv"
d <- read_csv(f, col_names = TRUE)
new_yield_80 <- d$yield_61_to_80[d$world == "new"]
new_yield_01 <- d$yield_81_to_01[d$world == "new"]
t.test(new_yield_01, new_yield_80, paired = TRUE, alternative = "greater")
library(BayesianFirstAid)
fit<-bayes.t.test(new_yield_01, new_yield_80, paired = TRUE)
plot(fit)
summary(fit)
diagnostics(fit)
model.code(fit)

methods(bayes.t.test)
getAnywhere(bayes.t.test.default())


# chapter 3 problems
# Easy
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 6 , size=9 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample( p_grid , prob=posterior , size= 10000 , replace=TRUE )
hist(samples)
sum(samples < 0.2)/length(samples)
sum(samples > 0.8)/length(samples)
sum(samples < 0.8  & samples > 0.2)/length(samples)
quantile(samples, 0.2)
1-quantile(samples, 0.8)

library(rethinking)
HPDI( samples , prob=0.66 )
PI( samples , prob=0.66 )

# Medium
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- rep( 1 , 1000 )
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
plot(posterior ~ p_grid, type = "l")
set.seed(100)
samples <- sample( p_grid , prob=posterior , size= 10000 , replace=TRUE )
HPDI( samples , prob=0.9 )

w <- rbinom(10000, 15, samples)
simplehist(w)
sum(w == 8) / length(w)


w <- rbinom(10000, 9, samples)
simplehist(w)
sum(w == 6) / length(w)

# 3.5
p_grid <- seq( from=0 , to=1 , length.out=1000 )
prior <- ifelse(p_grid < 0.5, 0, 1)
likelihood <- dbinom( 8 , size=15 , prob=p_grid )
posterior <- likelihood * prior
posterior <- posterior / sum(posterior)
set.seed(100)
samples <- sample(p_grid , prob=posterior , size= 10000 , replace=TRUE)
plot( posterior ~ p_grid , type="l" )
hist(samples)
sum(samples < 0.2)/length(samples)
sum(samples > 0.8)/length(samples)
sum(samples < 0.8  & samples > 0.2)/length(samples)
quantile(samples, 0.2)
1-quantile(samples, 0.8)

library(rethinking)
HPDI( samples , prob=0.66 )
PI( samples , prob=0.66 )

w <- rbinom(10000, 15, samples)
simplehist(w)
sum(w == 8) / length(w)


# Hard
birth1 <- c(1,0,0,0,1,1,0,1,0,1,0,0,1,1,0,1,1,0,0,0,1,0,0,0,1,0, 0,0,0,1,1,1,0,1,0,1,1,1,0,1,0,1,1,0,1,0,0,1,1,0,1,0,0,0,0,0,0,0,1,1,0,1,0,0,1,0,0,0,1,0,0,1,1,1,1,0,1,0,1,1,1,1,1,0,0,1,0,1,1,0,1,0,1,1,1,0,1,1,1,1)
birth2 <- c(0,1,0,1,0,1,1,1,0,0,1,1,1,1,1,0,0,1,1,1,0,0,1,1,1,0,1,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,1,1,0,1,1,0,1,1,1,0,0,0,0,0,0,1,0,0,0,1,1,0,0,1,0,0,1,1,0,0,0,1,1,1,0,0,0,0)

# 3H1. Using grid approximation, compute the posterior distribution for the probability of a birth being a boy. Assume a uniform prior probability. Which parameter value maximizes the posterior probability?
p_grid <- seq( from=0 , to=1 , length.out=1000 )

prior <- rep(0.5, 1000)
sum(birth1) + sum(birth2)
length(birth1) + length(birth2)

likelihood <- dbinom(sum(birth1) + sum(birth2), size= length(birth1) + length(birth2), prob=p_grid)

posterior <- likelihood * prior
plot( posterior ~ p_grid , type="l" )
# value with max posterior prob
p_grid[ which.max(posterior) ]


# 3H2. Using the sample function, draw 10,000 random parameter values from the posterior distribution you calculated above. Use these samples to estimate the 50%, 89%, and 97% highest posterior density intervals

samples <- sample(p_grid , prob=posterior , size= 10000 , replace=TRUE)

HPDI(samples, 0.5)
HPDI(samples, 0.89)
HPDI(samples, 0.97)
