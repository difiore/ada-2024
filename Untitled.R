library(mosaic)
reps <- 500
samp_dist_mean <-
  do(reps) * mean(rnorm(n = 10, mean = 10, sd = 2))
str(samp_dist_mean)

mean(samp_dist_mean$mean)
# true sem
2 / sqrt(10)

# sem from sampling dist
(se_mean <- sd(samp_dist_mean$mean))
(se_mean <- sqrt(var(samp_dist_mean$mean)))

# sem from single sample
one_sample <- rnorm(n = 10, mean = 10, sd = 2)
(sd(one_sample)/sqrt(10))

samp_dist_mean <-
  do(reps) * mean(rnorm(n = 1000, mean = 10, sd = 2))

histogram(~ mean, data = samp_dist_mean, xlab = "Samp Dist for the Mean")

(se_mean <- sd(samp_dist_mean$mean))
