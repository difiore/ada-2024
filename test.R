p <- ggplot(
  data = d,
  aes(log(Body_mass_female_mean))
)

(p + geom_histogram(bins = 9, aes(y = ..density..)) +
                      geom_density())
  
(p + geom_histogram(bins = 9, aes(y = ..density..)) +
                      geom_density() +
                      geom_vline(xintercept =
                                   quantile(
                                     log(d$Body_mass_female_mean),
                                     prob = c(0.05, 0.95),
                                     na.rm = TRUE
                                   ))
)
  
(p + geom_histogram(bins = 9, aes(y = ..density..)) +
    geom_density() +
    geom_vline(xintercept =
                 quantile(
                   log(d$Body_mass_female_mean),
                   prob = c(0.05, 0.95),
                   na.rm = TRUE
                 ),
               color = "red")
)

hist(log(d$Body_mass_female_mean), freq = FALSE)

lines(
  density(log(d$Body_mass_female_mean), na.rm = TRUE),
  col = "blue"
)

abline(v = quantile(
  log(d$Body_mass_female_mean),
  prob = c(0.05, 0.95),
  na.rm = TRUE
),
col = "red")
p <- ggplot(data = d, aes(
  x = log(Body_mass_female_mean),
  y = log(Brain_Size_Female_Mean)
)
)

p <- p + geom_point(na.rm = TRUE)
# new aesthetic

p <- ggplot(data = d, aes(
  x = log(Body_mass_female_mean),
  y = log(Brain_Size_Female_Mean)
))
p <- p + geom_point(aes(color = factor(Family)), na.rm = TRUE) # scatterplot
# modify the axis labels
p <- p + xlab("log(Female Body Mass)") +
  ylab("log(Female Brain Size)")
# modify the legend
p <- p + theme(legend.position = "bottom", legend.title = element_blank())
# plot the object
p
(p <- p + geom_smooth(data = d,
  aes(
    x = log(Body_mass_female_mean),
    y = log(Brain_Size_Female_Mean)),
  method = "lm", na.rm = TRUE))

p <- ggplot(data = d, aes(
  x = log(Body_mass_female_mean),
  y = log(Brain_Size_Female_Mean)
)
)

p <- p + geom_point(aes(color = factor(Family)),
                    na.rm = TRUE)

# modify axes
p <- p + xlab("log(Female Body Mass)") + # add xlabel
  ylab("log(Female Brain Size)") # add ylabel
  
# add legend
  p <- p + theme(legend.position = "bottom",
                 legend.title = element_blank()
  )

# plot the object
p

(p <- p + geom_smooth(method = "lm", na.rm = TRUE))

s <- d[, c("Family", "Genus", "Body_mass_male_mean")]

s <- d[order(d$Family, d$Genus, -d$Body_mass_male_mean),]
s <- arrange(d, Family, Genus, desc(Body_mass_male_mean))
s <- group_by(d, Family) %>% summarise(
  n_cases = n(),
  avgF = mean(Body_mass_female_mean, na.rm = TRUE),
  avgM = mean(Body_mass_male_mean, na.rm = TRUE))

s <- aggregate(d$Body_mass_female_mean ~ d$Family, FUN = "mean", na.rm = TRUE)
s <- summarise(group_by(d, Family),
               avgF = mean(Body_mass_female_mean, na.rm = TRUE))
