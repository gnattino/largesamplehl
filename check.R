#Generate data well calibrated model
set.seed(1234)

#Small sample
prob <- runif(1000)
y <- rbinom(1000, size = 1, prob = prob)

hltest(y, prob, G=10, outsample = T, epsilon0 = 0)
hltest(y, prob, G=10, outsample = T)

#Large sample
prob <- runif(1e6)
y <- rbinom(1e6, size = 1, prob = prob)

hltest(y, prob, G=10, outsample = T, epsilon0 = 0)
hltest(y, prob, G=10, outsample = T)


#Fit model
set.seed(1234)
dat <- data.frame(x1 = rnorm(5e5),
                  x2 = rbinom(5e5, size=1, prob=.5))
dat$prob <- 1/(1+exp(-(-1 + dat$x1 + dat$x2 + 0.5 * dat$x1*dat$x2)))
dat$y <- rbinom(5e5, size = 1, prob = dat$prob)

# modelCorrect <- glm(y ~ x1 + x2 + x1*x2, data = dat,
#              family = binomial(link="logit"))
#
# hltest(modelCorrect, epsilon0 = 0)
# hltest(modelCorrect)

modelBad <- glm(y ~ x1 + x2, data = dat,
             family = binomial(link="logit"))

hltest(modelBad, epsilon0 = 0)
hltest(modelBad)


set.seed(1234)
dat <- data.frame(x1 = rnorm(5e5),
                  x2 = rbinom(5e5, size=1, prob=.5))
dat$prob <- 1/(1+exp(-(-1 + dat$x1 + dat$x2 + 0.05 * dat$x1*dat$x2)))
dat$y <- rbinom(5e5, size = 1, prob = dat$prob)

# modelCorrect <- glm(y ~ x1 + x2 + x1*x2, data = dat,
#              family = binomial(link="logit"))
# hltest(modelCorrect, epsilon0 = 0)
# hltest(modelCorrect)

modelAcceptable <- glm(y ~ x1 + x2, data = dat,
                    family = binomial(link="logit"))
hltest(modelAcceptable, epsilon0 = 0)
hltest(modelAcceptable)

dat$phat <- predict(modelAcceptable, type = "response")
hist(abs(dat$prob-dat$phat))

hltest(y = dat$y, prob = dat$phat)

