#Generate data with by-construction poorly calibrated model
set.seed(123456)
prob <- runif(1000)
y <- rbinom(1000, size = 1, prob = logistic(logit(e)+2))
