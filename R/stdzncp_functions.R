#Auziliary function to estimate the standardized noncentrality parameter
b <- function(sqrtLambda, dof, alpha) {
  return(uniroot(f = function(x) {
    (pchisq((sqrtLambda + x)^2, df = dof, ncp = sqrtLambda^2 ) -
       pchisq((max(c(sqrtLambda - x,0)))^2, df = dof, ncp = sqrtLambda^2)) - (1-alpha)
  }, lower = 0, upper = 10 * qchisq(1-alpha/2, df = dof, ncp = sqrtLambda^2))$root)
}

#Estimate the standardized noncentrality parameter, its confidence interval and
# compute the p-value of the proposed test
stdzNcp <- function(cHat, dof, n, alternative, epsilon0, conf.level, cimethod) {

  alpha <- 1 - conf.level

  #Point estimate NCP and stdz NCP
  lambdaHat <- max(c(cHat - dof, 0))
  epsilonHat <- sqrt(lambdaHat/n)

  #Reference for CI: Kent and Hainsworth (1995)
  #CI 1: two-sided CI
  if(alternative == "two.sided") {

    #CI 1.1: central probability interval (equal tails of nc chi-squared distrib)
    if(method== "central") {
      if(cHat < qchisq(1 - alpha/2, df = dof, ncp = 0) ) {
        lambdaLow_central <- 0
      } else {
        lambdaLow_central <- uniroot(f = function(x) {
          qchisq(1 - alpha/2, df = dof, ncp = x) - cHat
        },
        lower = 0,
        upper = cHat)$root
      }

      if(cHat < qchisq(alpha/2, df = dof, ncp = 0)) {
        lambdaUpp_central <- 0
      } else {
        lambdaUpp_central <- uniroot(f = function(x) {
          qchisq(alpha/2, df = dof, ncp = x) - cHat
        },
        lower = 0,
        upper = (10*sqrt(cHat) + qnorm(1-alpha/2))^2)$root
      }

      epsilonLow <- sqrt(lambdaLow_central/n)
      epsilonUpp <- sqrt(lambdaUpp_central/n)

    }

    #CI 1.2: symmetric range interval
    if(method == "symmetric") {

      if(sqrt(cHat) < largesamplehl:::b(0, dof, alpha)) {
        lambdaLow_symmetric <- 0
      } else {
        lambdaLow_symmetric <- (uniroot(f = function(x) {
          x + largesamplehl:::b(x, dof, alpha) - sqrt(cHat)
        },
        lower = 0,
        upper = sqrt(cHat))$root)^2
      }

      lambdaUpp_symmetric <- (uniroot(f = function(x) {
        max(c(x - largesamplehl:::b(x, dof, alpha), 0)) - sqrt(cHat)
      },
      lower = 0,
      upper = (10*sqrt(cHat) + qnorm(1-alpha/2)))$root)^2


      epsilonLow <- sqrt(lambdaLow_symmetric/n)
      epsilonUpp <- sqrt(lambdaUpp_symmetric/n)
    }
  }

  #CI 2: interval ONE-SIDED
  if(alternative == "greater") {

    if(cHat < qchisq(1 - alpha, df = dof, ncp = 0)) {

      lambdaLow_oneSided <- 0

    } else {

      lambdaLow_oneSided <- uniroot(f = function(x) {
        qchisq(1 - alpha, df = dof, ncp = x) - cHat
      },
      lower = 0,
      upper = cHat)$root

    }

    epsilonLow <- sqrt(lambdaLow_oneSided/n)
    epsilonUpp <- Inf

  }

  #p-value (if epsilon0=0, it's the traditional HL test)
  p.value <- 1 - pchisq(cHat, df = dof, ncp = epsilon0^2 * n)

  return(list(lambdaHat = lambdaHat,
              epsilonHat = epsilonHat,
              conf.int = c(epsilonLow = epsilonLow,
                           epsilonUpp = epsilonUpp),
              p.value = p.value))
}
