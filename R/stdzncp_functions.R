# Auxiliary function to estimate the standardized noncentrality parameter
b <- function(sqrtLambda, dof, alpha) {
  return(stats::uniroot(f = function(x) {
    (stats::pchisq((sqrtLambda + x)^2, df = dof, ncp = sqrtLambda^2 ) -
       stats::pchisq((max(c(sqrtLambda - x,0)))^2, df = dof, ncp = sqrtLambda^2)) - (1-alpha)
  }, lower = 0, upper = 10 * stats::qchisq(1-alpha/2, df = dof, ncp = sqrtLambda^2))$root)
}

# Estimate the standardized noncentrality parameter,
# its confidence interval and compute the p-value of the proposed test
stdzNcp <- function(cHat, dof, n, epsilon0, conf.level, citype, cimethod) {

  alpha <- 1 - conf.level

  #Point estimate NCP and stdz NCP
  lambdaHat <- max(c(cHat - dof, 0))
  epsilonHat <- sqrt(lambdaHat/n)

  #Reference for CI: Kent and Hainsworth (1995)
  #CI 1: two-sided CI
  if(citype == "two.sided") {

    #CI 1.1: central probability interval (equal tails of nc chi-squared distrib)
    if(cimethod== "central") {
      if(cHat < stats::qchisq(1 - alpha/2, df = dof, ncp = 0) ) {
        lambdaLow_central <- 0
      } else {
        lambdaLow_central <- stats::uniroot(f = function(x) {
          stats::qchisq(1 - alpha/2, df = dof, ncp = x) - cHat
        },
        lower = 0,
        upper = cHat)$root
      }

      if(cHat < stats::qchisq(alpha/2, df = dof, ncp = 0)) {
        lambdaUpp_central <- 0
      } else {
        lambdaUpp_central <- stats::uniroot(f = function(x) {
          stats::qchisq(alpha/2, df = dof, ncp = x) - cHat
        },
        lower = 0,
        upper = (10*sqrt(cHat) + stats::qnorm(1-alpha/2))^2)$root
      }

      epsilonLow <- sqrt(lambdaLow_central/n)
      epsilonUpp <- sqrt(lambdaUpp_central/n)

    }

    #CI 1.2: symmetric range interval
    if(cimethod == "symmetric") {

      if(sqrt(cHat) < b(0, dof, alpha)) {
        lambdaLow_symmetric <- 0
      } else {
        lambdaLow_symmetric <- (stats::uniroot(f = function(x) {
          x + b(x, dof, alpha) - sqrt(cHat)
        },
        lower = 0,
        upper = sqrt(cHat))$root)^2
      }

      lambdaUpp_symmetric <- (stats::uniroot(f = function(x) {
        max(c(x - b(x, dof, alpha), 0)) - sqrt(cHat)
      },
      lower = 0,
      upper = (10*sqrt(cHat) + stats::qnorm(1-alpha/2)))$root)^2


      epsilonLow <- sqrt(lambdaLow_symmetric/n)
      epsilonUpp <- sqrt(lambdaUpp_symmetric/n)
    }
  }

  #CI 2: interval ONE-SIDED
  if(citype == "one.sided") {

    if(cHat < stats::qchisq(1 - alpha, df = dof, ncp = 0)) {

      lambdaLow_oneSided <- 0

    } else {

      lambdaLow_oneSided <- stats::uniroot(f = function(x) {
        stats::qchisq(1 - alpha, df = dof, ncp = x) - cHat
      },
      lower = 0,
      upper = cHat)$root

    }

    epsilonLow <- sqrt(lambdaLow_oneSided/n)
    epsilonUpp <- Inf

  }

  #p-value (if epsilon0=0, traditional HL test)
  p.value <- 1 - stats::pchisq(cHat, df = dof, ncp = epsilon0^2 * n)

  return(list(lambdaHat = lambdaHat,
              epsilonHat = epsilonHat,
              conf.int = c(epsilonLow = epsilonLow,
                           epsilonUpp = epsilonUpp),
              p.value = p.value))
}
