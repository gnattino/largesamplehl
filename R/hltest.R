#' Modified Hosmer-Lemeshow Test for Large Samples
#'
#' @param y,prob numeric vectors
#' @param glmObject numeric vectors
#' @param G numeric vectors
#' @param outsample numeric vectors
#' @param conf.level numeric vectors
#' @param alternative numeric vectors
#' @param cimethod numeric vectors
#' @param epsilon0 numeric vectors
#'
#' @return A list of class \code{htest} containing the following components:
#' \describe{
#'   \item{statistic}{The value of the test's statistic.}
#'   \item{p.value}{The p-value of the test.}
#'   \item{null.value}{The vector of coefficients hypothesized under the null hypothesis,
#'                     that is, the parameters corresponding to the bisector.}
#'   \item{alternative}{A character string describing the alternative hypothesis.}
#'   \item{method}{A character string indicating what type of calibration
#'                 test (internal or external) was performed.}
#'   \item{estimate}{The estimate of the coefficients of the polynomial logistic
#'                    regression.}
#'   \item{data.name}{A character string giving the name(s) of the data.}
#' }
#'
#' @name hltestMain
NULL

#' @rdname hltestMain
hltest <- function(...) {
  UseMethod("hltest")
}

#' @rdname hltestMain
hltest.numeric <- function(y, prob, G=10, outsample=FALSE,
                           alternative = "greater",
                           epsilon0 = sqrt((stats::qchisq(.95, df = (G-2)) - (G-2))/1e6),
                           conf.level = 0.95,
                           cimethod = ifelse(alternative == "greater", NULL, "symmetric")) {

  checkInputs(y, prob, G, outsample, conf.level, alternative, cimethod, epsilon0)

  #Remove missing
  indexesMissing <- (is.na(y) | is.na(prob))
  y <- y[!indexesMissing]
  prob <- prob[!indexesMissing]

  #Sample size
  n <- length(y)

  #Compute HL statistic
  cHat <- hlstat(y, prob, G)
  dof <- ifelse(outsample==T, G, G-2)

  resultStdzNcp <- largesamplehl:::stdzNcp(cHat, dof, n, alternative,
                                           epsilon0, conf.level, cimethod)

  #The value of the population parameter specified by the null hypothesis
  null.value <- epsilon0
  names(null.value) <- "epsilon0"

  #The value of the estimated population parameter involved in the null hypothesis.
  estimate <- resultStdzNcp$epsilonHat
  names(estimate) <- "epsilonHat"

  #Lambda hat (just want to return this, not required for htest)
  lambdaHat <- resultStdzNcp$lambdaHat
  names(lambdaHat) <- "lambdaHat"

  #Confidence interval (optional)
  conf.int <- resultStdzNcp$conf.int

  #Numeric vector with parameter(s) associated with the null distribution of the test statistic
  parameter <- c(dof, epsilon0^2*n)
  names(parameter)<-c("dof","lambda")

  #The value of the statistic
  statistic <- cHat
  names(statistic) <- "HL statistic"

  #P-value
  p.value <- resultStdzNcp$p.value

  if(epsilon0 == 0) {
    method <- "Traditional Hosmer-Lemeshow test - H0: epsilon=0 vs. Ha: epsilon>0"
  } else {
    method <- "Modified Hosmer-Lemeshow test for large samples - H0: epsilon<=epsilon0 vs. Ha: epsilon>epsilon0"
  }

  outputTest <- list(null.value = null.value,
                     alternative = alternative,
                     method = method,
                     estimate = estimate,
                     lambdaHat = lambdaHat,
                     data.name = "this is the data.name",
                     parameter = parameter,
                     statistic = statistic,
                     p.value = p.value,
                     conf.int)

  class(outputTest) <- "htest"

  return(outputTest)
}

#' @rdname hltestMain
hltest.glm <- function(glmObject,...) {

  #Extract predicted probabilities and response from glmObject
  prob <- predict(glmObject, type = "response")
  y <- glmObject$y

  result <- hltest.numeric(y = y, prob = prob, ...)

  return(result)
}

# Compute Hosmer-Lemeshow Statistic
hlstat <- function(y, prob, G) {

  cutresult <- cut(prob, breaks = quantile(prob, probs = seq(0, 1, 1/G)),
                 include.lowest=TRUE)

  obs <- xtabs(cbind(1-y, y) ~ cutresult)
  exp <- xtabs(cbind(1-prob, prob) ~ cutresult)

  output <- sum((obs-exp)^2/exp)

  return(output)
}




