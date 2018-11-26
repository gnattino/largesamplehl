#' Modified Hosmer-Lemeshow Test for Large Samples
#'
#' \code{hltest} implements a goodness-of-fit test to assess the goodness of fit of
#' logistic regression models in large samples.
#'
#' @param y,prob Numeric vectors with binary responses and predicted probabilities to be evaluated.
#' The vectors must have equal length. Missing values are dropped.
#' @param glmObject In alternative to the vectors \code{y} and \code{prob}, it is possible to
#' provide the \code{glm} object with the model to be evaluated.
#' @param G Number of groups to be used in the Hosmer-Lemeshow statistic.
#' @param outsample A boolean specifying whether the model has been fit on the data provided
#' (\code{outsample=FALSE}, default) or if the model has been developed on an external sample
#' (\code{outsample=TRUE}). The distribution of the Hosmer-Lemeshow
#' statistic is assumed to have \code{G-2} and \code{G} degrees of freedom if \code{outsample=FALSE} and
#' \code{outsample=TRUE}, respectively.
#' @param epsilon0 Value of the parameter epsilon0 (add details).
#' @param conf.level Confidence level for the confidence interval of epsilon. Equal to \code{.95}
#' by default.
#' @param citype Type of confidence interval of epsilon to be computed: one-sided
#' (\code{citype="one.sided"}, default) or two-sided
#' (\code{citype="two.sided"}).
#' @param cimethod Method to be used to compute the two-sided confidence interval:
#' symmetric (\code{cimethod="symmetric"}, default) or central
#' (\code{cimethod="central"}). See section "Details" for further information.
#' @param ... Additional (ignored) arguments.
#'
#' @return A list of class \code{htest} containing the following components:
#' \describe{
#'   \item{null.value}{The value of epsilon0 used in the test.}
#'   \item{statistic}{The value of the Hosmer-Lemeshow statistic.}
#'   \item{p.value}{The p-value of the test.}
#'   \item{parameter}{A vector with the parameters of the noncentral chi-squared distribution used to
#'                    compute the p-value: degrees of freedom (\code{dof}) and noncentrality
#'                    parameter (\code{lambda}).}
#'   \item{lambdaHat}{The estimate of noncentrality parameter lambda.}
#'   \item{estimate}{The estimate of epsilon.}
#'   \item{conf.int}{The confidence interval of epsilon.}
#' }
#'
#' @details Some details soon.
#'
#' @examples
#' #Generate fake data with two variables: one continuous and one binary.
#' set.seed(1234)
#' dat <- data.frame(x1 = rnorm(5e5),
#'                  x2 = rbinom(5e5, size=1, prob=.5))
#' #The true probabilities of the response depend on a negligible interaction
#' dat$prob <- 1/(1+exp(-(-1 + dat$x1 + dat$x2 + 0.05*dat$x1*dat$x2)))
#' dat$y <- rbinom(5e5, size = 1, prob = dat$prob)
#'
#' #Fit an acceptable model (does not include the negligible interaction)
#' model <- glm(y ~ x1 + x2, data = dat, family = binomial(link="logit"))
#'
#' #Check: predicted probabilities are very close to true probabilities
#' dat$phat <- predict(model, type = "response")
#' boxplot(abs(dat$prob-dat$phat))
#'
#' #Traditional Hosmer-Lemeshow test: reject H0
#' hltest(model, epsilon0 = 0)
#'
#' #Modified Hosmer-Lemeshow test: fail to reject H0
#' hltest(model)
#'
#' #Same output with vectors of responses and predicted probabilities
#' hltest(y=dat$y, prob=dat$phat)
#'
#' @export
hltest <- function(...) {
  UseMethod("hltest")
}

#' @describeIn hltest Method for vectors of responses and predicted probabilities.
#' @export
hltest.numeric <- function(y, prob, G=10, outsample=FALSE,
                           epsilon0 = sqrt((stats::qchisq(.95, df = (G-2)) - (G-2))/1e6),
                           conf.level = 0.95,
                           citype = "one.sided",
                           cimethod = ifelse(citype == "one.sided", NULL, "symmetric"),
                           ...) {

  checkInputs(y, prob, G, outsample, epsilon0, conf.level, citype, cimethod)

  #Remove missing
  indexesMissing <- (is.na(y) | is.na(prob))
  y <- y[!indexesMissing]
  prob <- prob[!indexesMissing]

  #Sample size
  n <- length(y)

  #Compute HL statistic
  cHat <- hlstat(y, prob, G)
  dof <- ifelse(outsample==T, G, G-2)

  resultStdzNcp <- stdzNcp(cHat, dof, n, epsilon0, conf.level, citype, cimethod)

  #The value of the population parameter specified by the null hypothesis
  null.value <- epsilon0
  names(null.value) <- "epsilon"

  #The value of the estimated population parameter involved in the null hypothesis.
  estimate <- resultStdzNcp$epsilonHat
  names(estimate) <- "epsilonHat"

  #Lambda hat (just want to return this, not required for htest)
  lambdaHat <- resultStdzNcp$lambdaHat
  names(lambdaHat) <- "lambdaHat"

  #Confidence interval (optional)
  conf.int <- resultStdzNcp$conf.int
  attr(conf.int, "conf.level") <- conf.level

  #Numeric vector with parameter(s) associated with the null distribution of the test statistic
  parameter <- c(dof, epsilon0^2*n)
  names(parameter)<-c("dof","lambda")

  #The value of the statistic
  statistic <- cHat
  names(statistic) <- "HL statistic"

  #P-value
  p.value <- resultStdzNcp$p.value

  if(epsilon0 == 0) {
    method <- c("Traditional Hosmer-Lemeshow test",
                "H0: epsilon=0 vs. Ha: epsilon>0")
  } else {
    method <- c("Modified Hosmer-Lemeshow test for large samples",
                "H0: epsilon<=epsilon0 vs. Ha: epsilon>epsilon0",
                paste0(ifelse(outsample==T, "Out-of-sample", "In-sample"),
                       " goodness of fit."))
  }

  data.name <- paste0("Model evaluated on ", n," observations")

  outputTest <- list(null.value = null.value,
                     alternative = "greater", #the test is always one-sided
                     method = method,
                     estimate = estimate,
                     lambdaHat = lambdaHat,
                     data.name = data.name,
                     parameter = parameter,
                     statistic = statistic,
                     p.value = p.value,
                     conf.int = conf.int)

  class(outputTest) <- "htest"

  return(outputTest)
}


#' @describeIn hltest Method for result of \code{glm} fit.
#' @export
hltest.glm <- function(glmObject,...) {

  #Extract predicted probabilities and response from glmObject
  prob <- stats::predict(glmObject, type = "response")
  y <- glmObject$y

  result <- hltest.numeric(y = y, prob = prob, ...)

  return(result)
}

# Compute Hosmer-Lemeshow Statistic
hlstat <- function(y, prob, G) {

  cutresult <- cut(prob, breaks = stats::quantile(prob, probs = seq(0, 1, 1/G)),
                   include.lowest=TRUE)

  obs <- stats::xtabs(cbind(1-y, y) ~ cutresult)
  exp <- stats::xtabs(cbind(1-prob, prob) ~ cutresult)

  output <- sum((obs-exp)^2/exp)

  return(output)
}

