#' Modified Hosmer-Lemeshow Test for Large Samples
#'
#' @param y,prob numeric vectors
#' @param glmObject numeric vectors
#' @param G numeric vectors
#'
#' @name hltestMain
NULL

#' @rdname hltestMain
hltest <- function(...) {
  UseMethod("hltest")
}

#' @rdname hltestMain
hltest.numeric <- function(y, prob, G=10) {
  z<-2*y
  j<-2*prob
  result <- paste0("numeric", sum(y*prob))
  return(result)
}

#' @rdname hltestMain
hltest.glm <- function(glmObject,...) {
  prob <- predict(glmObject, type = "response")
  y <- glmObject$y
  result <- hltest.numeric(y = y, prob = prob, ...)
  return(result)
}
