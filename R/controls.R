# Chek inputs
checkInputs <- function(y, prob, G, outsample, epsilon0, conf.level, citype, cimethod){

  if( ! "numeric" %in% class(prob) ) {

    stop("The vector 'prob' must be a vector of numeric probabilities")

    } else {


      if(length(y)!=length(prob)) {
        stop("The vectors 'y' and 'prob' must have the same length")
      }

      if(!all(y %in% c(0,1))) {
        stop("The vector 'y' must be a numeric vector with values 1 (events) and 0 (non-events)")
      }

  }


  if(! citype %in% c("two.sided", "one.sided") ) {
    stop("'citype' must be either 'two.sided' or 'one.sided'")
  }
  if(citype == "two.sided") {
    if(is.null(cimethod)) {
      stop("When 'citype' is 'two.sided', 'cimethod' must be 'central' or 'symmetric'")
    }
    if(! cimethod %in% c("two.sided", "one.sided")) {
      stop("When 'citype' is 'two.sided', 'cimethod' must be 'central' or 'symmetric'")
    }
  }

  return(NULL)

}
