# Check inputs
checkInputs <- function(y, prob, G, outsample, epsilon0, conf.level, citype, cimethod){

  if( ! "numeric" %in% class(prob) ) {

    stop("The vector 'prob' must be a vector of numeric probabilities")

    } else {


      if(length(y)!=length(prob)) {
        stop("The vectors 'y' and 'prob' must have the same length")
      }

      if(!all(y %in% c(0, 1, NA))) {
        stop("The vector 'y' must be a numeric vector with values 1 (events) and 0 (non-events)")
      }

      if(!all(prob <= 1 & prob >= 0, na.rm = T)) {
        stop("The predicted probabilities ('prob') must be values between 0 and 1")
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

  # Check that quantiles are unique. If not, take unique values
  breaks_guess <- stats::quantile(prob,
                                  probs = seq(0, 1, 1/G),
                                  na.rm = TRUE)

  #Make sure that there are not repeated values
  if(length(unique(breaks_guess)) < length(breaks_guess)) {

    breaks <- unique(breaks_guess)

    #Throw warning
    warning("The quantiles of the predicted probabilities are not unique with G=", G, ". ",
            "The test is applied with G=", length(breaks) - 1, " groups.")

    #Replace used value for G
    G <- (length(breaks) - 1)

  } else {

    breaks <- breaks_guess

  }

  #If G < 3, throw error
  if(G < 3) {

    stop("The number of groups must be >= 3.")

  }

  #Define degrees of freedom
  dof <- ifelse(outsample == T, G, G-2)


  #If default epsilon0 (NULL value), set
  if(is.null(epsilon0)) {

    epsilon0 <- sqrt((stats::qchisq(.95, df = dof) - dof)/1e6)

  }


  return(list(breaks = breaks,
              G = G,
              dof = dof,
              epsilon0 = epsilon0))

}
