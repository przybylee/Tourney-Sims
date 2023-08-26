#' A logistic function to get binomial probability team 1 wins
#'
#' @param a1 Linear value of team 1
#' @param a2 Linear value of team 2
#'
#' @return The probability that team 1 wins according to a logistic model
#' @export)
#'
#' @examples
#' logistic(1, 0)
#' 
logistic <- function(a1 = 0, a2 = 0){
  p <- 1/(1+ exp(a2 - a1))
  return(p)
}

##Comment to test git