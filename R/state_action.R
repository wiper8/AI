#' @include backprop.R
#' @include neuralnetwork.R
NULL

#' Create state of an object
#'
#' @param formula formula of the parameters that counts as state. Only right part with ~ is needed.
#' @param object data.frame or list of the object
#'
#' @return data.frame of the variable used for state.
#' @export
#'
#' @examples state(formula=neuralnetwork(x+y~a+b, hidden=2)$formula, object=list(x=3, y=5, a=3, b=8))
state <- function(formula, object) {
  if(length(formula)==2) {
    return(stats::model.frame(formula[c(1, 2)], object))
  }
  else if(length(formula)==3){
    return(stats::model.frame(formula[c(1, 3)], object))
  } else {
    stop("Problem in formula subset.")
  }
}

#' Choose taken action according to a given state
#'
#' @param policy nn
#' @param state data.frame or list (usually from the 'state' function)
#' @param ... optional : additional parameters for limit_FUN
#'
#' @return data.frame of the actions.
#' @export
#' @examples action(neuralnetwork(x+y~a+b, hidden=2),
#'                  state(neuralnetwork(x+y~a+b, hidden=2)$formula,
#'                  data.frame(x=2, b=5, a=8, y=4)))
action <- function(policy, state, ...) {
  as.data.frame(predict.nn(policy, state))
}
