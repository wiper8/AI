#' @include backprop.R
NULL

#' Predict outputs of neural network
#'
#' @param nn nn object
#' @param newdata data.frame with at least the inputs variables as columns.
#'
#' @return matrix of the output neurons.
#' @export
#'
# @examples
predict.nn <- function(nn, newdata) {

  target_names <- attr(terms(as.formula(call("~", nn$formula[[2]]))), "term.labels")
  pred_names <- attr(terms(as.formula(call("~", nn$formula[[3]]))), "term.labels")

  #list of the activations in neurons
  activations <- activations(nn, input=as.matrix(stats::model.frame(as.formula(call("~", nn$formula[[3]])), newdata)))

  #prediction
  pred <- activations[[nn$n_layer]]


  colnames(pred) <- target_names

  pred
}
