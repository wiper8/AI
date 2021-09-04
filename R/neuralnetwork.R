#' @include backprop.R

#' Initialize a neural network
#'
#'
#' @title neuralnetwork
#'
#' @param formula formula
#' @param hidden numeric vector for the number of neurons per hidden layer.
#' @param startweights NULL, "zero" or list of matrices of the startweights.
#' @param linear.output logic : are the outputs linear or passed through the
#'   activation_fun?
#' @param activation_fun function : activation function for neurons.
#' @param dactivation_fun function : derivative of the activation function.
#'
#' @return neural network (class : nn)
#' @export
#'
#' @examples
#' neuralnetwork(out~in, 1)
neuralnetwork <- function(formula, hidden = 0, startweights = NULL,
                          linear.output = TRUE, activation_fun = sig,
                          dactivation_fun = dsig) {
  #TODO add colnames and rownames to weights matrices
  #TODO check col_gradient green  : make sure 0.5 is black and red and black is correct. also check if colors are at the right places.

  #initialiser liste du neural network
  nn <- list()
  nn$formula <- formula

  n_vars <- stringr::str_count(as.character(formula[-1]), " ")/2+1


  #nombre de layer
  n_hidden_layer <- ifelse(any(hidden == 0), 0, length(hidden))
  n_layer <- n_hidden_layer + 2

  #initialiser liste des weights
  nn$weights <- list()

  #dimensions des layers
  weights_dims <- list()

  if(n_hidden_layer>0) {
    weights_dims[[1]] <- c(row=n_vars[2]+1, col=hidden[1])
    if(n_hidden_layer>1) for (i in 1:n_hidden_layer) weights_dims[[i+1]] <- c(row=hidden[i]+1, col=hidden[i+1])
    weights_dims[[n_layer-1]] <- c(row=tail(hidden, 1)+1, col=n_vars[1])

  } else {
    weights_dims[[1]] <- c(row=n_vars[2]+1, col=n_vars[1])
  }

  #initialiser les poids.
  if(is.null(startweights)) {
    for(i in 1:(n_layer-1)) nn$weights[[i]] <- matrix(stats::rnorm(prod(weights_dims[[i]])), nrow=weights_dims[[i]][1], ncol=weights_dims[[i]][2])
  } else if(is.list(startweights)) {
    for(i in 1:(n_layer-1)) nn$weights[[i]] <- matrix(startweights[[i]], nrow=weights_dims[[i]][1], ncol=weights_dims[[i]][2])
  } else if(startweights=="zero"){
    for(i in 1:(n_layer-1)) nn$weights[[i]] <- matrix(0, nrow=weights_dims[[i]][1], ncol=weights_dims[[i]][2])
  }


  nn$linear.output <- linear.output
  nn$n_hidden_layer <- n_hidden_layer
  nn$n_layer <- n_layer
  nn$activation_fun <- activation_fun
  nn$dactivation_fun <- dactivation_fun

  nn
}

