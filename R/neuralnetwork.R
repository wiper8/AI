#' @include backprop.R
NULL

#' Sigmoid function
#'
#' @param x numeric vector.
#'
#' @return sigmoized arguments.
#' @export
#' @examples
#' sig(c(3, 5, -2, 0))
#' sig(matrix(1:6, 2))
sig <- function(x) {
  1 / (1 + exp(-x))
}

#' Sigmoid derivative function
#'
#' @param x numeric vector.
#'
#' @return derivative of sigmoized arguments.
#' @export
#' @examples
#' dsig(c(0.2, 0.5, 0.7, 0.9))
#' dsig(matrix(1:6, 2))
dsig <- function(x) {
  x * (1 - x)
}

#' Rectified Linear Unit activation derivative function
#'
#' @param x numeric vector.
#'
#' @return ReLUed arguments.
#' @export
#' @examples
#' ReLU(c(3, 5, -2, 0))
#' ReLU(matrix(-2:3, 2))
ReLU <- function(x) {
  dim <- dim(x)
  matrix(pmax(0, x), dim[1])
}

#' Rectified Linear Unit activation function
#'
#' @param x numeric vector.
#'
#' @return derivative of ReLUed arguements.
#' @export
#' @examples
#' dReLU(c(3, 5, -2, 0))
#' dReLU(matrix(-2:3, 2))
dReLU <- function(x) {
  ifelse(x <= 0, 0, 1)
}

#' Leaky Rectified Linear Unit activation derivative function
#'
#' @param x numeric vector.
#'
#' @return Leaky ReLUed arguments.
#' @export
#' @examples
#' LReLU(c(3, 5, -2, 0))
#' LReLU(matrix(-2:3, 2))
LReLU <- function(x, a = 0.1) {
  dim <- dim(x)
  matrix(pmax(x * a, x), dim[1])
}

#' Leaky Rectified Linear Unit activation function
#'
#' @param x numeric vector.
#'
#' @return derivative of Leaky ReLUed arguements.
#' @export
#' @examples
#' dLReLU(c(3, 5, -2, 0))
#' dLReLU(matrix(-2:3, 2))
dLReLU <- function(x, a = 0.1) {
  ifelse(x <= 0, a, 1)
}


#' Initialize a neural network
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
#' neuralnetwork(out~inputs, 1)
neuralnetwork <- function(formula, hidden = 0, startweights = NULL,
                          linear.output = TRUE, activation_fun = sig,
                          dactivation_fun = dsig) {
  #TODO add colnames and rownames to weights matrices
  #TODO check col_gradient green  : make sure 0.5 is black and red and black is correct. also check if colors are at the right places.

  #initialiser liste du neural network
  nn <- list()
  nn$formula <- formula

  n_vars <- stringr::str_count(as.character(formula[-1]), " ") / 2 + 1


  #nombre de layer
  n_hidden_layer <- ifelse(any(hidden == 0), 0, length(hidden))
  n_layer <- n_hidden_layer + 1

  #initialiser liste des weights
  nn$weights <- list()

  #dimensions des layers
  weights_dims <- list()

  if(n_hidden_layer>0) {
    weights_dims[[1]] <- c(row=n_vars[2]+1, col=hidden[1])
    if(n_hidden_layer>1) for (i in 1:n_hidden_layer) weights_dims[[i+1]] <- c(row=hidden[i]+1, col=hidden[i+1])
    weights_dims[[n_layer]] <- c(row=tail(hidden, 1)+1, col=n_vars[1])

  } else {
    weights_dims[[1]] <- c(row=n_vars[2]+1, col=n_vars[1])
  }

  #initialiser les poids.
  if(is.null(startweights)) {
    for(i in 1:(n_layer)) nn$weights[[i]] <- matrix(stats::rnorm(prod(weights_dims[[i]])), nrow=weights_dims[[i]][1], ncol=weights_dims[[i]][2])
  } else if(is.list(startweights)) {
    for(i in 1:(n_layer)) nn$weights[[i]] <- matrix(startweights[[i]], nrow=weights_dims[[i]][1], ncol=weights_dims[[i]][2])
  } else if(startweights=="zero"){
    for(i in 1:(n_layer)) nn$weights[[i]] <- matrix(0, nrow=weights_dims[[i]][1], ncol=weights_dims[[i]][2])
  }


  nn$linear.output <- linear.output
  nn$n_hidden_layer <- n_hidden_layer
  nn$n_layer <- n_layer
  nn$activation_fun <- activation_fun
  nn$dactivation_fun <- dactivation_fun
  nn$nb_param <- sum(unlist(lapply(nn$weights, function(x) prod(dim(x)))))

  nn
}

