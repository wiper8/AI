% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neuralnetwork.R
\name{neuralnetwork}
\alias{neuralnetwork}
\title{Initialize a neural network}
\usage{
neuralnetwork(
  formula,
  hidden = 0,
  startweights = NULL,
  linear.output = TRUE,
  activation_fun = sig,
  dactivation_fun = dsig
)
}
\arguments{
\item{formula}{formula}

\item{hidden}{numeric vector for the number of neurons per hidden layer.}

\item{startweights}{NULL, "zero" or list of matrices of the startweights.}

\item{linear.output}{logic : are the outputs linear or passed through the
activation_fun?}

\item{activation_fun}{function : activation function for neurons.}

\item{dactivation_fun}{function : derivative of the activation function.}
}
\value{
neural network (class : nn)
}
\description{
Initialize a neural network
}
\examples{
neuralnetwork(out~inputs, 1)
}
