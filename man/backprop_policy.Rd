% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/backprop.R
\name{backprop_policy}
\alias{backprop_policy}
\title{Backpropagate data on a neural network}
\usage{
backprop_policy(
  policy_nn,
  critic_nn,
  newdata,
  n_epoch = 1,
  step_size = 0.1,
  batch_size = ceiling(nrow(newdata)/10),
  trace = FALSE
)
}
\arguments{
\item{policy_nn}{policy neural network.}

\item{critic_nn}{critic neural network.}

\item{newdata}{data.frame of the inputs from policy and the outputs wanted from critic.}

\item{n_epoch}{integer : number of iterations of backpropagation going
through every data once.}

\item{step_size}{numeric : multiplier of the gradient.}

\item{batch_size}{integer : size of batch of backpropagation.}

\item{trace}{logical : printing last ran epoch?}
}
\value{
list of neural network (class : nn) and the mean Loss
}
\description{
Backpropagate data on a neural network
}
