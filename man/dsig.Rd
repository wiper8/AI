% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/neuralnetwork.R
\name{dsig}
\alias{dsig}
\title{Sigmoid derivative function}
\usage{
dsig(x)
}
\arguments{
\item{x}{numeric vector.}
}
\value{
derivative of sigmoized arguments.
}
\description{
Sigmoid derivative function
}
\examples{
dsig(c(0.2, 0.5, 0.7, 0.9))
dsig(matrix(1:6, 2))
}
