\name{weighted mode}
\alias{weighted_mode}

\title{Weighted mode}

\description{
Calculate the weighted mode of a vector.
}

\usage{
weighted_mode(x, w = rep(1, length(x)), na.rm = FALSE)
}

\arguments{
\item{x}{An atomic vector.}

\item{w}{A numeric vector of weights, the same length as \code{x}. The default gives each element of \code{x} equal weight.}

\item{na.rm}{Should missing values in \code{x} be removed?}
}

\value{
A vector of the same type as \code{x} that gives the mode(s).
}

\details{
Returns the value(s) in \code{x} with the largest weight, which by default is the most frequently occurring values in \code{x}. This is equivalent to choosing a value \eqn{m}{m} that minimizes \eqn{\sum_{i = 1}^{n} w_{i} I[x_{i} \neq m]}{\sum w[x != m]}.

The mode need not be unique, and a warning is given in this case.
}

\examples{
# Unique mode
weighted_mode(c(1, 3, 2, 1))
weighted_mode(c(TRUE, FALSE, TRUE, FALSE), 1:4)

# Two modes
weighted_mode(c("a", "b", "c", "b", "c"))
}
