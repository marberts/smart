\name{price relative}
\alias{price_relative}

\title{Price relative}

\description{
Compute period-over-period price relatives.
}

\usage{
price_relative(price, period, product = names(price), include_base = TRUE)
}

\arguments{
\item{price}{A numeric vector of prices.}

\item{period}{A factor, or something that can be coerced into one, that gives the corresponding time period for each \code{price}. The ordering of time periods follows of the levels of \code{period}.}

\item{product}{A factor, or something that can be coerced into one, that gives the corresponding product identifier for each \code{price}. The default is to use the names of \code{price}, if any.}

\item{include_base}{Should price relatives in the first period be 1? Otherwise, price relatives in the first period are \code{NA}.}
}

\value{
A vector of price relatives, the same length as \code{price}.
}

\note{
By definition, there must be at most one price for each product in each time period to make price relatives. Having multiple prices corresponding to a period-product pair can give unexpected results.
}

\examples{
price <- 1:4
product <- c("a", "b", "a", "b")
period <- c(1, 1, 2, 2)

price_relative(price, period, product)

price_relative(price, period,  product, include_base = FALSE)

# Reorder time periods by setting the levels in 'period'

price_relative(price, factor(period, levels = 2:1), product)
}
