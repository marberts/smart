\name{complete periods}
\alias{complete_quarters}
\alias{complete_years}

\title{Complete periods}

\description{Find which elements in a vector of year months form complete quarters/years.}

\usage{
complete_quarters(x)

complete_years(x)
}

\arguments{
\item{x}{A vector of dates, or something that can be coerced into one.}
}

\value{
A logical vector, the same length as \code{x}, that is \code{TRUE} whenever an element of \code{x} corresponds to a complete quarter/year in \code{x}, \code{FALSE} otherwise.
}

\examples{
x <- seq(as.Date("2018-01-01"), as.Date("2019-11-01"), "month")

complete_quarters(x)

complete_years(x)
}
