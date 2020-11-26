\name{little helpers}
\alias{distinct}
\alias{distinct.default}
\alias{distinct.data.frame}
\alias{allNA}
\alias{integer_digits}

\title{
Little helpers
}

\description{
Little helper functions that are often useful.
\itemize{
\item{\code{distinct()} returns the number of unique element in \code{x}.}
\item{\code{allNA()} tests if all elements of \code{x} are NA.}
\item{\code{integer_digits()} gives the number of integer digits in a number (i.e., number of digits to the left of the decimal place).}
}
}

\usage{
\method{distinct}{default}(x, ...)

\method{distinct}{data.frame}(x, ...)

allNA(x)

integer_digits(x)
}

\arguments{
\item{x}{An R object, usually a vector.}

\item{...}{Other arguments to pass to \code{\link[base]{unique}}.}
}

\value{
The default method for \code{distinct()} just calls \code{length(unique(x, ...))}. The method for data frames operates on each column, returning a named vector with the number of distinct element in each column.

\code{allNA()} just calls \code{all(is.na(x))}.

\code{integer_digits()} is roughly the same as \code{nchar(abs(trunc(x)))}, but much faster.
}

\examples{
distinct(1:3)

distinct(c(1:3, NA, NA)) # NAs are lumped together

distinct(c(1:3, NA, NA), incomparables = NA_real_)

distinct(mtcars)

getS3method("distinct", "default")(mtcars) # none of the colums are identical

integer_digits(-1.5) # same as nchar(abs(trunc(-1.5)))

integer_digits(Inf) # nchar(abs(trunc(Inf))) returns 3
}
