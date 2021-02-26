\name{set operators}
\alias{\%c\%}
\alias{\%n\%}
\alias{\%u\%}
\alias{\%-\%}
\alias{\%(-)\%}
\alias{\%==\%}

\title{
Set operators
}

\description{
Binary operators for set inclusion, intersection, union, asymmetric and symmetric difference, and equality, just like \code{\%in\%}.
}

\usage{
x \%c\% y

x \%n\% y

x \%u\% y

x \%-\% y

x \%(-)\% y

x \%==\% y
}

\arguments{
\item{x, y}{The same as for \code{\link[=union]{union()}}, \code{\link[=intersect]{intersect()}}, \code{\link[=setdiff]{setdiff()}}, and \code{\link[=setequal]{setequal()}}.}
}

\value{
Return values for intersection, union, difference, and equality are the same as their base counterparts. Inclusion returns a length 1 logical that is \code{TRUE} if \code{x} is a subset (or equal to) \code{y}, \code{FALSE} otherwise. The symmetric difference returns the elements of \code{x} and \code{y} that are not members of both \code{x} and \code{y}.
}

\examples{
A <- 2:4
B <- 1:3
C <- 3:5

A \%n\% B \%n\% C

A \%u\% B \%u\% C

(A \%n\% B \%n\% C) \%c\% (A \%u\% B \%u\% C)

(A \%-\% (B \%u\% C)) \%==\% ((A \%-\% B) \%n\% (A \%-\% C))

A \%(-)\% NULL \%==\% A
}
