\name{set operators}
\alias{\%c\%}
\alias{\%n\%}
\alias{\%u\%}
\alias{\%-\%}
\alias{\%==\%}

\title{
Set operators
}

\description{
Binary operators for set inclusion, intersection, union, asymmetric difference, and equality, just like \code{\%in\%}.
}

\usage{
x \%c\% y

x \%n\% y

x \%u\% y

x \%-\% y

x \%==\% y
}

\arguments{
\item{x, y}{The same as in \code{\link[base]{sets}}.}
}

\value{
Output for intersection, union, difference, and equality are the same as in \code{\link[base]{sets}}. Inclusion returns a length 1 logical that is \code{TRUE} if \code{x} is a subset (or equal to) \code{y}.
}

\examples{
A <- 2:4
B <- 1:3
C <- 3:5

A \%n\% B \%n\% C

A \%u\% B \%u\% C

(A \%n\% B \%n\% C) \%c\% (A \%u\% B \%u\% C)

(A \%-\% (B \%u\% C)) \%==\% ((A \%-\% B) \%n\% (A \%-\% C))
}
