\name{generalized ave}
\alias{generalized_ave}

\title{
Generalized ave
}

\description{
A generalization of \code{\link[=ave]{ave()}} that is easier to use with functions other than \code{\link[=mean]{mean()}}.
}

\usage{
generalized_ave(fun)
}

\arguments{
\item{fun}{A function.}
}

\value{
A function

\code{function(x, f, ...)},

where \code{x} is a vector, \code{f} is a factor that groups \code{x}, and \code{...} are other arguments to pass to \code{fun}.

\code{x} serves the same role as in \code{\link[=ave]{ave()}}, with \code{f} replacing \code{...} so that there is a way to pass further arguments to \code{fun}. Unlike \code{\link[=ave]{ave()}}, the type of the return value is determined by the return value of \code{fun}, which may not be the same as \code{x}.
}

\details{
This is a function factory that returns a function that behaves much like \code{\link[=ave]{ave()}}, except that it works more consistently with functions other than \code{\link[=mean]{mean()}}. The function returned by \code{generalized_ave()}:
\enumerate{
\item{Allows further (named) arguments to be passed to \code{fun};}
\item{Bases the return value off of \code{fun}, instead of \code{x}.}
}

Unlike \code{\link[=ave]{ave()}}, the names for \code{x} are the only attributes of \code{x} that are kept.
}

\examples{
generalized_ave(mean)(1:10, rep(1:2, 5)) # same as stats::ave(1:10, rep(1:2, 5))

generalized_ave(sum)(1:10, rep(1:2, 5)) # same as stats::ave(1:10, rep(1:2, 5), FUN = sum)

generalized_ave(function(x) x == max(x))(1:10, rep(1:2, 5)) # returns a logical

# use interaction to group by multiple factors
f <- rep(1:2, 5)
g <- rep(1:5, 2)
generalized_ave(mean)(1:10, interaction(f, g)) # same as stats::ave(1:10, f, g)
}
