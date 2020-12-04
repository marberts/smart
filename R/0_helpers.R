#---- Internal helpers ----
is_T_or_F <- function(x) length(x) == 1 && is.logical(x) && !is.na(x)

all_same_length <- function(...) {
  res <- vapply(list(...), length, numeric(1))
  all(res == res[1])
}

#---- Distinct ----
distinct <- function(x, ...) UseMethod("distinct")

distinct.default <- function(x, ...) length(unique(x, ...))

distinct.data.frame <- function(x, ...) vapply(x, distinct, numeric(1), ...)

#---- NAs ----
allNA <- function(x) all(is.na(x))

#---- Sets ----
is.subset <- function(x, y) !anyNA(match(as.vector(x), as.vector(y)))

`%c%` <- function(x, y) is.subset(x, y)

`%==%` <- function(x, y) setequal(x, y)

`%n%` <- function(x, y) intersect(x, y)

`%u%` <- function(x, y) union(x, y)

`%-%` <- function(x, y) setdiff(x, y)

#---- Generalized ave ----
generalized_ave <- function(fun) {
  fun <- match.fun(fun)
  # return function
  function(x, f = rep(1L, length(x)), ...) {
    stopifnot("'x' must be an atomic vector" = is.atomic(x),
              "'f' must be an atomic vector" = is.atomic(f),
              "'x' and 'f' must be the same length" = length(x) == length(f))
    f <- as.factor(f)
    res <- lapply(split(x, f), fun, ...)
    structure(unsplit(res, f), names = names(x))
  }
}

#---- Integer digits ----
integer_digits <- function(x) {
  x <- abs(trunc(as.numeric(x)))
  floor(log10(pmax.int(x, 1))) + 1
}
