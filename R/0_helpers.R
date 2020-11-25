#---- Internal helpers ----
is_T_or_F <- function(x) length(x) == 1 && is.logical(x) && !is.na(x)

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
  function(x, ...) {
    stopifnot("'x' must be an atomic vector" = is.atomic(x))
    res <- setNames(rep.int(NA, length(x)), names(x))
    g <- if (missing(...)){
      as.factor(rep(1L, length(x)))
    } else {
      interaction(...)
    }
    split(res, g) <- lapply(split(x, g), fun)
    res
  }
}

#---- Integer digits ----
integer_digits <- function(x) {
  x <- abs(trunc(as.numeric(x)))
  floor(log10(pmax.int(x, 1))) + 1
}
