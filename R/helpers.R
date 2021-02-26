#---- Distinct ----
distinct <- function(x, ...) length(unique(x, ...))

#---- NAs ----
allNA <- function(x) all(is.na(x))

#---- Generalized ave ----
generalized_ave <- function(fun) {
  fun <- match.fun(fun)
  # return function
  function(x, f = rep(1L, length(x)), ...) {
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
