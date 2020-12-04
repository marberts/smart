price <- 1:10
id <- letters[c(5:1, 1:5)]
period <- rep(1:2, each = 5)
names(price) <- id

stopifnot(
  exprs = {
    identical(price_relative(price, period, id), structure(c(1, 1, 1, 1, 1, 6:10 / 5:1), names = id))
    identical(price_relative(price, period, include_base = FALSE), structure(c(NA, NA, NA, NA, NA, 6:10 / 5:1), names = id))
    identical(price_relative(price[-1], period[-1], id[-1]), structure(c(1, 1, 1, 1, 6:9 / 5:2, NA), names = id[-1]))
  },
  local = getNamespace("smart")
)