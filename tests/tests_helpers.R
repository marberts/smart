stopifnot(
  exprs = {
    distinct(letters[c(1, 1, 2)]) == 2
    distinct(NULL) == 0
    distinct(logical(0)) == 0
    distinct(c(1, NA, NA)) == 2
    distinct(c(1, NA, NA), incomparables = NA_real_) == 3
    allNA(c(NA, NA, NA))
    !allNA(c(NA, NA, 1))
    !allNA(c(3, 2, 1))
    integer_digits(-1) == 1
    integer_digits(-1.5) == 1
    integer_digits(0) == 1
    integer_digits(0.1) == 1
    integer_digits(-0.9) == 1
    integer_digits(100) == 3
    integer_digits(100.5) == 3
    identical(integer_digits(integer(0)), numeric(0))
    all.equal(generalized_ave(median)(1:10), rep(median(1:10), 10))
    all.equal(generalized_ave(sum)(1:10), rep(55, 10))
    all.equal(generalized_ave(mean)(1:10, rep(1:2, 5)), ave(1:10, rep(1:2, 5)))
    all.equal(generalized_ave(complete_quarters)(as.Date(c("2018-01-01", "2018-02-04", "2018-03-07", "2018-03-19")), c(1, 1, 1, 2)),
              c(TRUE, TRUE, TRUE, FALSE))
    all.equal(generalized_ave(mean)(c(1, 2, NA), c(1, 2, 2)), c(1, NA, NA))
    all.equal(generalized_ave(mean)(c(1, 2, NA), c(1, 2, 2), na.rm = TRUE), c(1, 2, 2))
    all.equal(generalized_ave(mean)(c(1, 2, NA), c(1, 2, NA), na.rm = TRUE), c(1, 2, NA))
  },
  local = getNamespace("smart")
)
