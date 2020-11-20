x <- seq(as.Date("2017-12-04"), as.Date("2019-04-04"), "month")[-5]

stopifnot(
  exprs = {
    complete_quarters(c("2016-12-01", "2017-01-01", "2017-02-01", "2017-03-01", "2017-04-01")) == c(FALSE, TRUE, TRUE, TRUE, FALSE)
    identical(complete_quarters(c("2017-01-01", "2017-02-01", "2017-03-01", NA, "2017-02-01")), c(TRUE, TRUE, TRUE, NA, TRUE))
    is.na(complete_quarters(NA))
    identical(complete_quarters(character(0)), logical(0))
    complete_years(x) == c(rep(FALSE, length(x)))
  },
  local = getNamespace("smart")
)
