#---- Complete periods ----
complete_periods <- function(period = c("quarter", "year")) {
  period <- match.arg(period)
  function(x) {
    x <- as.Date(x)
    if (!length(x)) return(logical(0))
    if (allNA(x)) return(rep(NA, length(x)))
    x <- lapply(list(period, "month"), cut, x = x, labels = FALSE)
    stats::ave(x[[2]], x[[1]], FUN = distinct) == (3 + 9 * (period == "year"))
  }
}

complete_quarters <- complete_periods("quarter")

complete_years <- complete_periods("year")
