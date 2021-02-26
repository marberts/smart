#---- Complete periods ----
complete_periods <- function(period = c("quarter", "year")) {
  period <- match.arg(period)
  function(x) {
    x <- as.Date(x)
    if (!length(x)) return(logical(0))
    if (allNA(x)) return(rep.int(NA, length(x)))
    x <- lapply(list(period, "month"), cut, x = x, labels = FALSE)
    generalized_ave(distinct)(x[[2]], x[[1]]) == (3 + 9 * (period == "year"))
  }
}

complete_quarters <- complete_periods("quarter")

complete_years <- complete_periods("year")
