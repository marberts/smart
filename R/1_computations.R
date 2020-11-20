#---- Mode ----
weighted_mode <- function(x, w = rep(1, length(x)), na.rm = FALSE) {
  stopifnot("'x' must be an atomic vector" = is.atomic(x),
            "'w' must be a numeric vector" = is.numeric(w),
            "'x' and 'w' must be the same length" = length(x) == length(w),
            "'na.rm' must be TRUE or FALSE" = is_T_or_F(na.rm))
  if (!length(x)) return(x)
  if ((anyNA(x) || anyNA(w))) {
    if (na.rm) {
      keep <- !(is.na(x) | is.na(w))
      x <- x[keep]
      w <- w[keep]
    } else {
      return(x[0][NA])
    }
  }
  ux <- unique(x)
  f <- as.factor(match(x, ux))
  tab <- vapply(split(w, f), sum, numeric(1))
  res <- ux[tab == max(tab)]
  if (length(res) > 1) warning("mode is not unique")
  res
}
