#---- Mode ----
weighted_mode <- function(x, w = rep(1L, length(x)), na.rm = FALSE) {
  if (length(x) != length(w)) {
    stop("'x' and 'w' must be the same length")
  }
  if (na.rm) {
    if (anyNA(x) || anyNA(w)) { # nested if to prevent anyNA(w) getting called twice
      keep <- !(is.na(x) | is.na(w))
      x <- x[keep]
      w <- w[keep]
    }
  } else if (anyNA(w)) {
    return(x[0][NA]) # impossible to know mode if any weights are missing
  }
  ux <- unique(x)
  if (!length(ux)) return(ux) # prevents max for returning -Inf
  f <- as.factor(match(x, ux))
  tab <- vapply(split(w, f), sum, numeric(1), USE.NAMES = FALSE)
  is_mode <- tab == max(tab) # lines up with ux
  if (anyNA(x)) {
    na <- which(is.na(ux)) # single integer
    modes <- which(is_mode)
    cond <- na %in% modes || # mode is NA if any mode is NA
      # or if the weight for any mode does not exceed the weight for the NA
      # and the weight for the next largest weight
      (length(ux) > 2 && tab[na] + max(tab[-c(modes[1], na)]) >= tab[modes[1]])
    if (cond) return(x[0][NA])
  }
  if (sum(is_mode) > 1) warning("mode is not unique")
  ux[is_mode]
}
