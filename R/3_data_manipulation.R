#---- Price relatives ----
price_relative <- function(price, period, product = names(price), include_base = TRUE) {
  stopifnot("'price' must be a numeric vector" = is.numeric(price),
            "'period' must be an atomic vector" = is.atomic(period),
            "'product' must be an atomic vector" = is.atomic(product),
            "'price' must be of length 2 or more" = length(price) >= 2,
            "'price', 'period', and 'product' must be the same length" = all_same_length(price, period, product),
            "'include_base' must be TRUE or FALSE" = is_T_or_F(include_base))
  offset <- function(x) {
    i <- c(1L, seq_len(length(x) - 1))
    replace(x[i], if(!include_base) 1, NA)
  }
  atrib <- attributes(price) # unsplit mangles attributes, so keep them for later
  period <- as.factor(period)
  product <- split(product, period)
  price <- split(price, period)
  matches <- Map(match, product, offset(product), incomparables = NA)
  rel <- Map(function(p1, p0, m) p1 / p0[m], price, offset(price), matches)
  res <- unsplit(rel, period)
  attributes(res) <- atrib
  res
}
