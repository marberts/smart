#---- Mode ----
stopifnot(
  exprs = {
    weighted_mode(letters[c(1, 2, 2, 3)]) == "b"
    weighted_mode(c(1, 1, 2, 3)) == 1
    weighted_mode(c(T, F, T))
    weighted_mode(letters[c(1, 2, 2, 3)], c(3, 1, 1, 1)) == "a"
    isFALSE(weighted_mode(c(T, F, T), c(1, 5, 1)))
    # NAs
    identical(weighted_mode(c(1, 2, 2, NA, NA, NA)), NA_real_)
    weighted_mode(c(1, 2, 2, NA, NA, NA), na.rm = TRUE) == 2
    identical(weighted_mode(c(T, F, T), c(1, NA, 1)), NA)
    weighted_mode(c(T, F, T), c(1, NA, 1), na.rm = TRUE)
    identical(weighted_mode(c(NA, "a", NA), c(1, NA, 1)), NA_character_)
    identical(weighted_mode(c(NA, "a", NA), c(1, NA, 1), na.rm = TRUE), character(0))
    # Length 0 inputs
    identical(weighted_mode(integer(0)), integer(0))
    identical(weighted_mode(character(0)), character(0))
    identical(weighted_mode(logical(0)), logical(0))
    identical(weighted_mode(integer(0), integer(0)), integer(0))
    identical(weighted_mode(logical(0), integer(0)), logical(0))
    identical(weighted_mode(character(0), integer(0)), character(0))
    is.null(weighted_mode(NULL))
   }, local = getNamespace("smart")
)
