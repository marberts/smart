#---- Sets ----
set_subset <- function(x, y) !anyNA(match(as.vector(x), as.vector(y)))

set_symdiff <- function(x, y) union(setdiff(x, y), setdiff(y, x))

`%c%` <- function(x, y) set_subset(x, y)

`%==%` <- function(x, y) setequal(x, y)

`%n%` <- function(x, y) intersect(x, y)

`%u%` <- function(x, y) union(x, y)

`%-%` <- function(x, y) setdiff(x, y)

`%(-)%` <- function(x, y) set_symdiff(x, y)