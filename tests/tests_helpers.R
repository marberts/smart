A <- 2:4
B <- 1:3
C <- 3:5

stopifnot(
  exprs = {
    distinct(letters[c(1, 1, 2)]) == 2
    distinct(NULL) == 0
    distinct(logical(0)) == 0
    distinct(c(1, NA, NA)) == 2
    distinct(c(1, NA, NA), incomparables = NA_real_) == 3
    identical(distinct(data.frame(a = 1:3, b = 1)), c(a = 3, b = 1))
    identical(distinct(data.frame()), stats::setNames(numeric(0), character(0)))
    allNA(c(NA, NA, NA))
    !allNA(c(NA, NA, 1))
    !allNA(c(3, 2, 1))
    A %n% B == 2:3
    B %n% A == 2:3
    (A %n% A) == A
    (A %n% NULL) %==% NULL
    A %n% B %n% C == 3
    ((A %n% B) %n% C) == (A %n% (B %n% C))
    A %u% B == c(2:4, 1)
    B %u% A == 1:4
    (A %u% A) == A
    (A %u% NULL) == A
    A %u% B %u% C == c(2:4, 1, 5)
    ((A %u% B) %u% C) %==% (A %u% (B %u% C))
    (A %n% (B %u% C)) %==% ((A %n% B) %u% (A %n% C))
    (A %u% (B %n% C)) %==% ((A %u% B) %n% (A %u% C))
    (A %n% B %n% C) %c% (A %u% B %u% C)
    A %-% B == 4
    B %-% A == 1
    (A %-% A) %==% NULL
    (A %-% NULL) %==% A
    (NULL %-% A) %==% NULL
    length(A %-% B %-% C) == 0
    (A %-% (B %u% C)) %==% ((A %-% B) %n% (A %-% C))
    (A %-% (B %n% C)) %==% ((A %-% B) %u% (A %-% C))
    (A %-% (B %-% C)) %==% ((A %n% C) %u% (A %-% B))
    ((A %-% B) %n% C) %==% ((A %n% C) %-% B)
    ((A %-% B) %n% C) %==% (A %n% (C %-% B))
    ((A %-% B) %u% C) %==% ((A %u% C) %-% (B %-% C))
    !A %c% B
    A %c% 1:5
    !A %c% NULL
    NULL %c% A
    A %c% A
  },
  local = getNamespace("smart")
)
