A <- 2:4
B <- 1:3
C <- 3:5

stopifnot(
  exprs = {
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
    A %(-)% B %==% c(1, 4)
    B %(-)% A %==% c(1, 4)
    (A %(-)% B %(-)% C) %==% c(1, 3, 5)
    ((A %(-)% B) %(-)% C) %==% (A %(-)% (B %(-)% C))
    (A %(-)% B) %c% (A %u% B)
    (A %u% B) %==% ((A %(-)% B) %(-)% (A %n% B))
    A %(-)% NULL %==% A
    A %(-)% A %==% NULL
    ((A %(-)% B) %(-)% (B %(-)% C)) %==% (A %(-)% C)
    (A %n% (B %(-)% C)) %==% ((A %n% B) %(-)% (A %n% C))
  },
  local = getNamespace("smart")
)
