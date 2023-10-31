
enumerate <- 0

# sb_str ====
expect_equal(
  sb_str("hello", 5:1),
  "olleh"
)
expect_equal(
  sb_str("hello", c(1:5, 5)),
  "helloo"
)
expect_equal(
  sb_str("hello", 2:5),
  "ello"
)
enumerate <- enumerate + 3

# sb_rec ====
x <- list(
  A = list(
    A = list(A = "AAA", B = "AAB"),
    B = list(A = "ABA", B = "ABB")
  ),
  B = list(
    A = list(A = "BAA", B = "BAB"),
    B = list(A = "BBA", B = "BBB")
  )
)
expect_equal(sb_rec(x, c(1,2,2)), "ABB")
expect_equal(sb_rec(x, c(2,2,1)), "BBA")
enumerate <- enumerate + 2

print(enumerate)
