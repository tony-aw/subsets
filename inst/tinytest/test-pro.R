

x <- "colx"
y <- "coly"
expect_error(
  aes_pro(x, y),
  pattern = "formula inputs must be given"
)


expect_error(
  aes_pro(x ~ y, y ~ x),
  pattern = "improper formula given"
)

enumerate <- 2