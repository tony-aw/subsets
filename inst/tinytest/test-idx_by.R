
source(file.path(getwd(), "source", "functions4testing.R"))

enumerate <- 0

for(i in 1:10) {
  r <- sample(1:20)
  grp <- factor(sample(letters[1:20]))
  expect_equal(
    idx_by(head, r, grp) |> as.integer(),
    tapply(r, grp, head) |> unlist(use.names = FALSE) |> as.integer()
  ) |> errorfun()
  r <- sample(letters[1:20])
  expect_equal(
    idx_by(head, r, grp) |> as.character(),
    tapply(r, grp, head) |> unlist(use.names = FALSE) |> as.character()
  ) |> errorfun()
  enumerate <- enumerate + 2
}


