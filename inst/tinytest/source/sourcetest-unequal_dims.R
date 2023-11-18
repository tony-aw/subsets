
# uniquely named matrix ====
x <- matrix(as.double(-1:-20), nrow = 5, ncol=4)
rownames(x) <- letters[1:5]
colnames(x) <- letters[1:4]
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)


# unnamed matrix ====
x <- matrix(as.double(-1:-20), nrow = 5, ncol=4)
row <- indx_general(x, 1)
col <- indx_general(x, 2)
temp.fun.matrix(x, row, col)


# non-uniquely named matrix ====
x <- matrix(as.double(-1:-20), nrow = 5, ncol=4)
rownames(x) <- c("a", "a", "b", "c", NA)
colnames(x) <- c("a", "a", "b", NA)
row <- indx_named(x, 1)
col <- indx_named(x, 2)
temp.fun.matrix(x, row, col)
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, row = c("a", "a", "a")),
    rep3.bind(x[which(rownames(x) %in% "a"), ], 1)
  ) |> errorfun()
}
if(isTRUE(test_allow_duplicates)) {
  expect_equal(
    sb_x(x, col = c("a", "a", "a")),
    rep3.bind(x[, which(colnames(x) %in% "a")], 2)
  ) |> errorfun()
}

