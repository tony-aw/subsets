
# i ====

xlist <- list(
  1:10,
  as.list(1:10),
  matrix(1:10, ncol=2),
  array(1:27, dim = c(3,3,3))
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = -1:-10),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = 0),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = 1000),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = sample(c(TRUE, FALSE), size = length(xlist[[i]]) - 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = sample(c(TRUE, FALSE), size = length(xlist[[i]]) + 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = rep(NA, length(xlist[[i]]))),
    pattern = "NA indices not allowed",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

xlist <- list(
  1:10,
  as.list(1:10),
  matrix(1:10, ncol=2),
  array(1:27, dim = c(3,3,3))
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], i = "a"),
    pattern = "`x` has no names; fix this before subsetting",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}


# lvl ====
if(test_use_factors) {
  x <- factor(month.abb[1:10])
  names(x) <- letters[1:10]
  # expect_error(
  #   sb_test(x, lvl = "1"),
  #   pattern = "unknown level given"
  # ) |> errorfun()
  expect_error(
    sb_test(x, lvl = "1", i = 1),
    pattern = "cannot specify both elements and levels"
  ) |> errorfun()
  enumerate <- enumerate + 2
}


# row ====
xlist <- list(
  matrix(1:10, ncol=2),
  data.frame(a = 1:10, b = 1:10)
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = -1:-5),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = 0),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = 1000),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = sample(c(TRUE, FALSE), size = nrow(xlist[[i]]) - 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = sample(c(TRUE, FALSE), size = nrow(xlist[[i]]) + 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = rep(NA, nrow(xlist[[i]]))),
    pattern = "NA indices not allowed",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

xlist <- list(
  matrix(1:10, ncol=2)
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], row = "a"),
    pattern = "`x` has no names; fix this before subsetting",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

# col ====
xlist <- list(
  matrix(1:10, ncol=5),
  data.frame(a = 1:10, b = 1:10, c=1:10, d=1:10, e=1:10)
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = -1:-5),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = 0),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = 1000),
    pattern = "integers must be > 1 and < bounds",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = sample(c(TRUE, FALSE), size = ncol(xlist[[i]]) - 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = sample(c(TRUE, FALSE), size = ncol(xlist[[i]]) + 1, replace = TRUE)),
    pattern = "incorrect length of logical indices",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = rep(NA, ncol(xlist[[i]]))),
    pattern = "NA indices not allowed",
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

xlist <- list(
  matrix(1:10, ncol=2)
)

for(i in 1:length(xlist)) {
  expect_error(
    sb_test(xlist[[i]], col = "a"),
    pattern =  paste0(
      "`x` has no names; fix this before subsetting"
    ),
    fixed = TRUE
  )|> errorfun()
  enumerate <- enumerate + 1
}

# dimensions ==== 
x <- array(1:27, c(3,3,3))
expect_error(
  sb_test(x, idx = 1:10, dims = c(1,3)),
  pattern = "`idx` must be a list, and `dims` must be a integer vector",
  fixed = TRUE
)
expect_error(
  sb_test(x, idx = list(1:10), dims = c(1,3)),
  pattern = "`length(idx) != length(dims)`",
  fixed = TRUE
)

expect_error(
  sb_test(x, idx = list(-1:-5), dim = 1),
  pattern = "integers must be > 1 and < bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, idx = list(0), dim = 1),
  pattern = "integers must be > 1 and < bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list(1000), dim = 1),
  pattern = "integers must be > 1 and < bounds",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, list(sample(c(TRUE, FALSE), size = nrow(x) - 1, replace = TRUE)), dim = 1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x,list(sample(c(TRUE, FALSE), size = nrow(x) + 1, replace = TRUE)), dim = 1),
  pattern = "incorrect length of logical indices",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



expect_error(
  sb_test(x, list(rep(NA, nrow(x))), dim = 1),
  pattern = "NA indices not allowed",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1


expect_error(
  sb_test(x, list("a"), dim = 1),
  pattern = "`x` has no names; fix this before subsetting",
  fixed = TRUE
)|> errorfun()
enumerate <- enumerate + 1



# data.frame-like objects ====
xlist <- list(
  df = data.frame(a = 1:26, b = letters),
  dt = data.table::data.table(a = 1:26, b = letters),
  tb = tibble::tibble(a = 1:26, b = letters),
  tt = tidytable::tidytable(a = 1:26, b = letters)
)
for(i in 1:length(xlist)) {
  x <- xlist[[i]]
  expect_error(
    sb_test(x, filter = "foo"),
    pattern = "`filter` must be a formula"
  ) |> errorfun()
  expect_error(
    sb_test(x, filter = ~ mean(a)),
    pattern = "invalid formula given"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = "is.numeric"),
    pattern = "`vars` must be a function"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = "is.numeric"),
    pattern = "`vars` must be a function"
  ) |> errorfun()
  expect_error(
    sb_test(x, vars = mean),
    pattern = "values must be type 'logical'"
  ) |> errorfun()
  enumerate <- enumerate + 5
}

for (i in 1:length(xlist)) {
  x <- xlist[[i]]
  colnames(x) <- c("a", "a")
  expect_error(
    sb_test(x, col=1),
    pattern = paste0(
      "`x` does not have unique variable names for all columns; ",
      "\n",
      "fix this before subsetting"
    )
  ) |> errorfun()
  enumerate <- enumerate + 1
}


# multi ====
xlist <- list(
  df = data.frame(a = 1:26, b = letters),
  dt = data.table::data.table(a = 1:26, b = letters),
  tb = tibble::tibble(a = 1:26, b = letters),
  tt = tidytable::tidytable(a = 1:26, b = letters)
)
for(i in 1:length(xlist)) {
  x <- xlist[[i]]
  expect_error(
    sb_test(x, row = 1:2, filter = ~ a>2),
    pattern = "cannot specify both `filter` and `row`"
  ) |> errorfun()
  expect_error(
    sb_test(x, col = 1:2, vars = is.numeric),
    pattern = "cannot specify both `vars` and `col`"
  ) |> errorfun()
  enumerate <- enumerate + 1
}

# x <- matrix(1:10, ncol=2)
# expect_error(
#   sb_test(x, row = 1 , col = 1, i = 1),
#   pattern = "cannot specify both row/col and elements",
#   fixed = TRUE
# ) |> errorfun()
# enumerate <- enumerate + 1


# duplicates ====
if(!test_allow_duplicates) {
  x <- 1:10
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1)),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a")),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- matrix(1:10, ncol=2)
  names(x) <- letters[1:10]
  expect_error(
    sb_test(x, i = c(1,1,1)),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, i = c("a", "a", "a")),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  rownames(x) <- letters[1:5]
  colnames(x) <- letters[1:2]
  row <- list(NULL, c(1,1,1), c("a", "a", "a"))
  col <- list(NULL, c(1,1), c("a", "a"))
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      if(!is.null(row[[i]]) || !is.null(col[[j]])) {
        expect_error(
          sb_test(x, row = row[[i]], col = col[[j]]),
          pattern = "duplicate integers or names not allowed"
        ) |> errorfun()
        enumerate <- enumerate + 1
      }
    }
  }
  
  expect_error(
    sb_test(x, col = c(1,1,1)),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  expect_error(
    sb_test(x, row = c(1,1,1), col = c(1,1,1)),
    pattern = "duplicate integers or names not allowed"
  ) |> errorfun()
  
  x <- array(1:27, c(3,3,3))
  rownames(x) <- c("a", "a", "b")
  expect_error(
    sb_test(x, list(c(1,1,1)), dim = 1),
    pattern = "duplicate integers or names not allowed"
  )
  expect_error(
    sb_test(x, list(c("a", "a")), dim = 1),
    pattern = "duplicate integers or names not allowed"
  )
  
}
