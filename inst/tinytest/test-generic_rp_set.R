
# set-up ====

enumerate <- 0 # to count number of tests in loops
source(file.path(getwd(), "source", "functions4testing.R"))

test_allow_duplicates <- FALSE
test_use_factors <- FALSE
any_empty_indices <- subsets:::.any_empty_indices

# test elements ====

test_sb <- function(x, i, rp) {
  if(!is.list(x)) {
    i <- indx_x(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    x[i] <- rp
    return(x)
  }
  if(is.list(x)) {
    i <- indx_x(i, x, names(x), length(x))
    if(length(i) == 0) return(x)
    if(length(i) != 1)  x[i] <- as.list(rp)
    if(length(i) == 1) x[[i]] <- rp
    return(x)
  }
}

sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  sb_set(x, ...)
  return(x)
}

temp.fun <- function(x, elements) {
  for (i in 1:length(elements)) {
    rp1 <- rp2 <- rep(1, length(indx_x(elements[[i]], x, names(x), length(x))))
    if(is.list(x)) rp1 <- as.list(rp1)
    if(is.list(x) && length(rep) != 1) rp2 <- as.list(rp)
    expect_equal(
      sb_set2(x, i = elements[[i]], rp = rp1),
      test_sb(x, i = elements[[i]], rp = rp2)
    ) |> errorfun()
    assign("enumerate", enumerate + 1, envir = parent.frame(n = 1))
  }
}


indx_general <- list(
  integer(0),
  1, 1:2, 2:1,
  c(rep(TRUE, 24), rep(FALSE, 24)),
  rep(TRUE, 48), rep(FALSE, 48),
  c(TRUE, rep(FALSE, 47)), c(FALSE, rep(TRUE, 47)),
  function(x) x>5
)

indx_named <- c(indx_general, "ab")

sys.source(file.path(getwd(), "source", "sourcetest-elements.R"), envir = environment())


# test equal dimensions ====

rep3.bind <- function(x, dim) {
  return(abind::abind(x, x, x, along = dim))
}

pre_subset_mat <- function(x, row = NULL, col = NULL) {
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col])
}

subset_mat <- function(x, row = NULL, col = NULL, rp) {
  
  if(!is.null(row)) row <- indx_x(row, x, rownames(x), nrow(x))
  if(!is.null(col)) col <- indx_x(col, x, colnames(x), ncol(x))
  
  if(any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  x[row, col] <- rp
  
  return(x)
}


temp.fun.matrix <- function(x, row, col) {
  for(i in 1:length(row)) {
    for(j in 1:length(col)) {
      # if(!rlang::is_missing(row[[i]]) && !rlang::is_missing(col[[j]])) {
        len <- length(pre_subset_mat(x, row[[i]], col[[j]]))
        rp <- seq_len(len)
        expect_equal(
          sb_set2(x, row = row[[i]], col = col[[j]], rp = rp),
          subset_mat(x, row[[i]], col[[j]], rp = rp)
        ) |> errorfun()
        expect_true(sb_set2(x, row = row[[i]], col = col[[j]], rp = rp) |>
                      is.matrix()) |> errorfun()
        assign("enumerate", enumerate + 2, envir = parent.frame(n = 1))
      # }
    }
  }
}



indx_general <- list(
  NULL,
  logical(0),
  rep(TRUE, 4), rep(FALSE, 4),
  c(TRUE, FALSE, FALSE, FALSE), c(FALSE, TRUE, TRUE, TRUE),
  1, seq(1, 2, by = 1), seq(2, 1, by = -1), 4
)

indx_named <- c(indx_general, list("a", c("a", "b")))

sys.source(file.path(getwd(), "source", "sourcetest-equal_dims.R"), envir = environment())


# test unequal dimensions ====


indx_general <- function(x, dim.i) {
  dim.n <- dim(x)[[dim.i]]
  dim.n1 <- dim.n - round(dim.n/2)
  dim.n2 <- dim.n - dim.n1
  list(
    NULL,
    logical(0),
    rep(TRUE, dim.n), rep(FALSE, dim.n),
    c(rep(TRUE, dim.n1), rep(FALSE, dim.n2)),
    1, seq(1, 2, by = 1), seq(2, 1, by = -1)
  )
}

indx_named <- function(x, dim.i) {
  c(indx_general(x, dim.i), list("a", c("a", "b")))
}

sys.source(file.path(getwd(), "source", "sourcetest-unequal_dims.R"), envir = environment())



# test arbitrary dimensions ====

subset_arr <- function(x, i, j, l) {
  i <- indx_x(i, x, rownames(x), nrow(x))
  j <- indx_x(j, x, colnames(x), ncol(x))
  l <- indx_x(l, x, dimnames(x)[4], dim(x)[4])
  rp <- seq_len(length(x[i, j, , l])) * -1
  x[i, j, , l] <- rp
  return(x)
}


x <- array(seq_len(10^4), dim = c(10, 10, 10, 10))
rownames(x) <- c(letters[1:8], "a", NA)

idx <- list(c("a"), c(1:3), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
rp <- seq_len(length(sb_x(x, idx, dims)))* -1
expect_equal(
  sb_set2(x, idx, dims, rp = rp),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]])
)

idx <- list(c("a"), logical(0), c(rep(TRUE, 5), rep(FALSE, 5)))
dims <- c(1,2,4)
rp <- seq_len(length(sb_x(x, idx, dims)))* -1
expect_equal(
  sb_set2(x, idx, dims, rp = rp),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]])
)

idx <- list(c("a"), c(1:4), rep(FALSE, 10))
dims <- c(1,2,4)
rp <- seq_len(length(sb_x(x, idx, dims)))* -1
expect_equal(
  sb_set2(x, idx, dims, rp = rp),
  subset_arr(x, idx[[1]], idx[[2]], idx[[3]])
)

enumerate <- enumerate + 3


# test datasets ====
# datasets are more complex; replacing relies on data.table::set()
# different tests will be used here ...
# fill in later .... 

# test errors ====

sb_set2 <- function(x, ...) {
  x <- data.table::copy(x)
  sb_set(x, ...)
  return(x)
}

x <- 1:10
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed",
  fixed = TRUE
)
enumerate <- enumerate + 2


x <- as.list(1:10)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:10),
  pattern = "`rp` must be a list"
)
expect_error(
  sb_set2(x, i = 1:5, rp = as.list(1:6)),
  pattern = "recycling not allowed",
  fixed = TRUE
)
enumerate <- enumerate + 3


x <- matrix(1:10, nrow = 2)
expect_error(
  sb_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "`rp` must be non-recursive"
)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, row = 1:2, col = 1:2, rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 4


x <- array(1:27, dim = c(3,3,3))
expect_error(
  sb_set2(x, i = 1:5, rp = as.list(1:5)),
  pattern = "`rp` must be non-recursive"
)
expect_error(
  sb_set2(x, i = 1:5, rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, list(1:2, 1:2), c(1,3), rp = 1:6),
  pattern = "recycling not allowed"
)
expect_error(
  sb_set2(x, list(1:2, 1:2), c(2,3), rp = 1:6),
  pattern = "recycling not allowed"
)
enumerate <- enumerate + 5


x <- data.frame(a = 1:10, b = letters[1:10])
expect_error(
  sb_set2(x, row = 1:5, rp = 1:10),
  pattern = "`rp` must be a data.frame-like object or a list"
)
enumerate <- enumerate + 2


# report number of tests

print(enumerate)

