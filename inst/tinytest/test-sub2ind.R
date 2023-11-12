enumerate <- 0

# 4D array ==== 
dims <- c(1000, 1000, 4, 4)
len <- prod(dims)
x <- array(1:len, dims)
coords <- rbind(c(4:1), 1:4)
ind <- sub2ind(coords, dims, len)
expect_equal(
  x[ind], c(x[4, 3, 2, 1], x[1, 2, 3, 4])
)
ind <- sub2ind(coords, dims, len, checks = FALSE)
expect_equal(
  x[ind], c(x[4, 3, 2, 1], x[1, 2, 3, 4])
)
expect_equal(
  ind2sub(ind, dims, len),
  coords
)
enumerate <- enumerate + 3


# 3D array ==== 
dims <- c(1000, 1000, 10)
len <- prod(dims)
x <- array(1:len, dims)
coords <- rbind(c(3:1), 1:3)
ind <- sub2ind(coords, dims, len)
expect_equal(
  x[ind], c(x[3, 2, 1], x[1, 2, 3])
)
ind <- sub2ind(coords, dims, len, checks = FALSE)
expect_equal(
  x[ind], c(x[3, 2, 1], x[1, 2, 3])
)
expect_equal(
  ind2sub(ind, dims, len),
  coords
)
enumerate <- enumerate + 3


# matrix ==== 
len <- (100^2)
dims <- c(100, 100)
x <- matrix(1:len, 100, 100)
coords <- rbind(c(10, 11), c(11, 10))
ind <- sub2ind(coords, dims, len)
expect_equal(
  x[ind], c(x[10, 11], x[11, 10])
)
ind <- sub2ind(coords, dims, len, checks = FALSE)
expect_equal(
  x[ind], c(x[10, 11], x[11, 10])
)
expect_equal(
  ind2sub(ind, dims, len),
  coords
)
enumerate <- enumerate + 3


# vector ==== 
len <- dims <- 1e6
x <- seq_len(len)
coords <- matrix(c(1, 10), ncol=1)
ind <- sub2ind(coords, dims, len)
expect_equal(
  x[ind], x[c(1, 10)]
)
ind <- sub2ind(coords, dims, len, checks = FALSE)
expect_equal(
  x[ind], x[c(1, 10)]
)
expect_equal(
  ind2sub(ind, dims, len),
  coords
)
enumerate <- enumerate + 3


# error checks ====
dims <- c(1000, 1000, 4, 4)
len <- prod(dims)
x <- array(1:len, dims)
coords <- rbind(c(4:1), 1:4)
expect_error(
  sub2ind(coords, numeric(0), len),
  pattern = "`length(x.dim) == 0`",
  fixed = TRUE
)
expect_error(
  sub2ind(matrix("1", ncol=4), character(4), len),
  pattern = "`x.dim` and `coords` must both be numeric",
  fixed = TRUE
)
expect_error(
  sub2ind(coords, c(1, dims), len),
  pattern = "`ncol(coords) != length(x.dim)`",
  fixed = TRUE
)
expect_error(
  sub2ind(coords, dims, 1),
  pattern = "length of object does not correspond to the given dimensions"
)
enumerate <- enumerate + 4


