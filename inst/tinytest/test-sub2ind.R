
source(file.path(getwd(), "source", "functions4testing.R"))

enumerate <- 0

# 4D array ==== 
dims <- c(10, 10, 10, 10)
len <- prod(dims)
temp.fun <- function(...) {
  return(x[...])
}

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 3, FALSE)
  ind2 <- sample(1:10, 3, FALSE)
  ind3 <- sample(1:10, 3, FALSE)
  ind4 <- sample(1:10, 3, FALSE)
  subs <- list(ind1, ind2, ind3, ind4)
  coords <- sub2coord(subs, length(dims))
  ind <- coord2ind(coords, dims, len)
  x.coord <- numeric(length(ind))
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3], coords[i,4]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  coords <- ind2coord(ind, dims, len)
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3], coords[i,4]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  expect_equal(
    coord2sub(coords), subs
  ) |> errorfun()
  enumerate <- enumerate + 3
}


# 3D array ==== 
dims <- c(10, 10, 10)
len <- prod(dims)
temp.fun <- function(...) {
  return(x[...])
}

for(i in 1:10) {
  x <- array(sample(seq_len(len*10), len, FALSE), dims)
  ind1 <- sample(1:10, 3, FALSE)
  ind2 <- sample(1:10, 3, FALSE)
  ind3 <- sample(1:10, 3, FALSE)
  subs <- list(ind1, ind2, ind3)
  coords <- coords <- sub2coord(subs, length(dims))
  ind <- coord2ind(coords, dims, len)
  x.coord <- numeric(length(ind))
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  coords <- ind2coord(ind, dims, len)
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2], coords[i,3]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  expect_equal(
    coord2sub(coords), subs
  ) |> errorfun()
  enumerate <- enumerate + 3
}



# matrix ==== 
dims <- c(10, 10)
len <- prod(dims)
temp.fun <- function(...) {
  return(x[...])
}

for(i in 1:10) {
  x <- matrix(sample(seq_len(len*10), len, FALSE), ncol = 10)
  ind1 <- sample(1:10, 3, FALSE)
  ind2 <- sample(1:10, 3, FALSE)
  subs <- list(ind1, ind2)
  coords <- coords <- sub2coord(subs, length(dims))
  ind <- coord2ind(coords, dims, len)
  x.coord <- numeric(length(ind))
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  coords <- ind2coord(ind, dims, len)
  for(i in 1:length(x.coord)) {
    x.coord[i] <- x[coords[i,1], coords[i,2]]
  }
  expect_equal(
    x[ind], x.coord
  ) |> errorfun()
  expect_equal(
    coord2sub(coords), subs
  ) |> errorfun()
  enumerate <- enumerate + 3
}



# error checks ====
dims <- c(1000, 1000, 4, 4)
len <- prod(dims)
x <- array(1:len, dims)
coords <- rbind(c(4:1), 1:4)
expect_error(
  coord2ind(coords, numeric(0), len),
  pattern = "`length(x.dim) == 0`",
  fixed = TRUE
)
expect_error(
  coord2ind(matrix("1", ncol=4), character(4), len),
  pattern = "`x.dim` and `coords` must both be numeric",
  fixed = TRUE
)
expect_error(
  coord2ind(coords, c(1, dims), len),
  pattern = "`ncol(coords) != length(x.dim)`",
  fixed = TRUE
)
expect_error(
  coord2ind(coords, dims, 1),
  pattern = "length of object does not correspond to the given dimensions"
)
enumerate <- enumerate + 4


