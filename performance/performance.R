# speed tests

library(rbenchmark)
library(subsets)

x <- matrix(1:50000, ncol = 20)
colnames(x) <- c(letters[1:18], "a", NA)
bm.matrix <- benchmark(
  "subsets" = sb_x.matrix(x, 1:100, c("a", "a")),
  "base R" = x[1:100, lapply(c("a", "a"), \(i) which(colnames(x) == i)) |> unlist(), drop = FALSE],
  replications = 100000
)
print(bm.matrix)
save(bm.matrix, file = "bm.matrix.RData")


x.dims <- c(1000,100,4)
x <- array(1:prod(x.dims), x.dims)
idx <- list(1:100, c(TRUE, TRUE, TRUE, FALSE))
dims <- c(1, 3)
all(sb_x.array(x, idx, dims) == abind::asub(x, idx, dims))
bm.array <- benchmark(
  "subsets" = sb_x.array(x, idx, dims),
  "base R + abind" = abind::asub(x, idx, dims),
  replications = 10000
)
print(bm.array)
save(bm.array, file = "bm.array.RData")


n <- 1e5
x <- data.frame(
  a = seq_len(n),
  b = sample(letters, size = n, replace = TRUE),
  c = seq_len(n) * -1,
  d = sample(rev(letters), size = n, replace = TRUE)
)
bm.df <- benchmark(
  "subsets" = sb_x.data.frame(x, 1:1000, c("a", "a")),
  "base R" = x[1:1000, lapply(c("a", "a"), \(i) which(colnames(x) == i)) |> unlist(), drop = FALSE],
  replications = 1e4
)
print(bm.df)
save(bm.df, file = "bm.df.RData")
