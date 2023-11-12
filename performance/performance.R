# speed tests

library(subsets)

x <- matrix(seq_len(1000*900), ncol = 900)
colnames(x) <- sample(c(letters, NA), 900, TRUE)
bm.matrix <- rbenchmark::benchmark(
  "subsets" = sb_x.matrix(x, 1:100, c("a", "a")),
  "base R" = x[1:100, lapply(c("a", "a"), \(i) which(colnames(x) == i)) |> unlist(), drop = FALSE],
  replications = 1e5,
  order = NULL
)
print(bm.matrix)
save(bm.matrix, file = "bm.matrix.RData")


x.dims <- c(1000, 900, 4)
x <- array(1:prod(x.dims), x.dims)
idx <- list(1:100, c(TRUE, TRUE, TRUE, FALSE))
dims <- c(1, 3)
all(sb_x.array(x, idx, dims) == abind::asub(x, idx, dims))
bm.array <- rbenchmark::benchmark(
  "subsets" = sb_x.array(x, idx, dims),
  "base R + abind" = abind::asub(x, idx, dims),
  replications = 1e4,
  order = NULL
)
print(bm.array)
save(bm.array, file = "bm.array.RData")


n <- 1e6
x <- data.frame(
  a = seq_len(n),
  b = sample(letters, size = n, replace = TRUE),
  c = seq_len(n) * -1,
  d = sample(rev(letters), size = n, replace = TRUE)
)
colsel <- rep("a", 4)
bm.df <- rbenchmark::benchmark(
  "subsets" = sb_x.data.frame(x, 1:1000, colsel),
  "base R" = x[1:1000, match(colsel, names(x))],
  replications = 1e4,
  order = NULL
)
print(bm.df)
save(bm.df, file = "bm.df.RData")
