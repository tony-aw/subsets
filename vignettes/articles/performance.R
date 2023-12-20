# speed tests

library(subsets)
library(ggplot2)
library(data.table)

# bench::mark ====

x.mat <- matrix(seq_len(1000*1000), ncol = 1000)
colnames(x.mat) <- sample(c(letters, NA), 1000, TRUE)
sel.rows <- 1:100
sel.cols <- rep(sample(letters[1:13]), 10)
bm.matrix <- bench::mark(
  "subsets" = sb_x.matrix(x.mat, sel.rows, sel.cols),
  "base R" = x.mat[sel.rows, lapply(sel.cols, \(i) which(colnames(x.mat) == i)) |> unlist(), drop = FALSE],
  min_iterations = 1e4
)
summary(bm.matrix, relative = TRUE)
autoplot(bm.matrix) + ggtitle("matrix")
print(bm.matrix)
save(bm.matrix, file = "bm.matrix.RData")


x.dims <- c(1000, 900, 4)
x.3d <- array(1:prod(x.dims), x.dims)
sel.rows <- 1:900
sel.lyrs <- c(TRUE, FALSE, TRUE, FALSE)
all(
  sb_x.array(x.3d, rcl = n(sel.rows, NULL, sel.lyrs)) ==
    abind::asub(x.3d, idx = list(sel.rows, sel.lyrs), dims = c(1,3))
)
bm.3d <- bench::mark(
  "subsets" =  sb_x.array(x.3d, rcl = n(sel.rows, NULL, sel.lyrs)),
  "base R + abind" = abind::asub(x.3d, idx = list(sel.rows, sel.lyrs), dims = c(1,3)),
  min_iterations = 1e4
)
summary(bm.3d, relative = TRUE)
autoplot(bm.3d) + ggtitle("3d")
print(bm.3d)
save(bm.3d, file = "bm.3d.RData")


n <- 1e5
chrmat <- matrix(
  sample(letters, n*400, replace = TRUE), ncol = 400
)
intmat <- matrix(
  seq.int(n*400), ncol = 400
)
x <- cbind(chrmat, intmat) |> as.data.frame()
rm(list = c("chrmat", "intmat"))
colnames(x) <- make.names(colnames(x), unique = TRUE)
sel.cols <- rep(sample(names(x), 10), 4)
sel.rows <- 1:1000
bm.df <- bench::mark(
  "subsets" = sb_x.data.frame(x, sel.rows, sel.cols),
  "base R" = x[sel.rows, match(sel.cols, names(x)), drop = FALSE],
  min_iterations = 1e4
)
summary(bm.df, relative = TRUE)
autoplot(bm.df) + ggtitle("data.frame")
print(bm.df)
save(bm.df, file = "bm.df.RData")

x <- as.data.table(x)
tempfun <- function(x, i, j) {
  x <- collapse::ss(x, i, j, check = TRUE)
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}
bm.dt <- bench::mark(
  "subsets" = sb_x.data.frame(x, sel.rows, sel.cols),
  "data.table + collapse" = tempfun(x, sel.rows, match(sel.cols, names(x))),
  min_iterations = 1e4
)
summary(bm.dt, relative = TRUE)
autoplot(bm.dt) + ggtitle("data.table")
print(bm.dt)
save(bm.dt, file = "bm.dt.RData")
