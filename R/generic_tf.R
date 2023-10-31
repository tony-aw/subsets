#' Method to Transform a Subset of an Object
#'
#' @description
#' This is an S3 Method to transform a subset of an object. \cr
#' Note that there is no \code{sb_tf} method for factors.
#' This is intentional; use \link{relevel} instead.
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param i `sb_rp(x, i = i, tf = tf)` corresponds to \code{x[i] <- tf(x[i])}. \cr
#' Any of the following can be given here:
#'  * `NULL`, only for multi-dimensional objects or factors.
#'  `NULL` results in the entire object being transformed.
#'  * a vector of length 0, in which case the original object is returned unchanged.
#'  * a strictly positive numeric vector with indices
#'  (duplicates are NOT allowed).
#'  * logical vector (without `NA`s) of the same length as `x`
#'  giving the indices to transform.
#'  * a character vector of index names
#'  (duplicates are NOT allowed).
#'  If an object has multiple indices with the given name,
#'  all the values of the corresponding indices will be transformed.
#'  * function that returns a logical vector giving the element indices to select.
#' @param row,col `sb(x, row, col, rp = rp)` corresponds to 
#' \code{x[row, col] <- tf(x[row, col, drop = FALSE])}.\cr
#' Thus `row` = rows, `col` = columns. \cr
#' Any of the following can be given here:
#'  * `NULL` (default), which results in ALL of the indices this dimension being transformed.
#'  * a vector of length 0, in which case the original object is returned unchanged.
#'  * a strictly positive integer vector with dimension indices to transform
#'  (duplicates are NOT allowed).
#'  * logical vector (without `NA`s) of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to transform.
#'  * a character vector of index names
#'  (duplicates are NOT allowed).
#'  If an object has multiple indices with the given name,
#'  all the corresponding indices will be transformed.
#' 
#' NOTE: The arguments `row` and `col` will be ignored if `i` is specified.
#' @param filter a one-sided formula with a single logical expression using the column names of the data.frame,
#' giving the condition which observations (rows)  should be transformed.
#' @param vars a function, giving the condition which variables (columns) should be transformed.
#' @param idx,dims arguments to subset arrays:
#'  * `idx`: a list of indices.
#'  * `dims`: a integer vector of the same length as `idx`,
#'  giving the dimensions to which the indices given in `idx` correspond to.
#' 
#' The elements of `idx` follow the same rules as the rules for `row` and `col`,
#' EXCEPT one should not fill in `NULL`. \cr
#' Thus `sb_tf(x, list(1:10, 1:4), c(1, 3), tf = tf)` is equivalent to
#' \code{x[1:10, , 1:4] <- tf(x[1:10, , 1:4, drop = FALSE])}. \cr
#' NOTE: The arguments `idx` and `dims` will be ignored if `i` is specified.
#' @param ... further arguments passed to or from other methods.
#' @param tf the transformation function to use.
#'
#'
#' @details
#' One cannot specify `i` and `row`/`col`/`lvl`/`idx`/`dims` simultaneously.
#' It's either `i`, or the other arguments. \cr
#' \cr
#' One cannot specify `row` and `filter` simultaneously.
#' It's either one or the other. Similarly,
#' one cannot specify `col` and `vars` simultaneously. \cr
#' \cr
#' In the above cases it holds that if one set is specified, the other is set is ignored.
#'
#'
#'
#' @returns
#' A copy of the transformed object.
#'
#'
#' @examples
#'
#' 
#' # vector-like objects ====
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_tf(obj, 1:3, 1:3, tf = \(x)x^2)
#' # above is equivalent to obj[1:3, 1:3] <- (obj[1:3, 1:3, drop = FALSE])^2; obj
#' sb_tf(obj, i = \(x)x>5, tf = \(x)x^2)
#' # above is equivalent to obj[obj > 5] <- (obj[obj > 5])^2; obj
#' sb_tf(obj, col = "a", tf = \(x)x^2) 
#' # above is equivalent to obj[, colnames(obj) %in% "a"] <- (obj[, colnames(obj) %in% "a"])^2; obj
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' sb_tf(obj, list(1:3, 1:2, c(1, 3)), 1:3, tf = \(x)x^2)
#' # above is equivalent to obj[1:3, 1:2, c(1, 3)] <- (obj[1:3, 1:2, c(1, 3), drop = FALSE])^2
#' sb_tf(obj, i = \(x)x<=5, tf = \(x)x^2)
#' # above is equivalent to obj[obj <= 5] <- (obj[obj <= 5])^2
#' 
#' 
#' # lists ====
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_tf(obj, "a", tf =  \(x)x^2)
#' # above is equivalent to obj[names(obj) %in% "a"] <- (obj[names(obj) %in% "a"])^2
#' sb_tf(obj, is.numeric, tf = \(x)x^2)
#' # above is equivalent to obj[sapply(obj, is.numeric)] <- lapply(obj[sapply(obj, is.numeric)], \(x)x^2); obj
#' 
#' 
#' # data.frame-like objects ====
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' print(obj)
#' sb_tf(obj, filter = ~ (a > 5) & (c < 19), vars = is.numeric, tf = \(x)x^2)
#' 
#'

#' @rdname sb_tf
#' @export
sb_tf <- function(x, ...) {
  UseMethod("sb_tf", x)
}


#' @rdname sb_tf
#' @export
sb_tf.default <- function(x, i, ..., tf) {
  
  if(!is.function(tf)) stop("`tf` must be a function")
  
  elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  
  if(length(elements) == 0) return(x)
  
  x[elements] <- tf(x[elements])
  return(x)
}


#' @rdname sb_tf
#' @export
sb_tf.list <- function(x, i, ..., tf) {
  
  if(!is.function(tf)) stop("`tf` must be a function")
  
  elements <- .indx_make_element(i, x, is_list = TRUE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  
  if(length(elements) == 0) {
    return(x)
  } else if(length(elements) == 1) {
    x[[elements]] <- tf(x[[elements]])
    return(x)
  } else {
    x[elements] <- lapply(x[elements], tf)
    return(x)
  }
}

#' @rdname sb_tf
#' @export
sb_tf.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., tf) {
  
  if(!is.function(tf)) stop("`tf` must be a function")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    if(length(elements) == 0) {
      return(x)
    }
    
    x[elements] <- tf(x[elements])
    return(x)
  }
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  x[row, col] <- tf(x[row, col, drop = FALSE])
  return(x)
}


#' @rdname sb_tf
#' @export
sb_tf.array <- function(x, idx = NULL, dims = NULL, i = NULL, ..., tf) {
  
  if(!is.function(tf)) stop("`tf` must be a function")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    
    if(length(elements) == 0) {
      return(x)
    }
    
    x[elements] <- tf(x[elements])
    return(x)
  } else {
    return(.arr_tf(x, idx, dims, tf, abortcall = sys.call()))
  }
}


#' @rdname sb_tf
#' @export
sb_tf.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ..., tf
) {
  
  if(!is.function(tf)) stop("`tf` must be a function")
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col)) {
    return(x)
  }
  
  if(is.null(row)) row <- seq_len(collapse::fnrow(x))
  if(is.null(col)) col <- seq_len(collapse::fncol(x))
  
  row <- as.integer(row)
  col <- as.integer(col)
  
  value <- collapse::ss(x, row, col, check = FALSE)
  value <- lapply(value, tf)
  x <- data.table::copy(x)
  data.table::set(x, row, col, value)
  
  return(x)
}
