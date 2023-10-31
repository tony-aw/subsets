#' Method to Replace Subsets of an Object With Different Values
#'
#' @description
#' This is an S3 Method to replace a subset of an object with different values.
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param i `sb_rp(x, i = i, rp = rp)` corresponds to \code{x[i] <- rp}. \cr
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
#' @param row,col `sb(x, row, col, rp = rp)` corresponds to \code{x[row, col] <- rp}. \cr
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
#' @param lvl the levels of factor `x` to replace with the replacement vector `rp`.
#' Note that if argument `i` is specified instead of `lvl`, re-levelling does not occur,
#' and replacing subsets with new (i.e. unknown) levels will result in NA values.
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
#' Thus `sb_rm(x, list(1:10, 1:4), c(1, 3), rp = rp)` is equivalent to
#' \code{x[1:10, , 1:4] <- rp}. \cr
#' NOTE: The arguments `idx` and `dims` will be ignored if `i` is specified.
#' @param ... further arguments passed to or from other methods.
#' @param rp an object of somewhat the same type as the selected subset of \code{x},
#' and the same same length as the selected subset of \code{x} or a length of 1.
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
#' @returns
#' A copy of the object with replaced values.
#'
#'
#' @examples
#' 
#' # vector-like objects ====
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_rp(obj, 1:3, 1:3, rp = -1:-9)
#' # above is equivalent to  obj[1:3, 1:3] <- -1:-9; obj
#' sb_rp(obj, i = \(x)x<=5, rp = -1:-5)
#' # above is equivalent to  obj[obj <= 5] <- -1:-5; obj
#' sb_rp(obj, col = "a", rp = -1:-8)
#' # above is equivalent to  obj[, which(colnames(obj) %in% "a")] <- -1:-8; obj
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' sb_rp(obj, list(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
#' # above is equivalent to obj[1:3, 1:2, c(1, 3)] <- -1:-12
#' sb_rp(obj, i = \(x)x<=5, rp = -1:-5)
#' # above is equivalent to obj[obj <= 5] <- -1:-5
#' 
#' 
#' # lists ====
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_rp(obj, "a", rp = list(1))
#' # above is equivalent to  obj[["a"]] <- 1; obj
#' sb_rp(obj, is.numeric, rp = list(-1:-10, -11:-20))
#' # above is equivalent to  obj[which(sapply(obj, is.numeric))] <- list(-1:-10, -11:-20); obj
#' 
#' 
#' # data.frame-like objects ====
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' print(obj)
#' sb_rp(obj, vars = is.numeric, rp = data.frame(-1:-10))
#' 
#'
#'
#'

#' @rdname sb_rp
#' @export
sb_rp <- function(x, ...) {
  UseMethod("sb_rp", x)
}


#' @rdname sb_rp
#' @export
sb_rp.default <- function(x, i, ..., rp) {
  
  elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  
  n.i <- length(elements)
  if(n.i == 0) return(x)
  if(length(rp) != n.i) {
    stop("recycling not allowed")
  }
  
  x[elements] <- rp
  return(x)
}


#' @rdname sb_rp
#' @export
sb_rp.list <- function(x, i, ..., rp) {
  
  if(!is.list(rp)) {
    stop("`rp` must be a list")
  }
  
  elements <- .indx_make_element(i, x, is_list = TRUE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  
  n.i <- length(elements)
  
  if(n.i == 0) {
    return(x)
  }
  
  if(n.i != length(rp)) {
    stop("recycling not allowed")
  }
  
  x[elements] <- rp
  return(x)
}

#' @rdname sb_rp
#' @export
sb_rp.factor <- function(x, i = NULL, lvl = NULL, ..., rp) {
  
  .check_args_factor(i, lvl, drop = FALSE, abortcall = sys.call())
  n.rp <- length(rp)
  
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(n.i != n.rp && n.rp != 1) stop("recycling not allowed")
    x[elements] <- rp
    return(x)
  }
  if(!is.null(lvl)) {
    if(length(lvl) == 0) return(x)
    .prep_relevel(lvl, rp, x, sys.call())
    set.lvls <- levels(x)
    set.lvls[set.lvls == lvl] <- rp
    levels(x) <- set.lvls
    return(x)
  }
}


#' @rdname sb_rp
#' @export
sb_rp.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp) {
  
  if(is.recursive(rp)) {
    stop("`rp` must be non-recursive")
  }
  n.rp <- length(rp)
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    n.i <- length(elements)
    
    if(n.i == 0) return(x)
    if(n.rp != n.i && n.rp != 1) {
      stop("recycling not allowed")
    }
    
    x[elements] <- rp
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
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  
  if(n.rp != (length(row) * length(col)) && n.rp != 1) {
    stop("recycling not allowed")
  }
  
  x[row, col] <- rp
  return(x)
}

#' @rdname sb_rp
#' @export
sb_rp.array <- function(x, idx = NULL, dims = NULL, i = NULL, ..., rp) {
  
  if(is.recursive(rp)) {
    stop("`rp` must be non-recursive")
  }
  n.rp <- length(rp)
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    
    n.i <- length(elements)
    
    if(n.i == 0) return(x)
    if(n.rp != n.i && n.rp != 1) {
      stop("recycling not allowed")
    }
    
    x[elements] <- rp
    return(x)
  } else {
    return(.arr_repl(x, idx, dims, rp, abortcall = sys.call()))
  }
}


#' @rdname sb_rp
#' @export
sb_rp.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ..., rp
) {
  
  if(!is.list(rp)) {
    stop("`rp` must be a data.frame-like object or a list")
  }
  
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
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  
  row <- as.integer(row)
  col <- as.integer(col)
  x <- data.table::copy(x)
  data.table::set(x, row, col, as.list(rp))
  
  return(x)
}
