#' Method to extract, exchange, or duplicate indices of an object
#'
#' @description
#' This is an S3 Method to extract, exchange, or duplicate indices of an object.
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param i `sb_x(x, i = i)` corresponds to \code{x[i]}. \cr
#' Any of the following can be given here:
#'  * `NULL`, only for multi-dimensional objects or factors.
#'  `NULL` results in the entire object being returned.
#'  * a vector of length 0, in which case an empty object of length 0 is returned.
#'  * a strictly positive integer vector with indices
#'  (duplicates are allowed, resulting in duplicated indices).
#'  * logical vector (without `NA`s) of the same length as `x`
#'  giving the indices to extract.
#'  * a character vector of index names
#'  (duplicates are allowed, resulting in duplicated indices).
#'  If an object has multiple indices with the specified name,
#'  all the corresponding indices will be extracted/exchanged/duplicated.
#'  * function that returns a logical vector giving the element indices to select.
#' @param lvl names of the levels of `x`,
#' for which the corresponding indices are to be extracted, exchanged, or duplicated. \cr
#' Duplicates are allowed, resulting in duplicated indices.
#' @param drop logical.
#'  * For factors: If `TRUE`, unused levels are dropped, if `FALSE` they are not dropped.
#'  * For lists: is `TRUE`, selecting a single element will give the simplified result,
#'  like using `[[]]`. If `FALSE`, a list is always returned regardless of the number of elements.
#' @param row,col `sb(x, row, col)` corresponds to \code{x[row, col, drop = FALSE]}. \cr
#' Thus `row` = rows, `col` = columns. \cr
#' Any of the following can be given here:
#'  * `NULL` (default), which results in ALL the indices of this dimension being selected,
#'  and the entire object is returned.
#'  * a vector of length 0, in which case an empty object is returned.
#'  * a strictly positive integer vector with dimension indices
#'  (duplicates are allowed, resulting in duplicated indices).
#'  * logical vector (without `NA`s) of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to extract.
#'  * a character vector of index names
#'  (duplicates are allowed, resulting in duplicated indices).
#'  If an object has multiple indices with the given name,
#'  all the corresponding indices will be extracted/exchanged/duplicated.
#' 
#' NOTE: The arguments `row` and `col` will be ignored if `i` is specified.
#' @param filter a one-sided formula with a single logical expression using the column names of the data.frame,
#' giving the condition which observations (rows) should be extracted. \cr
#' For example: \cr
#' to select rows for which column `a > 2` and for which column `b != "a"`,
#' specify the following formula: \cr
#' `~ (a > 2) & (b != "a")`
#' @param vars a function, giving the condition which variables (columns) should be extracted.
#' @param idx,dims arguments to subset arrays:
#'  * `idx`: a list of indices.
#'  * `dims`: a integer vector of the same length as `idx`,
#'  giving the dimensions to which the indices given in `idx` correspond to.
#' 
#' The elements of `idx` follow the same rules as the rules for `row` and `col`,
#' EXCEPT one should not fill in `NULL`. \cr
#' Thus `sb_rm(x, list(1:10, 1:4), c(1, 3))` is equivalent to \code{x[1:10, , 1:4, drop = FALSE]}. \cr
#' NOTE: The arguments `idx` and `dims` will be ignored if `i` is specified.
#' @param ... further arguments passed to or from other methods.
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
#' @returns
#' A copy of the sub-setted object.
#'
#'
#'
#' @examples
#' 
#' # vector-like objects ====
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_x(obj, 1:3, 1:3)
#' # above is equivalent to obj[1:3, 1:3, drop = FALSE]
#' sb_x(obj, i = \(x)x>5)
#' # above is equivalent to obj[obj > 5]
#' sb_x(obj, col = c("a", "a"))
#' # above is equivalent to obj[, lapply(c("a", "a"), \(i) which(colnames(obj) == i)) |> unlist()]
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' sb_x(obj, list(1:3, 1:2, c(1, 3)), 1:3)
#' # above is equivalent to obj[1:3, 1:2, c(1, 3), drop = FALSE]
#' sb_x(obj, i = \(x)x>5)
#' # above is equivalent to obj[obj > 5]
#' 
#' 
#' # lists ====
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_x(obj, 1) # obj[1]
#' sb_x(obj, 1, drop = TRUE) # obj[[1]]
#' sb_x(obj, 1:2) # obj[1:2]
#' sb_x(obj, is.numeric) # obj[sapply(obj, is.numeric)]
#' # for recusive indexing, see sb_rec()
#' 
#' 
#' # factors ====
#' obj <- factor(rep(letters[1:5], 2))
#' sb_x(obj, lvl = c("a", "a"))
#' # above is equivalent to obj[lapply(c("a", "a"), \(i) which(obj == i)) |> unlist()]
#' 
#' 
#' # data.frame-like objects ====
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' print(obj)
#' sb_x(obj, 1:3, 1:3) # obj[1:3, 1:3, drop = FALSE]
#' sb_x(obj, filter = ~ (a > 5) & (c < 19), vars = is.numeric)
#' 
#' 

#' @rdname sb_x
#' @export
sb_x <- function(x, ...) {
  UseMethod("sb_x", x)
}


#' @rdname sb_x
#' @export
sb_x.default <- function(x, i, ...) {
  elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  return(x[elements])
}

#' @rdname sb_x
#' @export
sb_x.factor <- function(x, i = NULL, lvl = NULL, drop = FALSE, ...) {
  
  .check_args_factor(i, lvl, drop, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    return(x[elements, drop = drop])
  }
  if(!is.null(lvl)) {
    indx <- .lvl2indx(lvl, x, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    return(x[indx, drop = drop])
  }
}


#' @rdname sb_x
#' @export
sb_x.list <- function(x, i, drop = FALSE, ...) {
  
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop("`drop` must be either `TRUE` or `FALSE`")
  }
  
  elements <- .indx_make_element(i, x, is_list = TRUE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  
  if(length(elements) == 1 && drop) {
    return(x[[elements]])
  } else { return(x[elements]) }
}

#' @rdname sb_x
#' @export
sb_x.matrix <- function(x, row = NULL, col = NULL, i = NULL, ...) {

  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    return(x[elements])
  }

  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  }
  

  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  return(x[row, col, drop = FALSE])
}

#' @rdname sb_x
#' @export
sb_x.array <- function(x, idx = NULL, dims = NULL, i = NULL, ...) {
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    return(x[elements])
  } else {
    return(.arr_x(x, idx, dims, abortcall = sys.call()))
  }
}

#' @rdname sb_x
#' @export
sb_x.data.frame <- function(x, row = NULL, col = NULL, filter = NULL, vars = NULL, ...) {
  
  .check_args_df(x, row, col, filter, vars, abortcall = sys.call())
  
  if(!is.null(row)) { row <- .indx_make_tableind(
    row, x,  1, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call()
  )}
  if(!is.null(col)) { col <- .indx_make_tableind(
    col, x,  2, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call()
  )}
  
  if(!is.null(filter)) {
    row <- .indx_make_filter(x, filter, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  
  out <- collapse::ss(x, row, col, check = FALSE)
  
  colnames(out) <- make.names(colnames(out), unique = TRUE)
  return(out)
}


