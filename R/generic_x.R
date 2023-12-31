#' Method to Extract, Exchange, or Duplicate Subsets of an Object
#'
#' @description
#' This is an S3 Method to extract, exchange,
#' or duplicate (i.e. replicate indices x times) indices of an object.
#'
#' @param x see \link{subsets_classes}.
#' @param i,lvl,row,col,idx,dims,rcl,filter,vars See \link{subsets_indx_args}. \cr
#' Duplicates are allowed, resulting in duplicated indices. \cr
#' An empty index selection results in an empty object of length 0. \cr
#' @param drop logical.
#'  * For factors: If `drop = TRUE`, unused levels are dropped, if `drop = FALSE` they are not dropped.
#'  * For lists: if `drop = TRUE`, selecting a single element will give the simplified result,
#'  like using `[[]]`. If `drop = FALSE`, a list is always returned regardless of the number of elements.
#' @param rat logical, indicating if attributes should be returned with the sub-setted object.
#' See Details section for more info.
#' @param ... further arguments passed to or from other methods.
#'
#'
#' @details
#' \bold{One the \code{rat} argument} \cr
#' Most `[` - methods strip most (but not all) attributes. \cr
#' If `rat = FALSE`, this default behaviour is preserved,
#' for compatibility with special classes. This is the fastest option. \cr
#' If `rat = TRUE`,
#' attributes from `x` missing after sub-setting are re-assigned to `x`.
#' Already existing attributes after sub-setting will not be overwritten. \cr
#' There is no `rat` argument for data.frame-like object:
#' their attributes will always be preserved. \cr
#' NOTE: In the following situations, the `rat` argument will be ignored,
#' as the attributes necessarily have to be dropped:
#'  * when `x` is a list, AND `drop = TRUE`, AND a single element is selected.
#'  * when `x` is a matrix or array, and sub-setting is done through the `i` argument.
#'
#' @returns
#' Returns a copy of the sub-setted object.
#'
#'
#'
#' @examples
#' 
#' # atomic objects ====
#' 
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
#' sb_x(obj, n(1:3, 1:2), c(1,3))
#' sb_x(obj, rcl = n(1:3, NULL, 1:2))
#' # above 2 lines are equivalent to obj[1:3, , 1:2, drop = FALSE]
#' sb_x(obj, i = \(x)x>5)
#' # above is equivalent to obj[obj > 5]
#' 
#' #############################################################################
#' 
#' 
#' # lists ====
#' 
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_x(obj, 1) # obj[1]
#' sb_x(obj, 1, drop = TRUE) # obj[[1]]
#' sb_x(obj, 1:2) # obj[1:2]
#' sb_x(obj, is.numeric) # obj[sapply(obj, is.numeric)]
#' # for recursive indexing, see sb_rec()
#' 
#' #############################################################################
#' 
#' 
#' # factors ====
#' 
#' obj <- factor(rep(letters[1:5], 2))
#' sb_x(obj, lvl = c("a", "a"))
#' # above is equivalent to obj[lapply(c("a", "a"), \(i) which(obj == i)) |> unlist()]
#' 
#' #############################################################################
#' 
#' 
#' # data.frame-like objects ====
#' 
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
sb_x.default <- function(x, i, ..., rat = FALSE) {
  elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  if(rat) {
    x <- .fix_attr(x[elements], attributes(x))
  } else{ x <- x[elements] }
  return(x)
}


#' @rdname sb_x
#' @export
sb_x.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rat = FALSE) {
  
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
  
  if(rat) {
    x <- .fix_attr(x[row, col, drop = FALSE], attributes(x))
  } else{ x <- x[row, col, drop = FALSE] }
  
  return(x)
}


#' @rdname sb_x
#' @export
sb_x.array <- function(x, idx = NULL, dims = NULL, rcl = NULL, i = NULL, ..., rat = FALSE) {
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    return(x[elements])
  }
  if(!is.null(rcl)) {
    if(length(dim(x)) != 3) stop("`rcl` only applicable for arrays with exactly 3 dimensions")
    if(!is.list(rcl) || length(rcl) != 3) stop("`rcl` must be a list of length 3")
    return(.sb3d_x(x, rcl[[1]], rcl[[2]], rcl[[3]], rat = rat))
  }

  if(rat) {
    x <- .fix_attr(.arr_x(x, idx, dims, abortcall = sys.call()), attributes(x))
  } else {
    x <- .arr_x(x, idx, dims, abortcall = sys.call())
  }
  return(x)
  
}

#' @rdname sb_x
#' @export
sb_x.factor <- function(x, i = NULL, lvl = NULL, drop = FALSE, ..., rat = FALSE) {
  
  .check_args_factor(i, lvl, drop, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    if(rat) {
      x <- .fix_attr(x[elements, drop = drop], attributes(x))
    } else{ x <- x[elements, drop = drop] }
    return(x)
  }
  if(!is.null(lvl)) {
    indx <- .lvl2indx(lvl, x, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    if(rat) {
      x <- .fix_attr(x[indx, drop = drop], attributes(x))
    } else{ x <- x[indx, drop = drop] }
    return(x)
  }
}


#' @rdname sb_x
#' @export
sb_x.list <- function(x, i, drop = FALSE, ..., rat = FALSE) {
  
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop("`drop` must be either `TRUE` or `FALSE`")
  }
  
  elements <- .indx_make_element(i, x, is_list = TRUE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  n.i <- length(elements)
  if(n.i == 1 && drop) {
    x <- x[[elements]]
  } else {
    if(rat) x <- .fix_attr(x[elements], attributes(x))
    if(!rat) x <- x[elements]
  }
  return(x)
}


#' @rdname sb_x
#' @export
sb_x.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ...
) {
  
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
  
  x <- collapse::ss(x, row, col, check = FALSE)
  
  names(x) <- make.names(names(x), unique = TRUE)
  return(x)
}


