#' Method to Modify Subsets of an Object By Reference
#'
#' @description
#' This is an S3 Method to replace or transform a subset of an object BY REFERENCE. \cr
#' WARNING: this method is currently experimental!
#' 
#'
#' @param x see \link{subsets_classes}.
#' @param i,row,col,idx,dims,filter,vars See \link{subsets_indx_args}. \cr
#' An empty index selection returns the original object unchanged. \cr
#' @param ... further arguments passed to or from other methods.
#' @param tf the transformation function.
#' @param rp an object of somewhat the same type as the selected subset of \code{x},
#' and the same same length as the selected subset of \code{x} or a length of 1.
#' 
#' @details
#' \bold{Transform or Replace} \cr
#' Specifying argument `tf` will transform the subset.
#' Specifying `rp` will replace the subset.
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' Note that there is not `sb_set()` method for factors: this is intentional. \cr
#' \cr
#' 
#' @section Warning:
#' Due to the way replacement or transformation by reference works,
#' types (see \link[base]{typeof}) CANNOT be coerced to another type.
#' Thus, for example, the following code:
#' 
#' ```{r eval = FALSE}
#' x <- 1:16
#' sb_set(x, i = 1:8, R = 8.5)
#' x
#' ```
#' gives `c(rep(8, 8) 9:16)` instead of `c(rep(8.5, 8), 9:16)`,
#' because `x` is of type `integer`, so `R` is interpreted as type `integer` also. \cr
#' 
#' @returns
#' Returns: VOID. This method modifies the object by REFERENCE. \cr
#' Do NOT use assignment like `x <- sb_set(x, ...)`. \cr
#' Since this function returns void, you'll just get NULL. \cr
#'
#'
#' @examples
#' 
#' gen_mat <- function() {
#'   obj <- matrix(1:16, ncol = 4)
#'   colnames(obj) <- c("a", "b", "c", "a")
#'   return(obj)
#' }
#' # vector-like objects ====
#' obj <- obj2 <- gen_mat()
#' obj
#' sb_set(obj, 1:3, 1:3, rp = -1:-9)
#' obj2
#' obj <- obj2 <- gen_mat()
#' obj
#' sb_set(obj, i = \(x)x<=5, rp = -1:-5)
#' obj2
#' obj <- obj2 <- gen_mat()
#' obj
#' sb_set(obj, col = "a", rp = cbind(-1:-4, -5:-8))
#' obj2
#' 
#' obj <- obj2 <- gen_mat()
#' obj
#' sb_set(obj, 1:3, 1:3, tf = \(x) -x)
#' obj2
#' obj <- obj2 <- gen_mat()
#' obj
#' sb_set(obj, i = \(x)x<=5, tf = \(x) -x)
#' obj2
#' obj <- obj2 <- gen_mat()
#' obj
#' sb_set(obj, col = "a", tf = \(x) -x)
#' obj2
#' 
#' 
#' gen_array <- function() {
#'   array(1:64, c(4,4,3))
#' }
#' obj <- gen_array()
#' obj
#' sb_set(obj, list(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
#' obj
#' obj <- gen_array()
#' obj
#' sb_set(obj, i = \(x)x<=5, rp = -1:-5)
#' obj
#' 
#' 
#'

#' @rdname sb_set
#' @export
sb_set <- function(x, ...) {
  UseMethod("sb_set", x)
}


#' @rdname sb_set
#' @export
sb_set.default <- function(x, i, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  
  n.i <- length(elements)
  if(n.i == 0)  return(invisible(NULL))
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i)
  collapse::setv(x, elements, rp, vind1 = TRUE)
  return(invisible(NULL))
}


#' @rdname sb_set
#' @export
sb_set.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    if(length(elements) == 0) return(invisible(NULL))
    
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp = tf(x[elements])
    }
    
    .check_rp_atomic(rp, length(elements))
    collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
    return(invisible(NULL))
    
  }
  
  if(is.null(i)) {
    if(!is.null(row)) {
      row <- .indx_make_dim(row, x,  1, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    }
    if(!is.null(col)) {
      col <- .indx_make_dim(col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    }
    
    if(.any_empty_indices(row, col)) {
      return(invisible(NULL))
    }
    
    if(is.null(row)) row <- collapse::seq_row(x)
    if(is.null(col)) col <- collapse::seq_col(x)
    
    
    coords <- as.matrix(data.table::CJ(col, row, sorted = FALSE))[, 2:1, drop = FALSE]
    elements <- coord2ind(coords, dim(x), checks = FALSE)
    
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- tf(x[row, col, drop = FALSE])
    }
    .check_rp_atomic(rp, (length(row) * length(col)))
    collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
    return(invisible(NULL))
  }
  
}


#' @rdname sb_set
#' @export
sb_set.array <- function(x, idx = NULL, dims = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    if(length(elements) == 0) return(invisible(NULL))
    
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp = tf(x[elements])
    }
    
    .check_rp_atomic(rp, length(elements))
    collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
    return(invisible(NULL))
  }
  
  if(is.null(i)) {
    x.dim <- dim(x)
    ndims <- length(x.dim)
    .arr_check(x, idx, dims, ndims, abortcall = sys.call())
    lst <- .arr_lst_grid(x, ndims, idx, dims, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    coords <- sub2coord(lst, x.dim)
    elements <- coord2ind(coords, x.dim, checks = FALSE)
    
    
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- tf(x[elements])
    }
    if(length(elements) != length(rp)) stop("recycling not allowed")
    collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
    return(invisible(NULL))
    
  }
}


#' @rdname sb_set
#' @export
sb_set.list <- function(x, i, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(
    i, x, is_list = TRUE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call()
  )
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- lapply(x[elements], tf)
  }
  
  .check_rp_list(rp, n.i)
  collapse::setv(x, elements, rp, vind1 = TRUE, xlist = TRUE)
  return(invisible(NULL))
}



#' @rdname sb_set
#' @export
sb_set.data.frame <- function(
    x, row = NULL, col = NULL, filter = NULL, vars = NULL, ..., rp, tf
) {
  
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
    return(invisible(NULL))
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  
  row <- as.integer(row)
  col <- as.integer(col)
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- lapply(collapse::ss(x, row, col, check = FALSE), tf)
  }
  
  .check_rp_df(rp)
  data.table::set(x, row, col, rp)
  return(invisible(NULL))
}


# .check_set_coercion(x, rp) {
#   if(typeof(x) != typeof(rp) stop("type coercion not allowed")
# }