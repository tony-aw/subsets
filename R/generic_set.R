#' Method to Modify Subsets of an Object By Reference
#'
#' @description
#' This is an S3 Method to replace or transform a subset of an object BY REFERENCE. \cr
#' WARNING: this method is currently experimental!
#' 
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param i,lvl,row,col,idx,dims,filter,vars See \link{subsets_indx_args}. \cr
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
#' Note that the `tf` argument is not available for factors: this is intentional. \cr
#' \cr
#' 
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
  
  if(!missing(rp)){
    collapse::setv(x, elements, rp, vind1 = TRUE)
    return(invisible(NULL))
  }
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    collapse::setv(x, elements, tf(x[elements]), vind1 = TRUE)
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
  
  if(!missing(rp)) {
    if(!is.list(rp)) stop("`rp` must be a list")
    collapse::setv(x, elements, rp, vind1 = TRUE, xlist = TRUE)
    return(invisible(NULL))
  }
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    collapse::setv(x, elements, lapply(x[elements], tf), vind1 = TRUE, xlist = TRUE)
    return(invisible(NULL))
  }
}

#' @rdname sb_set
#' @export
sb_set.factor <- function(x, i = NULL, lvl = NULL, ..., rp) {
  
  .check_args_factor(i, lvl, drop = FALSE, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    n.i <- length(elements)
    if(n.i == 0)  return(invisible(NULL))
    collapse::setv(x, elements, rp, vind1 = TRUE)
    return(invisible(NULL))

  }
  if(!is.null(lvl)) {
    if(length(lvl) == 0) return(x)
    .prep_relevel(lvl, rp, x, sys.call())
    
    collapse::setv(x, elements, rp, vind1 = FALSE)
    return(invisible(NULL))
    
  }
}


#' @rdname sb_set
#' @export
sb_set.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    if(length(elements) == 0) return(invisible(NULL))
    
    if(!missing(rp)) {
      collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
      return(invisible(NULL))
    }
    if(!missing(tf)) {
      collapse::setv(x, v = as.integer(elements), R = tf(x[elements]), vind1 = TRUE)
      return(invisible(NULL))
    }
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
    
    
    coords <- as.matrix(data.table::CJ(col, row, sorted = FALSE))[, 2:1]
    elements <- sub2ind(coords, dim(x), length(x), checks = FALSE)
    
    if(!missing(rp)) {
      collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
      return(invisible(NULL))
    }
    if(!missing(tf)) {
      collapse::setv(x, v = as.integer(elements), R = tf(x[elements]), vind1 = TRUE)
      return(invisible(NULL))
    }
  }
  
}

#' @rdname sb_set
#' @export
sb_set.array <- function(x, idx = NULL, dims = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    if(length(elements) == 0) return(invisible(NULL))
    
    if(!missing(rp)) {
      collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
      return(invisible(NULL))
    }
    if(!missing(tf)) {
      collapse::setv(x, v = as.integer(elements), R = tf(x[elements]), vind1 = TRUE)
      return(invisible(NULL))
    }
  }
  
  if(is.null(i)) {
    ndims <- length(dim(x))
    .arr_check(x, idx, dims, ndims, abortcall = sys.call())
    lst <- .arr_lst_grid(x, ndims, idx, dims, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    
    coords <- as.matrix(do.call(data.table::CJ, rev(lst)))
    coords <- coords[, ndims:1]
    elements <- sub2ind(coords, dim(x), length(x), checks = FALSE)
    
    if(!missing(rp)) {
      collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
      return(invisible(NULL))
    }
    if(!missing(tf)) {
      collapse::setv(x, v = as.integer(elements), R = tf(x[elements]), vind1 = TRUE)
      return(invisible(NULL))
    }
  }
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
    return(x)
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  
  row <- as.integer(row)
  col <- as.integer(col)
  x <- data.table::copy(x)
  
  if(!missing(rp)) {
    if(!is.list(rp)) stop("`rp` must be a data.frame-like object or a list")
    data.table::set(x, row, col, rp)
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    value <- collapse::ss(x, row, col, check = FALSE)
    value <- lapply(value, tf)
    data.table::set(x, row, col, value)
  }
  
  return(invisible(NULL))
}


