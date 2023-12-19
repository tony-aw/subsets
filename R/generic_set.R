#' Method to Modify Subsets of an Object By Reference
#'
#' @description
#' This is an S3 Method to replace or transform a subset of an object
#' \bold{By Reference}. \cr \cr
#' 
#'
#' @param x see \link{subsets_classes}.
#' @param i,row,col,idx,dims,rcl,filter,vars See \link{subsets_indx_args}. \cr
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
#' 
#' @section Reference Semantics:
#' 
#' The `sb_set()` method modifies an object \bold{by reference}. \cr
#' The advantage of this is that less memory is required to modify objects. \cr
#' But modifying an object by reference does have 3 potential disadvantages. \cr
#' \cr
#' First, the coercion rules are slightly different: see \link{subsets_classes}. \cr
#' \cr
#' Second, if 2 or more variables refer to exactly the same object,
#' changing one variable also changes the other ones. \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' x <- y <- 1:16
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' modifies not just `x`, but also `y`. \cr
#' \cr
#' Third, the second consequence is true even if one of the variables is locked
#' (see \link[base]{bindingIsLocked}). \cr
#' I.e. the following code,
#' 
#' ```{r eval = FALSE}
#' tinycodet::import_LL("tinycodet", "%<-c%")
#' x <- 1:16
#' y %<-c% x
#' sb_set(x, i = 1:6, rp = 8)
#' ```
#' modifies both `x` and `y` without error,
#' even though `y` is a locked constant. \cr \cr
#' 
#' 
#' 
#' 
#' @returns
#' Returns: VOID. This method modifies the object by REFERENCE. \cr
#' Do NOT use assignment like `x <- sb_set(x, ...)`. \cr
#' Since this function returns void, you'll just get NULL. \cr \cr
#'
#'
#' @examples
#' 
#' 
#' # atomic objects ====
#' 
#' gen_mat <- function() {
#'   obj <- matrix(1:16, ncol = 4)
#'   colnames(obj) <- c("a", "b", "c", "a")
#'   return(obj)
#' }
#' 
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
#' #############################################################################
#' 
#' # data.frame ====
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_set(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' )
#' print(obj)
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' obj <- sb_coe(obj, vars = is.numeric, v = as.numeric)
#' str(obj)
#' sb_set(obj,
#'   filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # SAFE: coercion performed by sb_coe(); so no warnings
#' ) 
#' print(obj)
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_set(
#'   obj, vars = is.numeric,
#'   tf = sqrt # SAFE: row=NULL & filter = NULL, so coercion performed
#' )
#' str(obj)
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
  
  return(.sb_set_atomic(x, elements, rp = rp, tf = tf))
}


#' @rdname sb_set
#' @export
sb_set.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    return(.sb_set_atomic(x, elements, rp = rp, tf = tf))
    
  }


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
  
  return(.sb_set_atomic(x, elements, rp = rp, tf = tf))

}


#' @rdname sb_set
#' @export
sb_set.array <- function(x, idx = NULL, dims = NULL, rcl = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    return(.sb_set_atomic(x, elements, rp = rp, tf = tf))
  }
  
  if(!is.null(rcl)) {
    elements <- .sb3d_set_elements(x, row = rcl[[1]], col = rcl[[2]], lyr = rcl[[3]])
    return(.sb_set_atomic(x, elements, rp = rp, tf = tf))
  }
  
  if(is.null(i)) {
    x.dim <- dim(x)
    ndims <- length(x.dim)
    .arr_check(x, idx, dims, ndims, abortcall = sys.call())
    lst <- .arr_lst_grid(x, ndims, idx, dims, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    coords <- sub2coord(lst, x.dim)
    elements <- coord2ind(coords, x.dim, checks = FALSE)
    
    return(.sb_set_atomic(x, elements, rp = rp, tf = tf))
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
    x, row = NULL, col = NULL, filter = NULL, vars = NULL,
    ..., rp, tf
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
  
  if(is.null(col)) col <- collapse::seq_col(x)
  col <- as.integer(col)
  
  
  if(is.null(row)) {
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- lapply(collapse::ss(x, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp)
    data.table::set(x, j = col, value = rp)
  }
  
  if(!is.null(row)) {
    row <- as.integer(row)
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      rp <- lapply(collapse::ss(x, i = row, j = col, check = FALSE), tf)
    }
    .check_rp_df(rp)
    data.table::set(x, i = row, j = col, value = rp)
  }
  
  return(invisible(NULL))
}



#' @keywords internal
#' @noRd
.sb_set_atomic <- function(x, elements, rp, tf) {
  
  n.i <- length(elements)
  
  if(n.i == 0) return(invisible(NULL))
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp = tf(x[elements])
  }
  
  .check_rp_atomic(rp, n.i)
  collapse::setv(x, v = as.integer(elements), R = rp, vind1 = TRUE)
  return(invisible(NULL))
}
