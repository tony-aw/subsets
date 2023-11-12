#' Method to Return a Copy of an Object With Modified Subsets
#'
#' @description
#' This is an S3 Method to return a copy of an object with modified subsets.
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
#' Specifying argument `tf` will transform the subset. \cr
#' Specifying `rp` will replace the subset. \cr
#' One cannot specify both `tf` and `rp`. It's either one set or the other. \cr
#' Note that the `tf` argument is not available for factors: this is intentional. \cr
#' 
#' 
#' @returns
#' A copy of the object with replaced/transformed values.
#'
#'
#' @examples
#' 
#' # vector-like objects ====
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_mod(obj, 1:3, 1:3, rp = -1:-9)
#' # above is equivalent to  obj[1:3, 1:3] <- -1:-9; obj
#' sb_mod(obj, i = \(x)x<=5, rp = -1:-5)
#' # above is equivalent to  obj[obj <= 5] <- -1:-5; obj
#' sb_mod(obj, col = "a", rp = -1:-8)
#' # above is equivalent to  obj[, which(colnames(obj) %in% "a")] <- -1:-8; obj
#' 
#' obj <- matrix(1:16, ncol = 4)
#' colnames(obj) <- c("a", "b", "c", "a")
#' print(obj)
#' sb_mod(obj, 1:3, 1:3, tf = \(x) -x)
#' # above is equivalent to  obj[1:3, 1:3] <- -1 * obj[1:3, 1:3]
#' sb_mod(obj, i = \(x)x<=5, tf = \(x) -x)
#' # above is equivalent to  obj[obj <= 5] <- -1:-5; obj
#' sb_mod(obj, col = "a", tf = \(x) -x)
#' # above is equivalent to  obj[, which(colnames(obj) %in% "a")] <- -1:-8; obj
#' 
#' 
#' obj <- array(1:64, c(4,4,3))
#' print(obj)
#' sb_mod(obj, list(1:3, 1:2, c(1, 3)), 1:3, rp = -1:-12)
#' # above is equivalent to obj[1:3, 1:2, c(1, 3)] <- -1:-12
#' sb_mod(obj, i = \(x)x<=5, rp = -1:-5)
#' # above is equivalent to obj[obj <= 5] <- -1:-5
#' 
#' 
#' # lists ====
#' obj <- list(a = 1:10, b = letters[1:11], c = 11:20)
#' print(obj)
#' sb_mod(obj, "a", rp = list(1))
#' # above is equivalent to  obj[["a"]] <- 1; obj
#' sb_mod(obj, is.numeric, rp = list(-1:-10, -11:-20))
#' # above is equivalent to  obj[which(sapply(obj, is.numeric))] <- list(-1:-10, -11:-20); obj
#' 
#' 
#' # data.frame-like objects ====
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' print(obj)
#' sb_mod(obj, vars = is.numeric, rp = data.frame(-1:-10))
#' 
#'
#'
#'

#' @rdname sb_mod
#' @export
sb_mod <- function(x, ...) {
  UseMethod("sb_mod", x)
}


#' @rdname sb_mod
#' @export
sb_mod.default <- function(x, i, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  
  n.i <- length(elements)
  if(n.i == 0) return(x)
  
  if(!missing(rp)){
    if(length(rp) != n.i)  stop("recycling not allowed")
    x[elements] <- rp
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    x[elements] <- tf(x[elements])
  }
  
  
  return(x)
}


#' @rdname sb_mod
#' @export
sb_mod.list <- function(x, i, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  elements <- .indx_make_element(
    i, x, is_list = TRUE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call()
  )
  
  n.i <- length(elements)
  
  if(n.i == 0) {
    return(x)
  }
  
  if(!missing(rp)) {
    if(!is.list(rp)) stop("`rp` must be a list")
    n.rp <- length(rp)
    if(n.i != n.rp && n.rp != 1) stop("recycling not allowed")
    x[elements] <- rp
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    x[elements] <- lapply(x[elements], tf)
  }
  
  return(x)
}

#' @rdname sb_mod
#' @export
sb_mod.factor <- function(x, i = NULL, lvl = NULL, ..., rp) {
  
  .check_args_factor(i, lvl, drop = FALSE, abortcall = sys.call())
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
    n.i <- length(elements)
    if(n.i == 0) return(x)
    n.rp <- length(rp)
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


#' @rdname sb_mod
#' @export
sb_mod.matrix <- function(x, row = NULL, col = NULL, i = NULL, ..., rp, tf) {
  
  if(!missing(rp) && !missing(tf)) stop("cannot specify both `rp` and `tf`")
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(!missing(rp)) {
      if(is.recursive(rp)) stop("`rp` must be non-recursive")
      n.rp <- length(rp)
      if(n.rp != n.i && n.rp != 1) stop("recycling not allowed")
      x[elements] <- rp
    }
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      x[elements] <- tf(x[elements])
    }
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
  
  if(!missing(rp)) {
    if(is.recursive(rp)) stop("`rp` must be non-recursive")
    n.rp <- length(rp)
    if(n.rp != (length(row) * length(col)) && n.rp != 1) stop("recycling not allowed")
    x[row, col] <- rp
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    x[row, col] <- tf(x[row, col, drop = FALSE])
  }
  return(x)
}

#' @rdname sb_mod
#' @export
sb_mod.array <- function(x, idx = NULL, dims = NULL, i = NULL, ..., rp, tf) {
  
  if(!is.null(i)) {
    elements <- .indx_make_element(i, x, is_list = FALSE, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
    n.i <- length(elements)
    if(n.i == 0) return(x)
    if(!missing(rp)) {
      if(is.recursive(rp)) stop("`rp` must be non-recursive")
      n.rp <- length(rp)
      if(n.rp != n.i && n.rp != 1) stop("recycling not allowed")
      x[elements] <- rp
    }
    if(!missing(tf)) {
      if(!is.function(tf)) stop("`tf` must be a function")
      x[elements] <- tf(x[elements])
    }
    return(x)
  }
  
  if(!missing(rp)) {
    if(is.recursive(rp)) stop("`rp` must be non-recursive")
    return(.arr_repl(x, idx, dims, rp, abortcall = sys.call()))
  }
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    return(.arr_tf(x, idx, dims, tf, abortcall = sys.call()))
  }
}


#' @rdname sb_mod
#' @export
sb_mod.data.frame <- function(
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
  
  return(x)
}


