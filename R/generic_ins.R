#' Methods to insert new values before or after an index along a dimension
#'
#' @description
#' The `sb_before()` method
#' inserts new values before some position along a dimension. \cr
#' The `sb_after()` method
#' inserts new values after some position along a dimension. \cr
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param new the new value(s). The type of object depends on `x`:
#'  * For vector-like objects, `new` can be any vector-like object.
#'  However, if one wished the added values in `new` to be named,
#'  it is advised to ensure `new` is the same type of object as `x`.
#'  (I.e. use matrix with column names for `new` when appending/inserting columns to matrix `x`.)
#'  * For factors, `new` must be a factor.
#'  * For lists, `new` must be a (possible named) list.
#'  * For data.frame-like objects, `new` must be a data.frame.
#' @param pos a strictly positive single integer scalar (so no duplicates),
#' giving the position along the dimension (specified in `margin`),
#' before or after which the new values are added.
#' @param margin a single scalar, giving the dimension along which to add new values.
#' @param .attr a list,
#' giving additional potentially missing attributes to be added to the returned object. \cr
#' By default, R's \link[base]{c} function concatenates AND strips attributes (for good reasons). \cr
#' In the `attr` argument, the attributes of the merged object can be specified. \cr
#' Only attributes that are actually missing AFTER insertion will be added,
#' thus preventing overwriting existing attributes like names. \cr
#' Defaults to the attributes of `x`. \cr
#' If `NULL`, no attributes will be added post-insert. \cr
#' If speed is important, `NULL` is the best option (but then attributes won't be preserved). \cr
#' @param ... further arguments passed to or from other methods.
#'
#' 
#'
#' @returns
#' Returns a copy of the appended object.
#'
#'
#'
#'
#' @examples
#'
#' # vector-like objects ====
#' x <- matrix(1:20 , ncol = 4)
#' print(x)
#' new <- -1 * x
#' sb_before(x, new, 1)
#' sb_before(x, new, 2)
#' sb_after(x, new, 1)
#' sb_after(x, new, 2)
#'
#'
#' # lists ====
#' x <- as.list(1:5)
#' new <- lapply(x, \(x)x*-1)
#' print(x)
#' sb_before(x, new)
#' sb_after(x, new)
#'
#' 
#' # factors ====
#' x <- factor(letters)
#' new <- factor("foo")
#' sb_before(x, new)
#' sb_after(x, new)
#' 
#' 
#' # data.frame-like objects ====
#' x <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' new <- data.frame(e = 101:110)
#' sb_before(x, new, 2)
#' sb_after(x, new, 2)
#' new <- x[1,]
#' sb_before(x, new, 1)
#' sb_after(x, new, 1)
#' 
#' 
#' 

#' @rdname sb_in
#' @export
sb_before <- function(x, ...) {
  UseMethod("sb_before", x)
}

#' @rdname sb_in
#' @export
sb_after <- function(x, ...) {
  UseMethod("sb_after", x)
}

#' @rdname sb_in
#' @export
sb_before.default <- function(x, new, pos = 1, .attr = attributes(x), ...) {
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_prepend(pos, n)) {
    out <- c(new, x)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- c(x[seq_len(pos-1)], new, x[seq.int(pos, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}

#' @rdname sb_in
#' @export
sb_after.default <- function(x, new, pos = length(x), .attr = attributes(x), ...) {
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_postpend(pos, n)) {
    out <- c(x, new)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  out <- c(x[seq_len(pos)], new, x[seq.int(pos+1, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}


#' @rdname sb_in
#' @export
sb_before.factor <- function(x, new, pos = 1, .attr = attributes(x), ...) {
  
  if(!is.factor(new)) {
    stop("`new` must be a (possibly named) factor")
  }
  
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_prepend(pos, n)) {
    out <- c(new, x)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  out <- c(x[seq_len(pos-1)], new, x[seq.int(pos, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}

#' @rdname sb_in
#' @export
sb_after.factor <- function(x, new, pos = length(x), .attr = attributes(x), ...) {
  
  if(!is.factor(new)) {
    stop("`new` must be a (possibly named) factor")
  }
  
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_postpend(pos, n)) {
    out <- c(x, new)
    out <- .fix_attr(out, .attr)
    return(out)
  }

  out <- c(x[seq_len(pos)], new, x[seq.int(pos+1, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}


#' @rdname sb_in
#' @export
sb_before.list <- function(x, new, pos = 1, .attr = attributes(x), ...) {
  
  if(!is.list(new)) {
    stop("`new` must be a (possibly named) list")
  }
  
  n <- length(x)
  .check_in(pos,  n, abortcall = sys.call())
  
  if(.is_prepend(pos, n)) {
    out <- c(new, x)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- c(x[seq_len(pos-1)], new, x[seq.int(pos, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}



#' @rdname sb_in
#' @export
sb_after.list <- function(x, new, pos = length(x), .attr = attributes(x), ...) {
  
  if(!is.list(new)) {
    stop("`new` must be a (possibly named) list")
  }
  
  n <- length(x)
  .check_in(pos, n, abortcall = sys.call())
  
  if(.is_postpend(pos, n)) {
    out <- c(x, new)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- c(x[seq_len(pos)], new, x[seq.int(pos+1, n)])
  out <- .fix_attr(out, .attr)
  return(out)
}


#' @rdname sb_in
#' @export
sb_before.array <- function(x, new, margin, pos = 1, .attr = attributes(x), ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  n <- dim(x)[[margin]]
  .check_in(pos, n, abortcall = sys.call())
  return(.sb_in_dim_before(x, margin, pos, new, .attr, abortcall = sys.call()))
}

#' @rdname sb_in
#' @export
sb_after.array <- function(x, new, margin, pos = dim(x)[margin], .attr = attributes(x), ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  
  n <- dim(x)[[margin]]
  .check_in(pos, n, abortcall = sys.call())
  return(.sb_in_dim_after(x, margin, pos, new, .attr, abortcall = sys.call()))
}

#' @rdname sb_in
#' @export
sb_before.data.frame <- function(x, new, margin, pos = 1, .attr = attributes(x), ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  
  if(!is.data.frame(new)) {
    stop("`new` must be a data.frame-like object")
  }

  if(margin == 1) {
    abind <- rbind
    ss <- function(x, ind) collapse::ss(x, i = ind)
    n <- collapse::fnrow(x)
  }
  if(margin == 2){ 
    abind <- cbind
    ss <- function(x, ind) collapse::ss(x, j = ind)
    n <- collapse::fncol(x)
  }
  
  if(.is_prepend(pos, n)) {
    out <- abind(new, x)
    out <- .fix_attr(out, .attr)
    colnames(out) <- make.names(colnames(out), unique = TRUE)
    return(out)
  }
  out <- abind(
    ss(x, seq_len(pos-1)),
    new,
    ss(x, seq.int(pos, n))
  )
  out <- .fix_attr(out, .attr)
  colnames(out) <- make.names(colnames(out), unique = TRUE)
  return(out)
}

#' @rdname sb_in
#' @export
sb_after.data.frame <- function(x, new, margin, pos = collapse::fdim(x)[margin], .attr = attributes(x), ...) {
  
  if(length(margin)>1 || !is.numeric(margin)) {
    stop("`margin` must be a single integer scalar")
  }
  
  if(!is.data.frame(new)) {
    stop("`new` must be a data.frame-like object")
  }
  
  if(margin == 1) {
    abind <- rbind
    ss <- function(x, ind) collapse::ss(x, i = ind)
    n <- collapse::fnrow(x)
  }
  if(margin == 2){ 
    abind <- cbind
    ss <- function(x, ind) collapse::ss(x, j = ind)
    n <- collapse::fncol(x)
  }
  
  if(.is_postpend(pos, n)) {
    out <- abind(x, new)
    out <- .fix_attr(out, .attr)
    colnames(out) <- make.names(colnames(out), unique = TRUE)
    return(out)
  }
  out <- abind(
    ss(x, seq_len(pos)),
    new,
    ss(x, seq.int(pos+1, n))
  )
  out <- .fix_attr(out, .attr)
  colnames(out) <- make.names(colnames(out), unique = TRUE)
  return(out)
}

#' @keywords internal
#' @noRd
.check_in <- function(pos, n, abortcall) {
  error.txt2 <- simpleError("`pos` must be a strictly positive integer scalar", call = abortcall)
  
  if(!is.numeric(pos) || length(pos) != 1) {
    stop(error.txt2)
  }
  if(any(pos < 1)) {
    stop(error.txt2)
  }
  if(any(pos > n)) {
    stop(simpleError("subscript out of bounds", call = abortcall))
  }
  
}


#' @keywords internal
#' @noRd
.is_prepend <- function(pos, n) {
  if(pos == 1 || n == 1) { 
      return(TRUE)
  }
  return(FALSE)
}


#' @keywords internal
#' @noRd
.is_postpend <- function(pos, n) {
  if(pos == n || n == 1) {
      return(TRUE)
  }
  return(FALSE)
}


#' @keywords internal
#' @noRd
.fix_attr <- function(out, .attr) {
  if(is.null(.attr)) {
    return(out)
  }
  missing.attrnms <- setdiff(names(.attr), names(attributes(out)))
  attributes(out) <- c(attributes(out), .attr[missing.attrnms])
  return(out)
}


#' @keywords internal
#' @noRd
.sb_in_dim_before <- function(x, margin, pos, new, .attr, abortcall) {
  
  n.x <- dim(x)[[margin]]
  
  if(.is_prepend(pos, n.x)) {
    out <- .abind(new, x, ._along = margin)
    out <- .fix_attr(out, .attr)
    return(out)
  }

  out <- .abind(
    .asub(x, idx = seq_len(pos-1), dims = margin),
    new,
    .asub(x, idx = seq.int(pos, n.x), dims = margin),
    ._along = margin
  )
  out <- .fix_attr(out, .attr)
  return(out)

}

.sb_in_dim_after <- function(x, margin, pos, new, .attr, abortcall) {
  
  n.x <- dim(x)[[margin]]
  
  if(.is_postpend(pos, n.x)) {
    out <- .abind(x, new, ._along = margin)
    out <- .fix_attr(out, .attr)
    return(out)
  }
  
  out <- .abind(
    .asub(x, idx = seq_len(pos), dims = margin),
    new,
    .asub(x, idx = seq.int(pos+1, n.x), dims = margin),
    ._along = margin
  )
  out <- .fix_attr(out, .attr)
  return(out)

}