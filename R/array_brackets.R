

#' @keywords internal
#' @noRd
.arr_length <- function(x, lst, dims) {
  x.dim <- dim(x)
  spec.dimsize <- collapse::vlengths(lst[dims])
  unspec.dimsize <- x.dim[-dims]
  return(prod(spec.dimsize, unspec.dimsize))
}


#' @keywords internal
#' @noRd
.arr_check <- function(x, idx, dims, ndims, abortcall) {
  if(!is.list(idx) || !is.numeric(dims)) {
    stop(simpleError("`idx` must be a list, and `dims` must be a integer vector", call = abortcall))
  }
  if(length(idx) != length(dims)) {
    stop(simpleError("`length(idx) != length(dims)`", call = abortcall))
  }
}

#' @keywords internal
#' @noRd
.arr_x <- function(x, idx, dims, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- rep(list(base::quote(expr = )), ndims)
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim(
      idx[[i]], x, dim.L=dims[i], allow_dupl = TRUE, inv = FALSE, abortcall
    )
  }
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_rm <- function(x, idx, dims, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- rep(list(base::quote(expr = )), ndims)
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim(
      idx[[i]], x, dim.L=dims[i], allow_dupl = FALSE, inv = TRUE, abortcall
    )
  }
  return(do.call(function(...)x[..., drop = FALSE], lst))
}


#' @keywords internal
#' @noRd
.arr_tf <- function(x, idx, dims, tf, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- rep(list(base::quote(expr = )), ndims)
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim(
      idx[[i]], x, dim.L=dims[i], allow_dupl = FALSE, inv = FALSE, abortcall
    )
  }
  
  temp.fun <- function(...) {
    x[...] <- tf(x[..., drop = FALSE])
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}

#' @keywords internal
#' @noRd
.arr_repl <- function(x, idx, dims, rp, abortcall) {
  
  ndims <- length(dim(x))
  .arr_check(x, idx, dims, ndims, abortcall)
  
  lst <- rep(list(base::quote(expr = )), ndims)
  for(i in seq_along(dims)) {
    lst[[dims[i]]] <- .indx_make_dim(
        idx[[i]], x, dim.L=dims[i], allow_dupl = FALSE, inv = FALSE, abortcall
      )
  }
  
  temp.fun <- function(...) {
    n.rp <- length(rp)
    if(n.rp != .arr_length(x, lst, dims) && n.rp != 1) {
      stop(simpleError("recycling not allowed", call = abortcall))
    }
    x[...] <- rp
    return(x)
  }
  out <- do.call(temp.fun, lst)
  return(out)
}

