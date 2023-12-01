
#' @keywords internal
#' @noRd
.sb3d_x <- function(x, row = NULL, col = NULL, lyr = NULL, rat = FALSE) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, allow_dupl = TRUE, inv = FALSE, abortcall = sys.call())
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  if(is.null(lyr)) lyr <- base::quote(expr = )
  
  if(rat) {
    x <- .fix_attr(x[row, col, lyr, drop = FALSE], attributes(x))
  } else{ x <- x[row, col, lyr, drop = FALSE] }
  
  return(x)
}


#' @keywords internal
#' @noRd
.sb3d_rm <- function(x, row = NULL, col = NULL, lyr = NULL, rat = FALSE) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, allow_dupl = FALSE, inv = TRUE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, allow_dupl = FALSE, inv = TRUE, abortcall = sys.call())
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, allow_dupl = FALSE, inv = TRUE, abortcall = sys.call())
  }
  
  if(is.null(row)) row <- base::quote(expr = )
  if(is.null(col)) col <- base::quote(expr = )
  if(is.null(lyr)) lyr <- base::quote(expr = )
  
  if(rat) {
    x <- .fix_attr(x[row, col, lyr, drop = FALSE], attributes(x))
  } else{ x <- x[row, col, lyr, drop = FALSE] }
  
  return(x)
}


#' @keywords internal
#' @noRd
.sb3d_mod <- function(x, row = NULL, col = NULL, lyr = NULL, rp, tf) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col, lyr)) {
    return(x)
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  if(is.null(lyr)) lyr <- seq_len(dim(x)[3])
  
  if(!missing(tf)) {
    if(!is.function(tf)) stop("`tf` must be a function")
    rp <- tf(x[row, col, lyr, drop = FALSE])
  }
  
  .check_rp_atomic(rp, (length(row) * length(col) * length(lyr)))
  x[row, col, lyr] <- rp
  
  return(x)
  
}

#' @keywords internal
#' @noRd
.sb3d_set_elements <- function(x, row, col, lyr) {
  
  if(!is.null(row)) {
    row <- .indx_make_dim(row, x,  1, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(col)) {
    col <- .indx_make_dim(col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  if(!is.null(lyr)) {
    lyr <- .indx_make_dim(lyr, x,  3, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call())
  }
  
  if(.any_empty_indices(row, col, lyr)) {
    return(invisible(NULL))
  }
  
  if(is.null(row)) row <- collapse::seq_row(x)
  if(is.null(col)) col <- collapse::seq_col(x)
  if(is.null(lyr)) lyr <- seq_len(dim(x)[3])
  
  coords <- as.matrix(data.table::CJ(lyr, col, row, sorted = FALSE))[, 3:1, drop = FALSE]
  elements <- coord2ind(coords, dim(x), checks = FALSE)
  return(elements)
}
