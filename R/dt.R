#' Functional forms of data.table Operations (also work on tidytables)
#'
#' @description
#' Functional forms of special data.table operations - ALL programmatically friendly 
#' (no Non-Standard Evaluation). \cr
#' \cr
#' `dt_aggregate()` aggregates a data.table or tidytable, and returns the aggregated copy. \cr
#' `dt_setcoe()` coercively transforms columns of a data.table or tidytable BY REFERENCE. \cr
#' 
#' 
#' 
#' @param x a `data.table` or `tidytable`.
#' @param f the aggregation function
#' @param col,vars columns to select for coercion; see \link{subsets_indx_args}. \cr
#' Duplicates are not allowed.
#' @param SDcols atomic vector,
#' giving the columns to which the aggregation function `f()` is to be applied on.
#' @param by atomic vector,
#' giving the grouping columns.
#' @param order_by logical (`TRUE` or `FALSE`),
#' indicating if the aggregated result should be ordered by the columns specified in `by`.
#'
#'
#' @returns
#' The sub-setted object.
#'
#'
#'
#' @examplesIf requireNamespace("sf") && requireNamespace("ggplot2")
#' requireNamespace("sf") && requireNamespace("ggplot2")
#' 
#' 
#' # dt_aggregate on sf-data.table ====
#' 
#' x <- sf::st_read(system.file("shape/nc.shp", package = "sf"))
#' x <- data.table::as.data.table(x)
#' 
#' x$region <- ifelse(x$CNTY_ID <= 2000, 'high', 'low')
#' plotdat <- dt_aggregate(
#'   x, SDcols = "geometry", f= sf::st_union, by = "region"
#' )
#' 
#' ggplot2::ggplot(plotdat, aes_pro(geometry = ~ geometry, fill = ~ region)) + 
#'   ggplot2::geom_sf()
#' 
#' #############################################################################
#' 
#' 
#' # dt_setcoe ====
#' 
#' obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_set(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' )
#' str(obj)
#' obj <- data.table::data.table(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' dt_setcoe(obj, vars = is.numeric, f = as.numeric) # integers are now numeric
#' str(obj)
#' sb_set(obj,
#'   filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # SAFE: coercion performed; so no warnings
#' ) 
#' str(obj)
#'
#'
#'
#' #############################################################################
#' 
#' 
#' # dt_setrm ====
#' 
#' obj <- data.table::data.table(
#'   a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
#' )
#' str(obj)
#' dt_setrm(obj, col = 1)
#' str(obj)
#' 
#' obj <- data.table::data.table(
#'   a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10])
#' )
#' str(obj)
#' dt_setrm(obj, vars = is.numeric)
#' str(obj)
#' 

#' @name dt
NULL


#' @rdname dt
#' @export
#' @importFrom data.table .SD .N .I ':='
dt_aggregate <- function(
    x, SDcols = NULL, f, by, order_by = FALSE
) {
  if(!data.table::is.data.table(x)) stop("`x` must be a data.table")
  if(anyDuplicated(names(x))) stop("`x` does not have unique variable names for all columns")
  
  if(!is.atomic(SDcols) || !is.atomic(by)) stop("`SDcols` and `by` must be atomic vectors")
  
  if(isFALSE(order_by)) return(x[, lapply(.SD, f), .SDcols = c(SDcols), by = c(by)])
  if(isTRUE(order_by)) return(x[, lapply(.SD, f), .SDcols = c(SDcols), keyby = c(by)])
  
  stop("`order_by` must be `TRUE` or `FALSE`")
}


#' @rdname dt
#' @export
dt_setcoe <- function(
    x, col = NULL, vars = NULL, f
) {
  
  if(!is.function(f)) stop("`f` must be a function")
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  
  .check_args_df(x, row = NULL, col = col, filter = NULL, vars = vars, abortcall = sys.call())
  
  if(!is.null(col)) {
    col <- .indx_make_tableind(
      col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call()
    )
    col <- names(x)[col]
  }
  
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
    col <- names(x)[col]
  }
  
  if(is.null(col)) col <- names(x)
  
  for(j in col) { # using loop instead of lapply to reduce memory to only one column at a time
    data.table::set(x, j = j, value = f(x[[j]]))
  }
}


#' @rdname dt
#' @export
dt_setrm <- function(x, col = NULL, vars = NULL) {
  if(!data.table::is.data.table(x)) { stop("`x` must be a data.table") }
  
  .check_args_df(x, row = NULL, col = col, filter = NULL, vars = vars, abortcall = sys.call())
  
  if(!is.null(col)) {
    col <- .indx_make_tableind(
      col, x,  2, allow_dupl = FALSE, inv = FALSE, abortcall = sys.call()
    )
    col <- names(x)[col]
  }
  
  if(!is.null(vars)) {
    col <- .indx_make_vars(x, vars, inv = FALSE, abortcall = sys.call())
    col <- names(x)[col]
  }
  
  if(is.null(col) || length(col) == 0) stop("must specify at least one column")
  
  for(j in col) { # using loop instead of lapply to reduce memory to only one column at a time
    data.table::set(x, j = j, value = NULL)
  }
}
