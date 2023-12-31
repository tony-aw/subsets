#' Generate Integer Sequence From a Range of Names
#'
#' @description
#' Generate integer sequence from a range of names.
#'
#' @param names a character vector of names. \cr
#' Duplicate names, empty names, or a character vector of length zero are not allowed.
#' @param start the name giving the starting index of the sequence
#' @param end the name giving the ending index of the sequence
#' @param inv logical, if TRUE,
#' the indices of all names EXCEPT the names of the specified sequence will be given.
#'
#' @returns
#' An integer vector.
#'
#' @examples
#'
#' x <- data.frame(a = 1:10, b = letters[1:10], c = factor(letters[1:10]), d = -1:-10)
#' ind <- seq_names(colnames(x), "b", "d")
#' sb_x(x, col = ind)
#'

#' @rdname seq_names
#' @export
seq_names <- function(names, start, end, inv = FALSE) {
  
  n <- length(names)
  if(n == 0) {
    stop("no names given")
  }
  if(collapse::anyv(names, "")) {
    stop("empty names not allowed")
  }
  if(anyDuplicated(names)) {
    stop("duplicate names not allowed")
  }
  if(!start %in% names) {
    stop("`start` not in `names`")
  }
  if(!end %in% names) {
    stop("`end` not in `names`")
  }
  if(!isTRUE(inv) & !isFALSE(inv)) {
    stop("`inv` must be `TRUE` or `FALSE`")
  }

  range <- c(collapse::whichv(names, start), collapse::whichv(names, end))
  range <- range[1]:range[2]
  out <- range
  if(inv) { out <- seq_len(n)[-range] }
  return(as.integer(out))
}


