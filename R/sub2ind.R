#' Convert Subscripts (Array Indices) to Flat Indices, and Vice-Versa.
#'
#' @description
#' The `sub2ind()` function converts subscripts
#' (i.e. coordinate-like indices) to flat (1D) indices,
#' and the `ind2sub()` does the opposite. \cr
#' The `sub2ind()` is thus the opposite of \link{arrayInd},
#' and `ind2sub` is merely a convenient wrapper around \link{arrayInd}. \cr
#' \cr
#' Both functions are written to be memory-efficient.
#'
#'
#' @param coords an integer matrix, giving the coordinate indices (subscripts) to convert. \cr
#' Each row is an index, and each column is the dimension. \cr
#' The number of columns of `coords` must be equal to the length of `x.dim`.
#' @param x.dim an integer vector giving the dimensions of the array in question. I.e. `dim(x)`.
#' @param x.len the length of the object, i.e. `length(x)`. This is needed to evaluate the dimensions.
#' @param ind an integer vector, giving the flat position indices to convert.
#' @param checks logical, indicating if arguments checks should be performed. \cr
#' Defaults to `TRUE`. Can be set to `FALSE` for minor speed improvements, but not recommended.
#' 
#'
#'
#' @returns
#' The converted indices.
#'
#'
#' @examples
#' x.dim <- c(1000, 10, 4, 4)
#' x.len <- prod(x.dim)
#' x <- array(1:x.len, x.dim)
#' x[4,3,2, 1]
#' x[1,2,3,4]
#' coords <- rbind(c(4:1), 1:4)
#' ind <- sub2ind(coords, x.dim, x.len)
#' print(ind)
#' x[ind] == c(x[4, 3, 2, 1], x[1, 2, 3, 4]) # TRUE, TRUE
#' ind2sub(ind, x.dim, x.len)
#' 


#' @rdname sub2ind
#' @export
sub2ind <- function(coords, x.dim, x.len, checks = TRUE) {
  n <- length(x.dim)
  
  if(isTRUE(checks)) {
    if(n == 0) {
      stop("`length(x.dim) == 0`")
    }
    
    if(!is.numeric(x.dim) || !is.numeric(coords)) {
      stop("`x.dim` and `coords` must both be numeric")
    }
    
    if(!isTRUE(collapse::fncol(coords) == n)) {
      stop("`ncol(coords) != length(x.dim)`")
    }
    
    if(!isTRUE(x.len == prod(x.dim))) {
      stop("length of object does not correspond to the given dimensions")
    }
  }
  
  ind2 <- coords[, 1]
  
  if(n > 1) {
    for(i in seq.int(n, 2)) ind2 <- ind2 + prod(x.dim[seq_len(i - 1)]) * (coords[, i] - 1)
  }
  
  return(ind2)
}


#' @rdname sub2ind
#' @export
ind2sub <- function(ind, x.dim, x.len) {
  if(!isTRUE(x.len == prod(x.dim))) {
    stop("length of object does not correspond to the given dimensions")
  }
  return(arrayInd(ind, x.dim))
}


