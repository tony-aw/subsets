#' Convert Subscripts to Coordinates, Coordinates to Flat Indices, and Vice-Versa
#'
#' @description
#' `sub2coord()` converts a list of integer subscripts to an integer matrix of coordinates. \cr
#' `coord2ind()` converts an integer matrix of coordinates to an integer vector of flat indices. \cr
#' `ind2coord()` converts an integer vector of flat indices to an integer matrix of coordinates. \cr
#' `coord2sub()` converts an integer matrix of coordinates to a list of integer subscripts. \cr
#' \cr
#' All of these functions are written to be memory-efficient. \cr
#' The `coord2ind()` is thus the opposite of \link{arrayInd},
#' and `ind2coord` is merely a convenient wrapper around \link{arrayInd}. \cr
#' \cr
#' 
#'
#'
#' @param subs a list of integer subscripts. \cr
#' The first element of the list corresponds to the first dimension,
#' the second element to the second dimensions, and so forth. \cr
#' The length of `subs` must be equal to the length of `x.dim`. \cr
#' One cannot give an empty subscript;
#' instead fill in something like `seq_len(dim(x)[margin])`.
#' @param coords an integer matrix, giving the coordinate indices (subscripts) to convert. \cr
#' Each row is an index, and each column is the dimension. \cr
#' The number of columns of `coords` must be equal to the length of `x.dim`.
#' @param x.dim an integer vector giving the dimensions of the array in question. I.e. `dim(x)`.
#' @param x.ndims the number of dimensions of the object, i.e. `length(dim(x))`.
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
#' ind <- coord2ind(coords, x.dim, x.len)
#' print(ind)
#' x[ind] == c(x[4, 3, 2, 1], x[1, 2, 3, 4]) # TRUE, TRUE
#' ind2coord(ind, x.dim, x.len)
#' 


#' @name sub2ind
NULL

#' @rdname sub2ind
#' @export
sub2coord <- function(subs, x.ndims) {
  if(length(subs) != x.ndims) {
    stop("`length(subs) != length(x.ndims)`")
  }
  coords <- as.matrix(do.call(data.table::CJ, c(rev(subs), list(sorted = FALSE))))
  coords <- coords[, x.ndims:1, drop = FALSE]
  return(coords)
}


#' @rdname sub2ind
#' @export
coord2sub <- function(coords) {
  subs <- data.table::as.data.table(coords) |> as.list()
  names(subs) <- NULL
  subs <- lapply(subs, collapse::funique)
  return(subs)
}


#' @rdname sub2ind
#' @export
coord2ind <- function(coords, x.dim, x.len, checks = TRUE) {
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
ind2coord <- function(ind, x.dim, x.len) {
  if(!isTRUE(x.len == prod(x.dim))) {
    stop("length of object does not correspond to the given dimensions")
  }
  return(arrayInd(ind, x.dim))
}


