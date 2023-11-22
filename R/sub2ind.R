#' Convert Subscripts to Coordinates, Coordinates to Flat Indices, and Vice-Versa
#'
#' @description
#' `sub2coord()` converts a list of integer subscripts to an integer matrix of coordinates. \cr
#' `coord2ind()` converts an integer matrix of coordinates to an integer vector of flat indices. \cr
#' `ind2coord()` converts an integer vector of flat indices to an integer matrix of coordinates. \cr
#' `coord2sub()` converts an integer matrix of coordinates to a list of integer subscripts.
#' Note that the `coord2sub()` function performs a very simple (one might even say naive) conversion. \cr
#' \cr
#' All of these functions are written to be memory-efficient. \cr
#' The `coord2ind()` is thus the opposite of \link{arrayInd},
#' and `ind2coord` is merely a convenient wrapper around \link{arrayInd}. \cr
#' \cr
#' 
#'
#'
#' @param sub a list of integer subscripts. \cr
#' The first element of the list corresponds to the first dimension (rows),
#' the second element to the second dimensions (columns),
#' etc. \cr
#' The length of `sub` must be equal to the length of `x.dim`. \cr
#' One cannot give an empty subscript;
#' instead fill in something like `seq_len(dim(x)[margin])`. \cr
#' NOTE: The `coord2sub()` function does not support duplicate subscripts.
#' @param coord an integer matrix, giving the coordinate indices (subscripts) to convert. \cr
#' Each row is an index, and each column is the dimension. \cr
#' The first columns corresponds to the first dimension,
#' the second column to the second dimensions,
#' etc. \cr
#' The number of columns of `coord` must be equal to the length of `x.dim`. \cr
#' @param x.dim an integer vector giving the dimensions of the array in question. I.e. `dim(x)`.
#' @param ind an integer vector, giving the flat position indices to convert.
#' @param checks logical, indicating if arguments checks should be performed. \cr
#' Defaults to `TRUE`. Can be set to `FALSE` for minor speed improvements, but not recommended.
#' 
#' 
#' @details
#' The S3 classes in 'R' use the standard Linear Algebraic convention,
#' as in academic fields like Mathematics and Statistics,
#' in the following sense: \cr
#'  * vectors are \bold{column} vectors (i.e. vertically aligned vectors);
#'  * index counting starts at `1`;
#'  * rows are the first dimension/subscript, columns are the second dimension/subscript, etc.
#' 
#' Thus, the orientation of flat indices in, for example, a 4 by 4 matrix, is as follows:
#' 
#' ```{r echo = FALSE, comment=NA}
#' matrix(1:16, ncol =4)
#' ```
#' and, the subscript `[1,2]` refers to the first row and the second column.
#' In a 4 by 4 matrix, subscript `[1,2]` would corresponds to flat index `5`. \cr
#' The functions desribed here thus follow also this convention. \cr
#' \cr
#' 
#' 
#'
#' @returns
#' For `sub2coord()` and `ind2coord()`: \cr
#' Returns an integer matrix of coordinates
#' (with properties as described in argument `coord`). \cr
#' \cr
#' For `coord2ind()`: \cr
#' Returns an integer vector of flat indices
#' (with properties as described in argument `ind`). \cr
#' \cr
#' For `coord2sub()`: \cr
#' Returns a list of integer subscripts
#' (with properties as described in argument `sub`) \cr
#' \cr
#'
#'
#' @examples
#' x.dim <- c(1000, 10, 4, 4)
#' x.len <- prod(x.dim)
#' x <- array(1:x.len, x.dim)
#' sub <- list(c(4,1), c(3,2), c(2,3), c(1,4))
#' coord <- sub2coord(sub, x.dim)
#' print(coord)
#' ind <- coord2ind(coord, x.dim)
#' print(ind)
#' all(x[ind] == c(x[c(4,1), c(3,2), c(2,3), c(1,4)])) # TRUE
#' coord2 <- ind2coord(ind, x.dim)
#' print(coord)
#' all(coord == coord2) # TRUE
#' sub2 <- coord2sub(coord2)
#' sapply(1:4, \(i)sub2[[i]]==sub[[i]]) |> all() # TRUE


#' @name sub2ind
NULL

#' @rdname sub2ind
#' @export
sub2coord <- function(sub, x.dim) {
  n <- length(x.dim)
  if(length(sub) != n) {
    stop("`length(sub) != length(x.dim)`")
  }
  coord <- as.matrix(do.call(data.table::CJ, c(rev(sub), list(sorted = FALSE))))
  coord <- coord[, n:1, drop = FALSE]
  return(coord)
}


#' @rdname sub2ind
#' @export
coord2sub <- function(coord) {
  sub <- data.table::as.data.table(coord) |> as.list()
  names(sub) <- NULL
  sub <- lapply(sub, collapse::funique)
  return(sub)
}


#' @rdname sub2ind
#' @export
coord2ind <- function(coord, x.dim, checks = TRUE) {
  n <- length(x.dim)
  
  if(isTRUE(checks)) {
    if(n == 0) {
      stop("`length(x.dim) == 0`")
    }
    
    if(!is.numeric(x.dim) || !is.numeric(coord)) {
      stop("`x.dim` and `coord` must both be numeric")
    }
    
    if(!isTRUE(collapse::fncol(coord) == n)) {
      stop("`ncol(coord) != length(x.dim)`")
    }
  }
  
  ind2 <- coord[, 1]
  
  if(n > 1) {
    for(i in seq.int(n, 2)) ind2 <- ind2 + prod(x.dim[seq_len(i - 1)]) * (coord[, i] - 1)
  }
  
  return(ind2)
}


#' @rdname sub2ind
#' @export
ind2coord <- function(ind, x.dim) {
  return(arrayInd(ind, x.dim))
}


