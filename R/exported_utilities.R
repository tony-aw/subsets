#' Exported Utilities
#'
#' @description
#' Exported utilities
#'
#' @param x a vector, vector-like object, factor, data.frame, data.frame-like object, or a list.
#' @param i any of the following:
#'  * a vector of length 0, in which case an empty object is returned.
#'  * a strictly positive vector with indices
#'  (duplicates are allowed, resulting in duplicated indices).
#'  * logical vector (without `NA`s) of the same length as `x` giving the indices to select.
#'  * a character vector of index names
#'  (duplicates are allowed, resulting in duplicated indices).
#'  If an object has multiple indices with the given name,
#'  all the corresponding indices will be selected/duplicated/re-arranged.
#'  * function that returns a logical vector giving the element indices to select.
#' @param xnames names or dimension names
#' @param xsize length or dimension size
#'
#' @returns
#' The subsetted object.
#' 
#' 
#' @examples
#' x <- 1:10
#' names(x) <- letters[1:10]
#' indx_x(1:5, x, names(x), length(x))
#' indx_rm(1:5, x, names(x), length(x))
#'
#'

#' @rdname exported_uilities
#' @export
indx_x <- function(i, x, xnames, xsize) {
  if(is.null(i)) return(base::quote(expr = ))
  if(is.function(i)) return(which(lapply(x, i) |> unname() |> unlist()))
  if(length(i)==0) return(numeric(0)) 
  if(is.character(i)) {
    out <- lapply(
      i, \(i) which(xnames == i)
    ) |> unlist()
    return(out)
  }
  if(is.logical(i)) return(which(i))
  if(is.numeric(i)) return(i)
}


#' @rdname exported_uilities
#' @export
indx_rm <- function(i, x, xnames, xsize) {
  if(is.null(i)) return(base::quote(expr = ))
  if(is.function(i)) return(which(!(lapply(x, i) |> unname() |> unlist())))
  if(length(i)==0) return(seq_len(xsize)) 
  if(is.character(i)) {
    return(which(!(xnames %in% i)))
  }
  if(is.logical(i)) return(which(!i))
  if(is.numeric(i)) return(seq_len(xsize)[-i])
}
