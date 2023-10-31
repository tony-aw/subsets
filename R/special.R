#' Specialized subsetting functions
#'
#' @description
#' The `sb_rec()` function performs recursive subsetting of lists. \cr
#' The `sb_str()` function subsets a single string as-if it is an iterable object. \cr
#'
#' @param lst a list.
#' @param str a single string.
#' @param ind an integer vector, giving the positions of the string to subset.
#' @param rec a vector of length `p`,
#' such that `lst[[rec]]` is equivalent to `lst[[rec[1]]...[[rec[p]]]`,
#' providing all but the final indexing results in a list.
#'
#' @details
#' The `sb_str()` function is several times faster
#' (the exact ratio depends on the string length)
#' than using something like the following:
#' 
#' ```{r, eval = FALSE}
#' 
#' x <- strsplit(x, "") |> unlist()
#' x <- paste0(x[ind])
#' paste(x, collapse = "")
#' 
#' ```
#' 
#'
#' @returns
#' The subsetted object.
#'
#'
#'
#' @examples
#' lst <- list(
#'   A = list(
#'     A = list(A = "AAA", B = "AAB"),
#'     B = list(A = "ABA", B = "ABB")
#'   ),
#'   B = list(
#'     A = list(A = "BAA", B = "BAB"),
#'     B = list(A = "BBA", B = "BBB")
#'   )
#' )
#' sb_rec(lst, c(1,2,2)) # this gives "ABB"
#' sb_rec(lst, c(2,2,1)) # this gives "BBA"
#' 
#' x <- "hello"
#' sb_str(x, 5:1) # this gives "olleh"
#' sb_str(x, c(1:5, 5)) # this gives "helloo"
#' sb_str(x, c(2:5)) # this gives "ello"
#' sb_str(x, seq(1, 5, by = 2)) # this gives "hlo"
#' substring(x, 1:5, 3:7) # "hel" "ell" "llo" "lo"  "o" 
#'

#' @name sb_special
NULL



#' @rdname sb_special
#' @export
sb_rec <- function(lst, rec) {
  return(lst[[rec]])
}

#' @rdname sb_special
#' @export
sb_str <- function(str, ind) {
  if(any(ind < 1)) {
    stop("ind must be a strictly positive integer")
  }
  ind <- as.integer(ind - 1)
  return(.rcpp_sb_str(str, ind))
}