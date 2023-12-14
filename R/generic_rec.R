#' Access Recursive Subsets
#'
#' @description
#' The `sb_rec()` method allows the user to access recursive subsets of lists,
#' and can be combined (i.e. piped) with the generic methods provided by 'subsets'. \cr
#' 
#' @param lst a list, or list-like object.
#' @param rec a vector of length `p`,
#' such that `lst[[rec]]` is equivalent to `lst[[ rec[1] ]]...[[ rec[p] ]]`,
#' providing all but the final indexing results in a list. \cr
#' When on a certain subset level of a nested list,
#' multiple subsets with the same name exist,
#' only the first one will be selected when performing recursive indexing by name,
#' due to the recursive nature of this type of subsetting.
#' 
#' 
#'
#' @returns
#' The sub-setted object.
#'
#'
#'
#' @examples
#' lst <- list(
#'   A = list(
#'     A = list(A = "AAA", B = "AAB"),
#'     A = list(A  = "AA2A", B = "AA2B"),
#'     B = list(A = "ABA", B = "ABB")
#'   ),
#'   B = list(
#'     A = list(A = "BAA", B = "BAB"),
#'     B = list(A = "BBA", B = "BBB")
#'   )
#' )
#' sb_rec(lst, c(1,2,2)) # this gives "AA2B"
#' sb_rec(lst, c("A", "B", "B")) # this gives "ABB"
#' sb_rec(lst, c(2,2,1)) # this gives "BBA"
#' sb_rec(lst, c("B", "B", "A")) # this gives "BBA"
#' 
#' # return a modified copy of the second-lowest level,
#' # where replace "ABB" is replaced with -1:
#' sb_rec(lst, c("A", "B")) |> sb_coe(i = "B", v = as.double) |> sb_mod(i = "B", rp = list(-1)) 
#' 
#' # replace "AAA" with -1 BY REFERENCE:
#' sb_rec(lst, c("A", "A")) |> sb_set(i = "A", rp = list(-1))
#' lst # notice the first element is replaced by -1
#'
#'

#' @name sb_rec
NULL



#' @rdname sb_rec
#' @export
sb_rec <- function(lst, rec) {
  return(lst[[rec]])
}
