#' Index Arguments in the Generic Sub-setting Methods
#'
#' @description
#' There are 4 types of arguments that can be used
#' in the generic methods of 'subsets' to specify the indices to perform operations on:
#' 
#'  * `i`: to specify flat (i.e. dimensionless) indices.
#'  * `row, col`: to specify rows and/or columns in tabular objects.
#'  * `idx, dims`: to specify indices of arbitrary dimensions in arrays.
#'  * `filter, vars`: to specify rows and/or columns specifically in data.frame-like objects. \cr \cr
#' 
#' 
#' @section Argument i:
#' `r .mybadge("class: vector-like", "class-vector--like-red.svg")` \cr
#' `r .mybadge("class: factor", "class-factor-red.svg")` \cr
#' `r .mybadge("class: list", "class-list-red.svg")` \cr
#' 
#' 
#' Any of the following can be specified for argument `i`:
#' 
#'  * `NULL`, only for multi-dimensional objects or factors,
#'  when specifying the other arguments (i.e. dimensional indices or factor levels.)
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with indices.
#'  * a \bold{logical vector} (without `NA`s!),
#'  of the same length as `x`,
#'  giving the indices to select for the operation.
#'  * a \bold{character} vector of index names.
#'  If an object has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#'  * a \bold{function} that returns a logical vector,
#'  giving the element indices to select for the operation.
#'
#' 
#' Using the `i` arguments corresponds to doing something like the following:
#' 
#' ```
#'  sb_x(x, i = i) ==> x[i]
#'  sb_rm(x, i = i) ==> remove x[i]
#'  sb_mod(x, i = i, rp = rp) ==> x[i] <- rp
#'  sb_mod(x, i = i, tf = tf) ==> x[i] <- tf(x[i])
#'  
#' ```
#'
#' @section Arguments row, col:
#' `r .mybadge("class: matrix", "class-matrix-red.svg")` \cr
#' `r .mybadge("class: data.frame-like", "class-data.frame--like-red.svg")` \cr
#' 
#' Any of the following can be specified for the arguments `row` / `col`:
#' 
#' * `NULL` (default), corresponds to a missing argument,
#'  which results in ALL of the indices in this dimension being selected for the operations.
#'  * a vector of length 0,
#'  in which case no indices are selected for the operation (i.e. empty selection).
#'  * a \bold{strictly positive integer} vector with dimension indices to select for the operation.
#'  * a \bold{logical} vector (without `NA`s!) of the same length as the corresponding dimension size,
#'  giving the indices of this dimension to select for the operation.
#'  * a \bold{character} vector of index names.
#'  If an object has multiple indices with the given name,
#'  ALL the corresponding indices will be selected for the operation.
#' 
#' NOTE: The arguments `row` and `col` will be ignored if `i` is specified.
#' 
#' Using the `row, col` arguments corresponds to doing something like the following:
#' 
#' ```
#'  sb_x(x, row = row, col = col) ==> x[row, col, drop = FALSE]
#'  sb_rm(x, row = row, col = col) ==> remove x[row, col, drop = FALSE]
#'  sb_mod(x, row = row, col = col, rp = rp) ==> x[row, col] <- rp
#'  sb_mod(x, row = row, col = col, tf = tf) ==> x[row, col, drop = FALSE] <- tf(x[row, col, drop = FALSE])
#'  
#' ```
#' 
#' @section Arguments idx, dims:
#' `r .mybadge("class: array", "class-array-red.svg")` \cr
#'  
#' `idx` must be a list of indices. \cr
#' `dims` must be an integer vector of the same length as `idx`,
#' giving the dimensions to which the indices given in `idx` correspond to. \cr
#' The elements of `idx` follow the same rules as the rules for `row` and `col`,
#' EXCEPT one should not fill in `NULL`. \cr
#' NOTE: The arguments `idx` and `dims` will be ignored if `i` is specified.
#' 
#' Using the `idx, dims` arguments,
#' corresponds to doing something like the following,
#' here using an example of a 4-dimensional array:
#' 
#' ```
#' sb_mod(x, list(1:10, 1:5), c(1, 3), rp = rp) ==> x[1:10, , 1:5, ] <- rp
#' sb_mod(x, list(1:10, 1:5), c(1, 3), tf = tf) ==> x[1:10, , 1:5, ] <- tf(x[1:10, , 1:5, , drop = FALSE])
#' 
#' ```
#' 
#' 
#' @section Arguments filter, vars:
#' `r .mybadge("class: data.frame-like", "class-data.frame--like-red.svg")` \cr
#' 
#' `filter` must be a one-sided formula
#' with a single logical expression using the column names of the data.frame,
#' giving the condition which observation/row indices should be selected for the operation. \cr
#' For example,
#' to perform an operation on the rows for which column `height > 2` and for which column `sex != "female"`,
#' specify the following formula: \cr
#' 
#' ```
#' ~ (height > 2) & (sex != "female")
#' ```
#' 
#' `vars` must be a function that returns a logical vector,
#' giving the column indices to select for the operation. \cr
#' For example, to select all numeric columns, specify `vars = is.numeric`. \cr
#' \cr
#' 
#' @section Argument lvl:
#' `r .mybadge("class: factor", "class-factor-red.svg")` \cr
#' 
#' For this argument, the names of the levels of `x` can be given,
#' selecting the corresponding indices for the operation. \cr
#' \cr
#' 
#' @section Duplicates (for Names, Integers, and Levels):
#' Generally speaking, duplicate names, integers, or levels are NOT allowed in index selection. \cr
#' The exception is the \link{sb_x} method,
#' as that method can be used for duplicating indices. \cr
#' \cr
#' 
#' @section Disallowed Combinations of Index Arguments:
#' One cannot specify `i` and `row`/`col`/`lvl`/`idx`/`dims` simultaneously.
#' It's either `i`, or the other arguments. \cr
#' One cannot specify `row` and `filter` simultaneously.
#' It's either one or the other. Similarly,
#' one cannot specify `col` and `vars` simultaneously. \cr
#' In the above cases it holds that if one set is specified, the other is set is ignored. \cr
#' \cr
#' 
#' 


#' @rdname aaa1_subsets_indx_args
#' @name aaa1_subsets_indx_args
#' @aliases subsets_indx_args
NULL
