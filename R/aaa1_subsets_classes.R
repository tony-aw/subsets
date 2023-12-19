#' Supported S3 classes, With Important Comments
#'
#' @description
#' The `sb_` generic methods support the following categories of S3 classes: \cr
#' 
#'  * atomic vector classes (vectors, matrices, arrays);
#'  * factors;
#'  * lists;
#'  * the data.frame-like classes data.frames, data.tables, tibbles, tidytables, \cr
#'  and classes that are straight-forward inheritors from these classes
#'  (such as sf-data.frames or sf-data.tables).
#' 
#' These categories of classes are quite different from each other.
#' It is vital to understand their differences,
#' in order to use the 'subsets' package properly. \cr
#' Thus this help page will explain their important properties and important differences. \cr
#' \cr
#' \cr
#' @section Auto-Coercion Rules:
#' \bold{Coercion Semantics} \cr
#' The \link{sb_mod} method
#' modify subsets of an object through a \bold{deep copy}. \cr
#' The \link{sb_set} method and \link{dt_setcoe} function
#' modify subsets of an object \bold{by reference}. \cr
#' These 2 copy semantics - "pass by reference" or "modify copy" - 
#' have slightly different auto-coercion rules. These are explained in this section. \cr
#' Note that the \link{sb_before} and \link{sb_after} methods
#' usually allow coercion for all classes.
#' \cr
#' \cr
#' \bold{Atomic} \cr
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("NO")` \cr
#' Atomic objects are automatically coerced to fit the modified subset values,
#' when modifying through copy. \cr
#' For example, replacing one or multiple values in an integer vector
#' (type `int`)
#' with a decimal number
#' (type `dbl`)
#' will coerce the entire vector to type `dbl`. \cr
#' \cr
#' Replacing or transforming subsets of atomic objects \bold{by reference}
#' does NOT support coercion.
#' Thus, for example, the following code,
#' ```{r eval = FALSE}
#' x <- 1:16
#' sb_set(x, i = 1:6, rp = 8.5)
#' x
#' ```
#' gives `c(rep(8, 6) 7:16)` instead of `c(rep(8.5, 6), 7:16)`,
#' because `x` is of type `integer`, so `rp` is interpreted as type `integer` also. \cr
#' \cr
#' \cr
#' \bold{Factor} \cr
#' `r .mybadge_coercion_through_copy("NO")` \cr
#' Factors only accept values that are part of their levels,
#' and thus do NOT support coercion on modification.
#' There is no mechanism for changing factors by reference at all. \cr
#' Replacing a value with a new value not part of its levels,
#' will result in the replacement value being `NA`. \cr
#' \cr
#' \cr
#' \bold{List} \cr
#' `r .mybadge_coercion_through_copy("depends")` \cr
#' `r .mybadge_coercion_by_ref("depends")` \cr
#' Lists themselves allow complete change of their elements,
#' since lists are merely pointers. \cr
#' This is true regardless if replacement/transformation takes place by reference or through a copy. \cr
#' For example, the following code performs full coercion:
#' ```{r eval = FALSE}
#' x <- list(factor(letters), factor(letters))
#' sb_mod(x, 1, rp = list(1))
#' sb_set(x, 1, rp = list(1)); print(x)
#' ```
#' However, a recursive subset of a list which itself is not a list,
#' follows the coercion rules of whatever class the recursive subset is. \cr
#' For example the following code:
#' ```{r eval = FALSE}
#' x <- list(1:10, 1:10)
#' sb_rec(x, 1) |> sb_mod(i = 1, rp = "a") # coerces to character
#' sb_rec(x, 1) |> sb_set(i = 1, rp = "a"); print(x) # no coercion; "a" replaced with NA.
#' ```
#' transforms recursive subsets according to the - in this case - 
#' atomic auto-coercion rules. \cr
#' \cr
#' \cr
#' \bold{Data.frame-like, when replacing/transforming whole columns} \cr
#' `r .mybadge_coercion_through_copy("YES")` \cr
#' `r .mybadge_coercion_by_ref("YES")` \cr
#' A data.frame-like objects
#' (data.frames, data.tables, tibbles, tidytables, and their sf-equivalents)
#' are actually lists, where each column is itself a list.
#' As such, replacing/transforming whole columns,
#' so `row = NULL` and `filter = NULL`,
#' allows completely changing the type of the column. \cr
#' Note that coercion of columns needs arguments
#' `row = NULL` and `filter = NULL`
#' in the \link{sb_mod} and \link{sb_set} methods;
#' NO auto-coercion will take place when specifying something like `row = 1:nrow(x)`
#' (see next section). \cr
#' \cr
#' \cr
#' \bold{Data.frame-like, when partially replacing/transforming columns} \cr
#' `r .mybadge_coercion_through_copy("NO")` \cr
#' `r .mybadge_coercion_by_ref("NO")` \cr
#' If rows are specified in the \link{sb_mod} and \link{sb_set} methods,
#' and thus not whole columns but parts of columns are replaced or transformed,
#' NO auto-coercion takes place. \cr
#' I.e.: replacing/transforming a value in an integer (`int`) column to become `1.5`,
#' will NOT coerce the column to the decimal type (`dbl`);
#' instead, the replacement value `1.5` is coerced to integer `1`. \cr
#' The `coe` argument in the \link{sb_mod} method
#' allows the user to enforce coercion,
#' even if subsets of columns are replaced/transformed instead of whole columns. \cr
#' Specifically, the `coe` arguments allows the user to specify a coercive function
#' to be applied on the entirety of every column specified in `col` or `vars`;
#' columns outside this subset are not affected. \cr
#' This coercion function is, of course,
#' applied before replacement (`rp`) or transformation (`tf()`). \cr
#' \cr \cr
#' 
#' @section Technical Details:
#' \bold{Atomic} \cr 
#' `r .mybadge_require_unique_names("NO")` \cr
#' The atomic vector is the most basic class type. \cr
#' Matrices and Arrays are just atomic vectors with dimension attributes. \cr
#' All elements in an atomic vector class
#' must have the same atomic type
#' ("logical", "integer", "numeric", "complex", "character" and "raw"). \cr
#' Therefore, the \link{sb_coe} method will coerce the entirety of an atomic vector,
#' NOT just a subset of it. \cr
#' \cr
#' \bold{Factor} \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' Factors have the property that they can only store values that are defined in
#' their "levels" attribute; other values are not allowed, and thus result in `NA`s. \cr
#' Thus And \link{sb_mod} does NOT coerce replacement values for factors!
#' This is quite different from the atomic vector classes,
#' which can store an infinite variety of values
#' (provided they are of the same atomic type). \cr
#' Due to these properties, there is NO `sb_set()` method for factors. \cr
#' \cr
#' \bold{List} \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' Lists are recursive objects (i.e. they can be nested),
#' and they do not actually store values but rather store reference to other objects. \cr
#' Therefore \link{sb_rec} method can be used to access recursive subsets of a list,
#' no matter how deep/low in hierarchy it is in the list. \cr
#' \cr
#' \bold{Data.frame-like} \cr
#' `r .mybadge_require_unique_names("YES")` \cr
#' The data.frame-like objects quite different from the previously named classes. \cr
#' And the different data.frame-like classes also differ from each other quite a bit -
#' especially in terms of sub-setting. \cr
#' The 'subsets' R-package attempts to keep the data.frame methods as class agnostic as possible,
#' through the class agnostic functionality of the 'collapse' and 'data.table' R-packages. \cr
#' These 3 things cause some
#' \bold{important}
#' oddities in how data.frame-like classes are treated differently from the other classes: 
#' 
#'  * Whole-columns will be auto-coerced when replaced/transformed by `sb_mod()`,
#'  but partial columns will NOT be auto-coerced.
#'  * The \link{sb_x} and \link{sb_rm} methods always automatically conserve all attributes
#'  (though names are adjusted accordingly, of course),
#'  they are never stripped, unlike the other classes.
#'  * Giving a data.frame-like object with non-unique column names to the `sb_`-methods
#'  returns an error.
#'  Also, duplicating columns with \link{sb_x}
#'  will automatically adjust the column names to make them unique. \cr \cr
#' 
#' 
#' @examples
#' 
#' # Coercion examples - Atomic ====
#' 
#' x <- 1:16
#' sb_set(x, i = 1:6, rp = 8.5)
#' x
#' 
#' #############################################################################
#' 
#' 
#' # Coercion examples - List ====
#' 
#' x <- list(factor(letters), factor(letters))
#' sb_mod(x, 1, rp = list(1))
#' sb_set(x, 1, rp = list(1)); print(x)
#' x <- list(1:10, 1:10)
#' 
#' sb_rec(x, 1) |> sb_mod(i = 1, rp = "a") # coerces to character
#' sb_rec(x, 1) |> sb_set(i = 1, rp = "a"); print(x) # no coercion; "a" replaced with NA.
#' 
#' #############################################################################
#' 
#' 
#' # Coercion examples - data.frame-like - whole columns ====
#' 
#' # sb_mod():
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_mod(
#'   obj, vars = is.numeric,
#'   tf = sqrt # SAFE: row=NULL & filter = NULL, so coercion performed
#' )
#' 
#' # sb_set():
#' sb_set(
#'   obj, vars = is.numeric,
#'   tf = sqrt # SAFE: row=NULL & filter = NULL, so coercion performed
#' )
#' str(obj)
#' 
#' #############################################################################
#' 
#' 
#' # Coercion examples - data.frame-like - partial columns ====
#' 
#' # sb_mod():
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' 
#' sb_mod(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' ) 
#' sb_mod(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   coe = as.double, tf = sqrt # SAFE: coercion performed
#' )
#' 
#' # sb_set():
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj) # notice that columns "a" and "c" are INTEGER (`int`)
#' sb_set(
#'   obj, filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # WARNING: sqrt() results in `dbl`, but columns are `int`, so decimals lost
#' )
#' print(obj)
#' 
#' obj <- data.frame(a = 1:10, b = letters[1:10], c = 11:20, d = factor(letters[1:10]))
#' str(obj)
#' obj <- sb_coe(obj, vars = is.numeric, v = as.numeric)
#' str(obj)
#' sb_set(obj,
#'   filter = ~ (a >= 2) & (c <= 17), vars = is.numeric,
#'   tf = sqrt # SAFE: coercion performed by sb_coe(); so no warnings
#' ) 
#' print(obj)
#' 
#' 
#' 

#' @rdname aaa1_subsets_classes
#' @name aaa1_subsets_classes
#' @aliases subsets_classes
NULL
