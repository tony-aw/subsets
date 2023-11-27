#' Supported S3 classes, With Important Comments
#'
#' @description
#' The `sb_` methods support the following categories of S3 classes: \cr
#' 
#'  * atomic vector classes (vectors, matrices, arrays);
#'  * factors;
#'  * lists;
#'  * data.frame-like classes (data.frames, data.tables, tibbles, tidytables).
#' 
#' These categories of classes are quite different from each other.
#' It is vital to understand their differences,
#' in order to use the 'subsets' package properly. \cr
#' Thus this help page will explain their important properties and important differences. \cr
#' \cr
#' 
#' @section Atomic Vector Classes: 
#' `r .mybadge_coercion("YES")` \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' The atomic vector is the most basic class type. \cr
#' Matrices and Arrays are just atomic vectors with dimension attributes. \cr
#' All elements in an atomic vector class
#' must have the same atomic type
#' ("logical", "integer", "numeric", "complex", "character" and "raw"). \cr
#' Therefore, the \link{sb_coe} method will coerce the entirety of an atomic vector,
#' NOT just a subset of it. \cr
#' \cr
#' @section Factor Class:
#' `r .mybadge_coercion("NO")` \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' Factors have the property that they can only store values that are defined in
#' their "levels" attribute; other values are not allowed, and thus result in `NA`s. \cr
#' Thus And \link{sb_mod} does NOT coerce replacement values for factors!
#' This is quite different from the atomic vector classes,
#' which can store an infinite variety of values
#' (provided they are of the same atomic type). \cr
#' Due to these properties, there is NO `sb_set()` method for factors. \cr
#' \cr
#' @section List Class:
#' `r .mybadge_coercion("depends")` \cr
#' `r .mybadge_require_unique_names("NO")` \cr
#' Lists are recursive objects (i.e. they can be nested),
#' and they do not actually store values but rather store reference to other objects. \cr
#' Therefore \link{sb_rec} method can be used to access recursive subsets of a list,
#' no matter how deep/low in hierarchy it is in the list. \cr
#' \cr
#' @section Data.frame-like classes:
#' `r .mybadge_coercion("NO")` \cr
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
#'  * The \link{sb_mod} method \bold{does NOT auto-coerce columns} in data.frame-like classes. \cr
#'  I.e.: replacing/transforming a value in an integer (`int`) column to become `1.5`,
#'  will NOT coerce the column to the decimal type (`dbl`);
#'  instead, the replacement value `1.5` is coerced to integer `1`. \cr
#'  The user must explicitly coerce the columns themselves
#'  (see the \link{sb_coe} method, or the `coe` argument in \link{sb_mod}).
#'  * The \link{sb_x} and \link{sb_rm} methods always automatically conserve all attributes
#'  (though names are adjusted accordingly, of course),
#'  they are never stripped, unlike the other classes.
#'  * Giving a data.frame-like object with non-unique column names to the `sb_`-methods
#'  returns an error.
#'  Also, duplicating columns with \link{sb_x}
#'  will automatically adjust the column names to make them unique. \cr \cr
#' 
#' 


#' @rdname aaa1_subsets_classes
#' @name aaa1_subsets_classes
#' @aliases subsets_classes
NULL
