#' subsets: an Easy Grammar of Subsets
#' 
#' @description
#' subsets: an Easy Grammar of Subsets \cr
#' 
#' 
#' @section Motivation:
#' 
#' Among programming languages,
#' 'R' has perhaps one of the most flexible and comprehensive sub-setting functionality.
#' But with flexibility often comes confusion and (apparent) inconsistencies.
#' And 'R' is no exception. \cr
#' \cr
#' This becomes quite apparent when one reads (online) documents such as
#' \href{https://www.burns-stat.com/pages/Tutor/R_inferno.pdf}{"The R Inferno"}
#' by Patrick Burns,
#' and 
#' \href{https://github.com/ReeceGoding/Frustration-One-Year-With-R}{"Frustration: One Year With R"}
#' by Reece Goding.
#' These documents point out many inconsistencies,
#' and sub-setting related inconsistencies make up a good portion of these documents. \cr
#' \cr
#' To my surprise, there is no comprehensive R-package
#' (as far as I could see at least)
#' that actually attempts to "fix" the subset-related issues laid out in these
#' and other such documents. \cr
#' \cr
#' Famous subset-related R packages such as 'dplyr' and 'data.table'
#' focus almost exclusive on data.frame-like objects,
#' and occasionally even add more frustration in some aspects,
#' like being not very programmatically friendly. \cr
#' \cr
#' Thus, this R package was born. \cr
#' \cr
#' Although this package was somewhat made for people who are new to 'R'
#' (especially when coming from another programming language),
#' and found themselves confused,
#' I trust this package will be useful even for those who are
#' quite experienced in 'R'. \cr
#' \cr
#' 
#' 
#' @section Goal & Properties:
#' 
#' The Goal of the 'subsets' package is not to replace the square-brackets operators per-sé,
#' (see \link[base]{Extract}), but to provide \bold{alternative} sub-setting methods and functions,
#' to be used in situations where the square-brackets operators are inconvenient. \cr
#' These are (hopefully) easier sub-setting methods and functions
#' with the following properties:
#' 
#'  * \bold{Programmatically friendly}:
#'    * Non-standard evaluation is quite controversial (and for good reasons),
#'    and therefore completely absent in this R package.
#'    * Name-based arguments instead of position-based arguments.
#'    * Missing arguments can be filled with `NULL`,
#'    instead of using dark magic like `base::quote(expr =    )`.
#'    * Functions are pipe-friendly.
#'  * \bold{Beginner friendly}:
#'    * No (silent) vector recycling.
#'    * Extracting and removing subsets uses the same syntax.
#'    * All functions return a copy of the object, unless stated otherwise.
#'  * \bold{Class consistent}: 
#'    * sub-setting of multi-dimensional objects by specifying dimensions
#'    (i.e. rows, columns, ...)
#'    use `drop = FALSE`. So matrix in, matrix out.
#'    * The functions deliver the same results for
#'    data.frames, data.tables, tibbles, and tidytables.
#'    No longer does one have to re-learn the different brackets-based sub-setting rules
#'    for different types of data.frame-like objects.
#'    Powered by the subclass agnostic 'C'-code from 'collapse' and 'data.table'.
#'  * \bold{Explicit copy semantics}:
#'    * Sub-set operations that change its memory allocations,
#'    always return a modified copy of the object.
#'    * For sub-set operations that just change values in-place
#'    (similar to the `[<-` and `[[<-` methods)
#'    the user can choose a method that modifies the object by \bold{reference},
#'    or choose a method that returns a \bold{deep copy}.
#'  * \bold{Careful handling of names and other attributes}:
#'    * Sub-setting an object by index names returns ALL indices with that name,
#'    not just the first.
#'    * Data.frame-like objects (see supported classes below)
#'    are forced to have unique column names.
#'    * Attributes of data.frame-like objects (see supported classes below) are always preserved when sub-setting.
#'    * For other object types, the user can specify whether to preserve Attributes,
#'    or use R's `[` attribute behaviour (i.e. drop most attributes).
#'    This is to ensure compatibility with R-packages that create their own attribute behaviour for sub-setting.
#'  * \bold{Support a wide variety of S3 classes}:
#'    * Support atomic objects (vectors, matrices, arrays).
#'    * Support factors.
#'    * Support lists.
#'    * Support the following data.frame-like objects:
#'    data.frame, data.table, tibble, and tidytable class,
#'    and objects derived from these classes.
#'    * Support for the column selection sub-setting used in ggplot2's \link[ggplot2]{aes} function.
#'    * Support for sub-setting characters in a single string.
#'    * Since the main functions are S3 methods,
#'    other packages may add functionality for additional S3 classes.
#'  * \bold{Concise function and argument names}.
#'  * \bold{Performance aware}: \cr
#'  Despite the many checks performed, the functions are kept reasonably speedy,
#'  through the use of the 'Rcpp', 'collapse', and 'data.table' R-packages.
#'  Most of the heavy lifting in this package is done by the 'collapse' package. \cr \cr
#'  
#'  
#' @section Methods and Functions:
#' 
#' The main focus is on the following generic S3 methods:
#' 
#'  * \link{sb_x}: method to extract, exchange, or duplicate indices.
#'  * \link{sb_rm}: method to remove indices.
#'  * \link{sb_set}: method to modify
#'  (transform or replace values)
#'  subsets of an object by \bold{reference}.
#'  * \link{sb_mod}: method to return a \bold{copy}
#'   of an object with modified
#'   (transformed or replaced values) subsets.
#'  * \link{sb_coe}: coerce and transform a whole object,
#'   or a recursive subset of an object.
#'  * \link{sb_before}, \link{sb_after}:
#'  methods to insert new values before or after an index
#'  along a dimension of an object.
#'  * \link{sb_rec}: not actually a method,
#'  but a function that can be combined with the above methods,
#'  for recursive sub-setting operations. \cr \cr
#' 
#' NOTE: \link{sb_mod} is faster than \link{sb_set},
#' but \link{sb_set} is more memory efficient than \link{sb_mod}. \cr
#' \cr
#' Beside these generic S3 methods,
#' additional specialized sub-setting functions are provided:
#' 
#'  * \link{aes_pro}: programmatically friendly and stable version of the
#'  \code{ggplot2::}\link[ggplot2]{aes} function.
#'  * \link{sb_str}: extract or replace a subset of characters of a single string
#'  (each single character is treated as a single element).
#'  * \link{sb_a}: extract multiple attributes from an object. \cr \cr
#' 
#' And finally,
#' a couple of helper functions for creating ranges, sequences, and indices
#' (often needed in sub-setting)
#' are provided:
#' 
#'  * \link{seq_rec}: Recursive sequence generator
#'  (for example to generate a Fibonacci sequence).
#'  * \link{seq_names}: create a range of indices from a specified starting and ending name.
#'  * \link{sub2coord}, \link{coord2ind}: Convert subscripts
#'  (array indices)
#'  to coordinates,
#'  coordinates to flat indices, and vice-versa. \cr \cr
#' 
#' 
#' @section Help pages:
#' 
#' For an explanation of the classes, and how each class is treated by 'subsets',
#' see \link{subsets_classes}. \cr
#' \cr
#' For an explanation of the common indexing arguments in the generic methods,
#' see \link{subsets_indx_args}. \cr
#' \cr 
#' 
#' 
#' @author \strong{Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @seealso
#' 
#' 'subsets' relies on the 'Rcpp', 'collapse' and 'data.table' R-packages
#' to ensure an acceptable performance of its functions despite the many checks that these functions perform.
#' I also recommend using these packages for other sub-setting and data wrangling functionalities. \cr
#' \cr
#' 'subsets' uses a modified version of the \link[abind]{abind} function from the 'abind' R-package.
#' This R-package is recommended for binding and sub-filling arrays of arbitrary dimensions. \cr
#' \cr
#' Besides these package,
#' the following R packages work very nicely together with 'subsets':
#' 
#'  * 'stringi': \cr
#'  THE R package for fast and concise string manipulation - an essential part of any programming language.
#'  * 'tinycodet': \cr
#'  Helps the user with their coding etiquette.
#'  Focuses on 4 aspects: (1) safe functionalities;
#'  (2) an import system that combines benefits of using a package without attaching, and attaching a package;
#'  (3) extending the capabilities of the 'stringi' package;
#'  (4) functions to reduce repetitive code.
#' 
#' @references The badges shown in the documentation of this R-package were made using the services of: \url{https://shields.io/}
#' 
#' @docType package
#' @name aaa0_subsets
#' @rdname aaa0_subsets
#' @aliases subsets-package
#' @aliases subsets
#' @aliases subsets_help
#' @useDynLib subsets, .registration=TRUE
#' @importFrom Rcpp evalCpp
#' @exportPattern "^[[:alpha:]]+"
NULL
#> NULL
