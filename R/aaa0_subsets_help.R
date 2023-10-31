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
#' And R is no exception. \cr
#' \cr
#' This becomes quite apparent when one reads (online) documents such as
#' \href{https://www.burns-stat.com/pages/Tutor/R_inferno.pdf}{"The R Inferno"}
#' by Patrick Burns,
#' and 
#' \href{https://github.com/ReeceGoding/Frustration-One-Year-With-R}{"Frustration: One Year With R"}
#' by Reece Goding.
#' These document point out many inconsistencies,
#' and sub-setting related inconsistencies make up a good portion of these documents. \cr
#' \cr
#' To my surprise, there is no comprehensive R package
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
#' @section Properties:
#' 
#' The subsets package provides
#' easier subsetting functions with the following properties:
#' 
#'  * \bold{Programmatically friendly}:
#'    * Non-standard evaluation is highly controversial (and for good reasons!),
#'    and therefore completely absent in this R package;
#'    * Name-based instead of position-based arguments; 
#'    * Missing arguments can be filled with `NULL`,
#'    instead of using dark magic like `base::quote(expr =    )`.
#'    * Functions are pipe-friendly.
#'  * \bold{Beginner friendly}:
#'    * No (silent) vector recycling; 
#'    * Extracting and removing subsets uses the same syntax.
#'  * \bold{Class consistent}: 
#'    * sub-setting of multi-dimensional objects by specifying dimensions
#'    (i.e. rows, columns, ...)
#'    use `drop = FALSE`. So matrix in, matrix out.
#'    * The functions deliver the same results for
#'    data.frames, data.tables, tibbles, and tidytables.
#'    No longer does one have to re-learn the different brackets-based sub-setting rules
#'    for different types of data.frame-like objects.
#'    Powered by the subclass agnostic 'C'-code from 'collapse' and 'data.table'.
#'    * Smart with sub-setting recursive lists.
#'  * \bold{Careful handling of name-based indexing}:
#'    * Sub-setting object by index names returns ALL indices with that name,
#'  not just the first;
#'    * Data.frame-like objects are forced to have unique column names;
#'    * Selecting non-existing names always gives an error.
#'  * \bold{Support a wide variety of data types}:
#'    * Support vector-like (atomic) objects (vectors, matrices, arrays);
#'    * Support lists;
#'    * Support factors;
#'    * Support data.frame-like objects
#'    (data.frame, data.table, tibble, tidytable, etc.).
#'  * \bold{Concise function and argument names}.
#'  * \bold{Special functions}: \cr
#'  for string subsetting, vectorized recursive list subsetting,
#'  and even for the column selection subsetting used in ggplot2's \link[ggplot2]{aes} function.
#'  * \bold{Performance aware}: \cr
#'  Despite the many checks performed, the functions are kept reasonably speedy,
#'  through the use of the 'Rcpp', 'collapse', and 'data.table' R-packages. \cr \cr
#'  
#'  
#' @section Methods and Functions:
#' 
#' The main focus is on the following generic S3 methods:
#' 
#'  * \link{sb_x}: method to extract, exchange, or duplicate indices.
#'  * \link{sb_rm}: method to remove indices.
#'  * \link{sb_tf}: method to transform values of subsets.
#'  * \link{sb_rp}: method to replace values of subsets.
#'  * \link{sb_before}, \link{sb_after}:
#'  methods to insert new values before or after an index
#'  along a dimension of an object. \cr \cr
#'  
#' Beside these generic S3 methods,
#' additional specialized sub-setting functions are provided:
#' 
#'  * \link{aes_pro}: programmatically friendly and stable version of ggplot2's aesthetic sub-setting function.
#'  * \link{sb_str}: subset a single string (each single character is treated as a single element).
#'  * \link{sb_rec}: recursive sub-setting of lists.
#' 
#' And finally,
#' a couple of helper functions for creating ranges and sequences
#' (occasionally needed in sub-setting)
#' are provided:
#' 
#'  * \link{seq_rec}: Generalized recursive sequence generator.
#'  * \link{seq_names}: create a range of indices from a specified starting and ending name. \cr \cr
#' 
#' 
#' @author \strong{Maintainer}: Tony Wilkes \email{tony_a_wilkes@outlook.com} (\href{https://orcid.org/0000-0001-9498-8379}{ORCID})
#' 
#' 
#' @seealso
#' 
#' 'subsets' relies on the 'Rcpp', 'collapse' and 'data.table' R-packages
#' to ensure an acceptable performance of its functions despite the many checks that these functions perform.
#' I also recommend using these packages for other subsetting and data wrangling functionalities. \cr
#' Besides these package,
#' the following R packages work very nicely together with 'subsets':
#' 
#'  * 'stringi': \cr
#'  THE R package for fast and concise string manipulation - an essential part of any programming language.
#'  * 'abind': \cr
#'  Provides binding arrays along an arbitrary dimension.
#'  * 'tinycodet': \cr
#'  Helps the user with their coding etiquette.
#'  Focuses on 4 aspects: (1) safe functionalities,
#'  (2) an import system that combines benefits of using without attaching and attaching a package,
#'  (3) extending the capabilities of the aforementioned 'stringi' package,
#'  (4) functions to reduce repetitive code.
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