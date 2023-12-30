#' Miscellaneous Information
#'
#' @section Controversy Surrounding Non-Standard Evaluation:
#' Non-Standard Evaluation (sometimes abbreviated as "NSE"),
#' is quite controversial. \cr
#' Consider the following example:
#'
#' ```{r echo=TRUE, eval = FALSE}
#' aplot <- "ggplot2"
#' library(aplot)
#' ```
#' What package will be attached? It will not be 'ggplot2',
#' nor will an error occur.
#' Instead, the package 'aplot' will be attached. \cr
#' This is due to evaluating the expression 'aplot' as a quoted expression,
#' instead of evaluating the contents (i.e. string or formula) of the variable.
#' In other words: Non-Standard Evaluation. \cr
#' \cr
#' A standard evaluated expression works better. \cr
#' Standard evaluation in 'R' is not limited to characters.
#' Formulas can also be used.
#' For example, if the `library()` function would support a formula input,
#' the following would correctly load 'ggplot2':
#' 
#' ```{r echo=TRUE, eval = FALSE}
#' aplot <- ~ ggplot2
#' library(aplot)
#' ```
#' 
#' 
#' 
#' 
#' @section Performance Tips: 
#' 
#' A few tips to improve performance:
#' 
#'  - Use integer vectors for sub-setting whenever possible.
#'  - Do not set \code{rat = TRUE} if it's not truly necessary
#'  (it defaults to \code{rat = FALSE}).
#'  - Perform as much sub-set operations in a single function.
#' 
#' @rdname aaa3_subsets_misc
#' @name aaa3_subsets_misc
#' @aliases subsets_misc
NULL
