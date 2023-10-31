#' Programmatically Friendly, Standard Evaluated aes() Function Alias
#'
#' @description
#' Programmatically friendly version of 'ggplot2''s \link[ggplot2]{aes} function.
#' This function is programmatically friendly because it uses
#' proper standard evaluation,
#' instead of non-standard evaluation,
#' tidy evaluation,
#' or similar programmatically unfriendly evaluations. \cr
#'
#' @details
#' Non-Standard Evaluation (sometimes abbreviated as "NSE"),
#' is highly controversial. \cr
#' Consider the following example:
#'
#' ```{r echo=TRUE, eval = FALSE}
#' aplot <- "ggplot2"
#' library(aplot)
#' ```
#' What package will be loaded? It will not be 'ggplot2', nor will an error occur.
#' Instead, the package 'aplot' will be attached. \cr
#' This is due to evaluating the expression as a quoted expression,
#' instead of evaluating the contents of the variable
#' (a variable containing a string or formula).
#' In other words: Non-Standard Evaluation. \cr
#' \cr
#' Often standard-evaluated alternatives are also provided.
#' But in the case of the \code{aes()} function in 'ggplot2',
#' the standard-evaluated alternative changes frequently.
#' Moreover, the alternatives provided so far are rather clumsy. \cr
#' \cr
#' The \code{aes_pro()} function is the standard evaluated alternative.
#' Due to the way \code{aes_pro()} is programmed,
#' it should work no matter how many times the standard evaluation techniques
#' change in 'ggplot2'.
#' It should also work in older and newer versions of 'ggplot2'.
#'
#'
#' @param ... arguments to be passed to \link[ggplot2]{aes},
#' except \code{aes_pro()} forces programmatically friendly standard evaluation.
#'
#' @return
#' See \link[ggplot2]{aes}.
#'
#'
#' @examplesIf requireNamespace("ggplot2")
#' requireNamespace("ggplot2")
#' 
#'
#' data("starwars", package = "dplyr")
#' x <- "mass"
#' y <- "height"
#' color <- "sex"
#' xform <- ~ mass
#'
#' ggplot2::ggplot(starwars, aes_pro(x, y, color = color)) +
#'   ggplot2::geom_point()
#'

#' @rdname aes_pro
#' @export
aes_pro <- function(...) {
  lst <- list(...)
  check <- vapply(lst, is.character, logical(1))
  if(any(!check)) {
    stop("character input must be given")
  }
  args.names <- ifelse(names(lst)=="", "", paste0(names(lst), " = "))
  args.values <- do.call(c, lst)
  args <- paste0(args.names, args.values, collapse = ", ")
  txt <- paste0("ggplot2::aes(", args, ")")
  message(txt)
  eval(parse(text=txt), envir = parent.frame(n = 1))
}

