#' Programmatically Friendly, Standard Evaluated aes() Function Alias
#'
#' @description
#' Programmatically friendly version of \code{ggplot2""}\link[ggplot2]{aes} function.
#' This function is programmatically friendly because it uses
#' proper standard evaluation,
#' instead of non-standard evaluation,
#' tidy evaluation,
#' or similar programmatically unfriendly evaluations. \cr
#' 
#'
#'
#' @param ... arguments to be passed to \link[ggplot2]{aes},
#' but passed as formulas rather than non-standard evaluated dark magic. \cr
#'
#'
#' 
#' @details
#' Non-Standard Evaluation is quite controversial (see \link{subsets_NSE}). \cr
#' \cr
#' Often standard-evaluated alternatives provided. \cr
#' But in the case of the \code{aes()} function in 'ggplot2',
#' the standard-evaluated alternative changes frequently,
#' and the ones provided so far are rather clumsy. \cr
#' \cr
#' The \code{aes_pro()} function is the standard evaluated alternative.
#' Due to the way \code{aes_pro()} is programmed,
#' it should work no matter how many times the standard evaluation techniques
#' change in 'ggplot2'. \cr
#' It should also work in older and newer versions of 'ggplot2'. \cr
#' To support functions in combinations with references of the variables,
#' the input used here are formula inputs, rather than character inputs. \cr
#' See Examples section below.
#'
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
#' x <- ~ sqrt(mass)
#' y <- ~ height
#' color <- ~ sex
#'
#' ggplot2::ggplot(starwars, aes_pro(x, y, color = color)) +
#'   ggplot2::geom_point()
#'

#' @rdname aes_pro
#' @export
aes_pro <- function(...) {
  lst <- list(...)
  is_formula <- function(x) inherits(x, "formula") && is.call(x) && x[[1]] == quote(`~`)
  check <- vapply(lst, is_formula, logical(1))
  if(any(!check)) {
    stop("formula inputs must be given")
  }
  args.names <- ifelse(names(lst)=="", "", paste0(names(lst), " = "))
  args.values <- vapply(lst, \(x)as.character(x)[2], character(1))
  args <- paste0(args.names, args.values, collapse = ", ")
  txt <- paste0("ggplot2::aes(", args, ")")
  message(txt)
  eval(parse(text=txt), envir = parent.frame(n = 1))
}

