#' Programmatically Friendly, Standard Evaluated Versions of with() and ggplot2::aes()
#'
#' @description
#' Programmatically friendly versions of the
#' \link{with} and \code{ggplot2::}\link[ggplot2]{aes} functions.
#' These alternative functions are more programmatically friendly because it uses
#' proper standard evaluation,
#' instead of non-standard evaluation,
#' tidy evaluation,
#' or similar programmatically unfriendly evaluations. \cr
#' 
#'
#'
#' @param ... arguments to be passed to \link[ggplot2]{aes},
#' but given as one-sided formulas,
#' rather than non-standard evaluated dark magic. \cr
#' @param data a list, environment, or data.frame.
#' @param form a one-sided formula giving the expression to evaluate.
#' Global variables are not allowed, only variables that are actually present in the data.
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
#' data("mpg", package = "ggplot2")
#' x <- ~ cty
#' y <- ~ sqrt(hwy)
#' color <- ~ drv
#'
#' ggplot2::ggplot(mpg, aes_pro(x, y, color = color)) +
#'   ggplot2::geom_point()
#' 
#' myform <- ~ sqrt(hwy)
#' mpg$hwy_sqrt <- with_pro(mpg, myform)
#' summary(mpg)
#'

#' @rdname pro
#' @export
aes_pro <- function(...) {
  lst <- list(...)
  is_formula <- function(x) inherits(x, "formula") && is.call(x) && x[[1]] == quote(`~`)
  check <- vapply(lst, is_formula, logical(1))
  if(any(!check)) stop("formula inputs must be given")
  check <- lengths(lst)
  if(any(check != 2)) stop("improper formula given")
  args.names <- ifelse(names(lst)=="", "", paste0(names(lst), " = "))
  args.values <- vapply(lst, \(x)as.character(x)[2], character(1))
  args <- paste0(args.names, args.values, collapse = ", ")
  txt <- paste0("ggplot2::aes(", args, ")")
  message(txt)
  eval(parse(text=txt), envir = parent.frame(n = 1))
}


#' @rdname pro
#' @export
with_pro <- function(data, form) {
  is_formula <- inherits(form, "formula") && is.call(form) && form[[1]] == quote(`~`)
  if(!is_formula) stop("`form` must be a formula")
  if(length(form) != 2) stop("improper formula given")
  if(!is.recursive(data)) stop("`data` must be a recursive object")
  
  vars <- all.vars(form)
  if(any(!vars %in% names(data))) stop("unknown variables given")
  
  txt <- as.character(form)[2]
  expr <- parse(text = txt)
  environment(form) <- emptyenv()
  out <- with(data, expr = eval(expr))
  return(out)
}

