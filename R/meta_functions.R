#' Internal functions
#'
#'
#'
#'
#'
#'

#' @keywords internal
#' @noRd
.mybadge_class <- function(x) {
  txt <- paste0("class: ", x)
  file <- paste0("class-", x, "-red.svg")
  text <- sprintf("\\link[=subsets_classes]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_coercion <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  if(x == "depends") x2 <- "depends-lightblue"
  txt <- paste0("auto-coercion: ", x)
  file <- paste0("auto-coercion-", x2, ".svg")
  text <- sprintf("\\link[=subsets_classes]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_coercion_by_ref <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  if(x == "depends") x2 <- "depends-lightblue"
  txt <- paste0("coercion_by_reference: ", x)
  file <- paste0("coercion_by_reference-", x2, ".svg")
  text <- sprintf("\\link[=subsets_classes]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_coercion_through_copy <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  if(x == "depends") x2 <- "depends-lightblue"
  txt <- paste0("coercion_through_copy: ", x)
  file <- paste0("coercion_through_copy-", x2, ".svg")
  text <- sprintf("\\link[=subsets_classes]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


#' @keywords internal
#' @noRd
.mybadge_require_unique_names <- function(x) {
  if(x == "YES") x2 <- "YES-darkgreen"
  if(x == "NO") x2 <- "NO-red"
  txt <- paste0("require unique names: ", x)
  file <- paste0("require_unique_names-", x2, ".svg")
  text <- sprintf("\\link[=subsets_classes]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}


