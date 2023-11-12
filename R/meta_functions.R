#' Internal functions
#'
#'
#'
#'
#'
#'

#' @keywords internal
#' @noRd
.mybadge <- function(txt, file) {
  text <- sprintf("\\link[=subsets_indx_args]{%s}", txt)
  html <- sprintf(
    "\\figure{%s}{options: alt='[%s]'}",
    file, txt)
  sprintf("\\ifelse{html}{%s}{%s}", html, text)
}
