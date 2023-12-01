.onAttach <- function(libname, pkgname) {
  txt <- paste0(
    "Run `",
    '?subsets::subsets',
    "` to open the introduction help page of 'subsets'."
  )
  packageStartupMessage(txt)
}
