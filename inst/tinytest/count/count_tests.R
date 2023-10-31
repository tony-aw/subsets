# count number of tests


# set working directory to source file location ====
stub <- function() {}
thisPath <- function() {
  # Based on:
  # https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/15373917#15373917
  # https://stackoverflow.com/questions/1815606/determine-path-of-the-executing-script/7585599#7585599
  # https://stackoverflow.com/questions/47044068/get-the-path-of-current-script/47045368#47045368
  # https://stackoverflow.com/questions/53512868/how-to-automatically-include-filepath-in-r-markdown-document/53516876#53516876
  # https://gist.github.com/jasonsychau/ff6bc78a33bf3fd1c6bd4fa78bbf42e7
  cmdArgs <- commandArgs(trailingOnly = FALSE)
  if (length(grep("^-f$", cmdArgs)) > 0) {
    # R console option
    return(normalizePath(dirname(cmdArgs[grep("^-f", cmdArgs) + 1]))[1])
  } else if (length(grep("^--file=", cmdArgs)) > 0) {
    # Rscript/R console option
    scriptPath <- normalizePath(dirname(sub("^--file=", "", cmdArgs[grep("^--file=", cmdArgs)])))[1]
    return(scriptPath)
  } else if (Sys.getenv("RSTUDIO") == "1") {
    if (rstudioapi::isAvailable(version_needed=NULL,child_ok=FALSE)) {
      # RStudio interactive
      return(dirname(rstudioapi::getSourceEditorContext()$path))
    } else if (is.null(knitr::current_input(dir = TRUE)) == FALSE) {
      # Knit
      return(knitr::current_input(dir = TRUE))
    } else {
      # R markdown on RStudio
      return(getwd())
    }
  } else if (is.null(attr(stub, "srcref")) == FALSE) {
    # 'source'd via R console
    return(dirname(normalizePath(attr(attr(stub, "srcref"), "srcfile")$filename)))
  } else {
    stop("Cannot find file path")
  }
}

wd <- thisPath()
setwd(wd)
setwd("..")
getwd()

enumerate_total <- 0
files <- list.files(normalizePath(getwd()), pattern = ".R", full.names = TRUE)
for(iFile in files) {
  capture.output(source(normalizePath(iFile)), file = nullfile())
  print(enumerate)
  enumerate_total <- enumerate_total + enumerate
}

print(enumerate_total)
