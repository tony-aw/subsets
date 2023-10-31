#

#' @keywords internal
#' @noRd
.is.missing <- function(x) {
  return(missing(x) || identical(x, quote(expr = )))
}

#' @keywords internal
#' @noRd
.selectnames <- function(sel, nms) {
  out <- lapply(sel, \(i)collapse::whichv(nms, i)) |> unlist()  
  return(out)
}
# NOTE regarding speed of .selectnames():
# lapply + collapse::whichv() is faster than
# outer(), grr::matches() (written in C), or even a simple Rcpp method

#' @keywords internal
#' @noRd
.indx_check_general <- function(indx, abortcall) {
  if(anyNA(indx)) {
    error.txt <- paste0("NA indices not allowed")
    stop(simpleError(error.txt, call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_check_names <- function(dnames, abortcall) {
  
  if(length(dnames) == 0) {
    error.txt <- paste0(
      "`x` has no names; fix this before subsetting"
    )
    stop(simpleError(error.txt, call = abortcall))
    
  }
}

#' @keywords internal
#' @noRd
.indx_check_logical <- function(n.indx, dlength, abortcall) {
  if(n.indx != dlength) {
    error.txt <- paste0("incorrect length of logical indices")
    stop(simpleError(error.txt, call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_check_int <- function(indx, dlength, abortcall) {
  if(.anynegative(indx)) {
    error.txt <- "only strictly positive integers allowed"
    stop(simpleError(error.txt, call = abortcall))
  }
  if(.anygreater(indx, as.integer(dlength))) {
    error.txt <- "subscript out of bounds"
    stop(simpleError(error.txt, call = abortcall))
  }
}


#' @keywords internal
#' @noRd
.indx_stop <- function(abortcall) {
  stop(simpleError(
    "indices must be a numeric, logical, or character vector",
    call = abortcall
  ))
}


#' @keywords internal
#' @noRd
.indx_convert_chr <- function(indx, dnames, allow_dupl, inv, abortcall) {
  
  if(!allow_dupl) {
    if(anyDuplicated(indx)) {
      error.txt <- "duplicate integers or names not allowed"
      stop(simpleError(error.txt, call = abortcall))
    }
  }
  
  if(!inv && allow_dupl) {
    out <- .selectnames(indx, dnames)
    return(out)
  }
  if(!inv && !allow_dupl) { return(collapse::`%iin%`(dnames, indx)) }
  if(inv){ return(collapse::`%!iin%`(dnames, indx)) }
  
}



#' @keywords internal
#' @noRd
.indx_convert_int <- function(indx, n, allow_dupl, inv, abortcall) {
  
  if(!allow_dupl) {
    if(anyDuplicated(indx)) {
      error.txt <- "duplicate integers or names not allowed"
      stop(simpleError(error.txt, call = abortcall))
    }
  }
  if(!inv) { return(indx) }
  if(inv) { return(seq_len(n)[-indx]) }
  
}



#' @keywords internal
#' @noRd
.indx_make_element <- function(indx, x, is_list, allow_dupl, inv, abortcall) {
  
  if(is.function(indx)) {
    if(is_list){
      indx <- vapply(x, indx, FUN.VALUE = logical(1), USE.NAMES = FALSE) |> unlist()
    } else {indx <- indx(x)}
    
    if(!is.logical(indx)) {
      error.txt <- simpleError(
        "if elements are given through a function, the function must return a logical vector",
        call = abortcall
      )
      stop(error.txt)
    }
    if(!inv) return(which(indx))
    if(inv) return(which(!indx))
  }
  
  n <- length(x)
  n.indx <- length(indx)
  
  .indx_check_general(indx, abortcall)
  
  if(n.indx == 0) {
    if(!inv) return(integer(0))
    if(inv) return(seq_len(n))
  }
  
  if(is.numeric(indx)) {
    indx <- as.integer(indx)
    .indx_check_int(indx, n, abortcall)
    return(.indx_convert_int(indx, n, allow_dupl, inv, abortcall))
  }
  
  if(is.character(indx)) {
    .indx_check_names(names(x), abortcall)
    return(.indx_convert_chr(indx, names(x), allow_dupl, inv, abortcall))
    
  }
  
  if(is.logical(indx)) {
    .indx_check_logical(n.indx, n, abortcall)
    
    if(!inv){return(which(indx))}
    if(inv){return(which(!indx))}
    
  }
  
  
  .indx_stop(abortcall)
}



#' @keywords internal
#' @noRd
.lvl2indx <- function(indx, x, allow_dupl, inv, abortcall) {
  
  n <- length(x)
  
  .indx_check_general(indx, abortcall)
  
  if(length(indx)==0) {
    if(!inv) return(integer(0))
    if(inv) return(seq_len(n))
  }
  
  if(!allow_dupl) {
    if(anyDuplicated(indx)) {
      error.txt <- "duplicate integers or names not allowed"
      stop(simpleError(error.txt, call = abortcall))
    }
  }
  
  if(any(collapse::`%!iin%`(indx, levels(x)))) {
    error.txt <- "unknown level given"
    stop(simpleError(error.txt, call = abortcall))
  }
  
  if(!inv && allow_dupl) {
    out <- .selectnames(indx, x)
    return(out)
  }
  if(!inv && !allow_dupl) { return(collapse::`%iin%`(x, indx)) }
  if(inv){ return(collapse::`%!iin%`(x, indx)) }
  
  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.prep_relevel <- function(indx, rp, x, abortcall) {
  
  n.indx <- length(indx)
  if(n.indx == 0) {
    return(logical(0))
  }

  if(anyDuplicated(indx)) {
    error.txt <- "duplicate integers or names not allowed"
    stop(simpleError(error.txt, call = abortcall))
  }

  if(any(collapse::`%!iin%`(indx, levels(x)))) {
    error.txt <- "unknown level given"
    stop(simpleError(error.txt, call = abortcall))
  }
  
  if(n.indx != length(rp)) {
    error.txt <- "recycling not allowed"
    stop(simpleError(error.txt, call = abortcall))
  }

}

#' @keywords internal
#' @noRd
.indx_make_dim <- function(
    indx, x, dim.L=1, allow_dupl, inv, abortcall
) {
  

  .indx_check_general(indx, abortcall)
  n.indx <- length(indx)
  
  if(n.indx == 0) {
    if(!inv) return(integer(0))
    if(inv) return(NULL)
  }
  
  if(is.numeric(indx)) {
    dlength <- dim(x)[[dim.L]]
    indx <- as.integer(indx)
    .indx_check_int(indx, dlength, abortcall)
    return(.indx_convert_int(indx, dlength, allow_dupl, inv, abortcall))
  }
  
  if(is.character(indx)) {
    dnames <- dimnames(x)[[dim.L]]
    .indx_check_names(dnames, abortcall)
    return(.indx_convert_chr(indx, dnames, allow_dupl, inv, abortcall))
    
  }

  if(is.logical(indx)) {
    dlength <- dim(x)[[dim.L]]
    .indx_check_logical(n.indx, dlength, abortcall)

    if(!inv){return(which(indx))}
    if(inv){return(which(!indx))}
    
  }

  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.indx_make_tableind <- function(
    indx, x, dim.L=1, allow_dupl, inv, abortcall
) {
  
  .indx_check_general(indx, abortcall)
  
  n.indx <- length(indx)
  
  if(n.indx == 0) {
    if(!inv) return(integer(0))
    if(inv){
      if(dim.L == 1) dlength <- collapse::fnrow(x)
      if(dim.L == 2) dlength <- collapse::fncol(x)
      return(seq_len(dlength))
    }
  }
  
  if(is.numeric(indx)) {
    if(dim.L == 1) dlength <- collapse::fnrow(x)
    if(dim.L == 2) dlength <- collapse::fncol(x)
    indx <- as.integer(indx)
    .indx_check_int(indx, dlength, abortcall)
    return(.indx_convert_int(indx, dlength, allow_dupl, inv, abortcall))
  }
  
  if(is.character(indx)) {
    if(dim.L == 1) dnames <- rownames(x)
    if(dim.L == 2) dnames <- colnames(x)

    .indx_check_names(dnames, abortcall)
    return(.indx_convert_chr(indx, dnames, allow_dupl, inv, abortcall))
  }
  
  if(is.logical(indx)) {
    if(dim.L == 1) dlength <- collapse::fnrow(x)
    if(dim.L == 2) dlength <- collapse::fncol(x)
    .indx_check_logical(n.indx, dlength, abortcall)
    
    if(!inv){return(which(indx))}
    if(inv){return(which(!indx))}
  }
  
  .indx_stop(abortcall)
}


#' @keywords internal
#' @noRd
.indx_make_filter <- function(x, filter, inv, abortcall) {
  
  check_is_formula <- inherits(filter, "formula") & is.call(filter)
  if(!check_is_formula) {
    stop(simpleError("`filter` must be a formula", call = abortcall))
  }
  if(length(filter) != 2) {
    stop(simpleError("invalid formula given", call = abortcall))
  }
  
  exprs <- as.character(filter)[2]
  exprs <- paste0("with(x, ", exprs, ")")
  mm <- eval(parse(text = exprs))
  if(!is.logical(mm)) {
    stop(simpleError("invalid formula given", call = abortcall))
  }
  if(!inv)return(which(mm))
  if(inv)return(which(!mm))
  
}


#' @keywords internal
#' @noRd
.indx_make_vars <- function(x, vars, inv, abortcall) {
  if(!is.function(vars)) {
    stop(simpleError("`vars` must be a function", call = abortcall))
  }
  out <- collapse::get_vars(x, vars, return = "logical")
  if(!inv)return(which(out))
  if(inv)return(which(!out))
}



#' @keywords internal
#' @noRd
.check_args_factor <- function(i, lvl, drop, abortcall) {
  if(!is.null(i) && !is.null(lvl)) {
    stop(simpleError("cannot specify both elements and levels", call = abortcall))
  }
  if(!isTRUE(drop) && !isFALSE(drop)) {
    stop(simpleError("`drop` must be either `TRUE` or `FALSE`", call = abortcall))
  }
}



#' @keywords internal
#' @noRd
.check_args_array <- function(x, idx, dims, i, abortcall) {
  
  present_dims <- !is.null(idx) || !is.null(dims)
  if(present_dims & !is.null(i)) {
    stop("cannot specify both `idx`/`dims` and elements")
  }
}



#' @keywords internal
#' @noRd
.check_args_df <- function(x, row, col, filter, get_vars, abortcall) {
  if(!is.null(filter) && !is.null(row)) {
    error.txt <- simpleError(
      "cannot specify both `filter` and `row`",
      call = abortcall
    )
    stop(error.txt)
  }
  if(!is.null(get_vars) && !is.null(col)) {
    error.txt <- simpleError(
      "cannot specify both `vars` and `col`",
      call = abortcall
    )
    stop(error.txt)
  }
  if(collapse::fndistinct(names(x)) != collapse::fncol(x)) {
    error.txt <- simpleError(paste0(
      "`x` does not have unique variable names for all columns; ",
      "\n",
      "fix this before subsetting"
    ), call = abortcall)
    stop(error.txt)
  }
}



#' @keywords internal
#' @noRd
.any_empty_indices <- function(...) {
  lst <- list(...)
  check <- vapply(lst, \(x)!is.null(x) && length(x) == 0, FUN.VALUE = logical(1))
  if(any(check)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}



#' @keywords internal
#' @noRd
.old_approx_empty_df <- function(x, row, col, class) {
  if(class == "data.frame") {
    x.class <- class(x)
    x2 <- collapse::qDF(x, keep.attr = TRUE)
    x2 <- x2[row, col, drop = FALSE]
    x3 <- collapse::qDF(x, keep.attr = TRUE)
    class(x3) <- x.class
    return(x3)
  }
  if(class == "data.table") {
    x.class <- class(x)
    x2 <- collapse::qDF(x, keep.attr = TRUE)
    x2 <- x2[row, col, drop = FALSE]
    x3 <- collapse::qDT(x, keep.attr = TRUE)
    class(x3) <- x.class
    return(x3)
  }
  if(class == "tibble") {
    x.class <- class(x)
    x2 <- collapse::qDF(x, keep.attr = TRUE)
    x2 <- x2[row, col, drop = FALSE]
    x3 <- collapse::qTBL(x, keep.attr = TRUE)
    class(x3) <- x.class
    return(x3)
  }
}


#' @keywords internal
#' @noRd
.l <- base::list
