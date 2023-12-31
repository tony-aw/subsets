% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/aaa2_subsets_indx_args.R
\name{aaa2_subsets_indx_args}
\alias{aaa2_subsets_indx_args}
\alias{subsets_indx_args}
\title{Index Arguments in the Generic Sub-setting Methods}
\description{
There are 6 types of arguments that can be used
in the generic methods of 'subsets' to specify the indices to perform operations on:
\itemize{
\item \code{i}: to specify flat (i.e. dimensionless) indices.
\item \verb{row, col}: to specify rows and/or columns in tabular objects.
\item \verb{idx, dims}: to specify indices of arbitrary dimensions in arrays.
\item \code{rcl}: to specify rows (first dimension),
columns (second dimension),
and layers (third dimension),
in arrays that have exactly 3 dimensions.
\item \code{lvl}: specify levels, for factors only.
\item \verb{filter, vars}: to specify rows and/or columns specifically in data.frame-like objects. \cr \cr
}

In this help page, \code{x} refers to the object to be sub-setted. \cr
\cr
}
\section{Argument i}{

\ifelse{html}{\figure{class-atomic-red.svg}{options: alt='[class: atomic]'}}{\link[=subsets_classes]{class: atomic}} \cr
\ifelse{html}{\figure{class-factor-red.svg}{options: alt='[class: factor]'}}{\link[=subsets_classes]{class: factor}} \cr
\ifelse{html}{\figure{class-list-red.svg}{options: alt='[class: list]'}}{\link[=subsets_classes]{class: list}} \cr

Any of the following can be specified for argument \code{i}:
\itemize{
\item \code{NULL}, only for multi-dimensional objects or factors,
when specifying the other arguments
(i.e. dimensional indices or factor levels.)
\item a vector of length 0,
in which case no indices are selected for the operation
(i.e. empty selection).
\item a \bold{strictly positive integer} vector with indices.
\item a \bold{logical vector} (without \code{NA}s!),
of the same length as \code{x},
giving the indices to select for the operation.
\item a \bold{character} vector of index names. \cr
If an object has multiple indices with the given name,
ALL the corresponding indices will be selected for the operation.
\item a \bold{function} that takes as input \code{x},
and returns a logical vector,
giving the element indices to select for the operation. \cr
For atomic objects, \code{i} is interpreted as \code{i(x)}. \cr
For lists, \code{i} is interpreted as \code{lapply(x, i)}. \cr
}

Using the \code{i} arguments corresponds to doing something like the following:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ sb_x(x, i = i) # ==> x[i]
 
}\if{html}{\out{</div>}}

For a brief explanation of the relationship between flat indices (\code{i}),
and the dimension indices (\code{row}, \code{col}, etc.),
see the \code{Details} section in \link{sub2ind}. \cr
}

\section{Arguments row, col}{

\ifelse{html}{\figure{class-matrix-red.svg}{options: alt='[class: matrix]'}}{\link[=subsets_classes]{class: matrix}} \cr
\ifelse{html}{\figure{class-data.frame-like-red.svg}{options: alt='[class: data.frame-like]'}}{\link[=subsets_classes]{class: data.frame-like}} \cr

Any of the following can be specified for the arguments \code{row} / \code{col}:
\itemize{
\item \code{NULL} (default), corresponds to a missing argument,
which results in ALL of the indices in this dimension being selected for the operation.
\item a vector of length 0,
in which case no indices are selected for the operation (i.e. empty selection).
\item a \bold{strictly positive integer} vector with dimension indices to select for the operation.
\item a \bold{logical} vector (without \code{NA}s!) of the same length as the corresponding dimension size,
giving the indices of this dimension to select for the operation.
\item a \bold{character} vector of index names. \cr
If an object has multiple indices with the given name,
ALL the corresponding indices will be selected for the operation.
}

NOTE: The arguments \code{row} and \code{col} will be ignored if \code{i} is specified.

Using the \verb{row, col} arguments corresponds to doing something like the following:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ sb_x(x, row = row, col = col) # ==> x[row, col, drop = FALSE]
 
}\if{html}{\out{</div>}}
}

\section{Arguments idx, dims}{

\ifelse{html}{\figure{class-array-red.svg}{options: alt='[class: array]'}}{\link[=subsets_classes]{class: array}} \cr

\code{idx} must be a list of indices. \cr
\code{dims} must be an integer vector of the same length as \code{idx},
giving the dimensions to which the indices given in \code{idx} correspond to. \cr
The elements of \code{idx} follow the same rules as the rules for \code{row} and \code{col},
EXCEPT one should not fill in \code{NULL}. \cr
NOTE: The arguments \code{idx} and \code{dims} will be ignored if \code{i} is specified. \cr
\cr
To keep the syntax short,
the user can use the \link{n} function instead of \code{list()} to specify \code{idx}. \cr
\cr
Using the \verb{idx, dims} arguments,
corresponds to doing something like the following,
here using an example of a 4-dimensional array:

\if{html}{\out{<div class="sourceCode">}}\preformatted{sb_x(x, n(1:10, 1:5), c(1, 3)) # ==> x[1:10, , 1:5, , drop = FALSE]

}\if{html}{\out{</div>}}
}

\section{Arguments rcl}{

\ifelse{html}{\figure{class-array-red.svg}{options: alt='[class: array]'}}{\link[=subsets_classes]{class: array}} \cr

The \code{rcl} argument is only applicable for arrays with exactly 3 dimensions. \cr
If the user knows a-priori that an array has 3 dimensions,
using \code{rcl} is more efficient than using the \verb{idx, dims} arguments. \cr
\cr
The \code{rcl} argument must be a list of exactly 3 elements,
with the first element giving the indices of the first dimension (rows),
the second element giving the indices of the second dimension (columns),
and the third element giving the indices of the third and last dimension (layers);
thus \code{rcl} stands for "rows, columns, layers"
(i.e. the 3 dimensions of a 3-dimensional array). \cr
For each of the aforementioned 3 elements of the list \code{rcl},
any of the following can be specified:
\itemize{
\item \code{NULL}, corresponds to a missing argument,
which results in ALL of the indices in this dimension being selected for the operation.
\item a vector of length 0,
in which case no indices are selected for the operation (i.e. empty selection).
\item a \bold{strictly positive integer} vector with dimension indices to select for the operation.
\item a \bold{logical} vector (without \code{NA}s!) of the same length as the corresponding dimension size,
giving the indices of this dimension to select for the operation.
\item a \bold{character} vector of index names. \cr
If an object has multiple indices with the given name,
ALL the corresponding indices will be selected for the operation.
}

By default \code{rcl} is not a list but simply \code{NULL},
to be used when specifying the other arguments (either \verb{idx, dims} or \code{i}). \cr
\cr
To keep the syntax short,
the user can use the \link{n} function instead of \code{list()} to specify \code{rcl}. \cr
\cr
Using the \code{rcl} argument corresponds to doing something like the following:

\if{html}{\out{<div class="sourceCode">}}\preformatted{sb_x(x, rcl = n(NULL, 1:10, 1:5)) # ==> x[, 1:10, 1:5, drop = FALSE]

}\if{html}{\out{</div>}}
}

\section{Argument lvl}{

\ifelse{html}{\figure{class-factor-red.svg}{options: alt='[class: factor]'}}{\link[=subsets_classes]{class: factor}} \cr

For this argument, the names of the levels of \code{x} can be given,
selecting the corresponding indices for the operation. \cr
\cr
}

\section{Arguments filter, vars}{

\ifelse{html}{\figure{class-data.frame-like-red.svg}{options: alt='[class: data.frame-like]'}}{\link[=subsets_classes]{class: data.frame-like}} \cr

\code{filter} must be a one-sided formula
with a single logical expression using the column names of the data.frame,
giving the condition which observation/row indices should be selected for the operation. \cr
For example,
to perform an operation on the rows for which column \code{height > 2} and for which column \code{sex != "female"},
specify the following formula: \cr

\if{html}{\out{<div class="sourceCode">}}\preformatted{~ (height > 2) & (sex != "female")
}\if{html}{\out{</div>}}

\code{vars} must be a function that returns a logical vector,
giving the column indices to select for the operation. \cr
For example, to select all numeric columns, specify \code{vars = is.numeric}. \cr
\cr
}

\section{Duplicates (for Names, Integers, and Levels)}{

Generally speaking, duplicate names, integers, or levels are NOT allowed in index selection. \cr
The exception is the \link{sb_x} method,
as that method can be used for duplicating indices. \cr
\cr
}

\section{Out-of-Bounds Integers and Unknown Names/Levels}{

Integers that are out of bounds always give an error. \cr
Specifying unknown names/levels is considered a form of zero-length indexing. \cr
\cr
}

\section{Disallowed Combinations of Index Arguments}{


One cannot specify \code{i} and  the other indexing arguments simultaneously;
it's either \code{i}, or the other arguments. \cr
The arguments are  evaluated in the following order:
\enumerate{
\item Argument \code{i}
\item Argument \code{lvl} (for factors) or argument \code{rcl} (for 3-dimensional arrays)
\item The rest of the indexing arguments.
}

One cannot specify \code{row} and \code{filter} simultaneously.
It's either one or the other. Similarly,
one cannot specify \code{col} and \code{vars} simultaneously. \cr
In the above cases it holds that if one set is specified, the other is set is ignored. \cr
\cr
}

\section{Drop}{

Sub-setting with the generic methods from the 'subsets' R-package using dimensional arguments
(\verb{row, col, lyr, idx, dims, filter, vars})
always use \code{drop = FALSE}. \cr
To drop potentially redundant (i.e. single level) dimensions,
use the \link[base]{drop} function, like so:

\if{html}{\out{<div class="sourceCode">}}\preformatted{ sb_x(x, row = row, col = col) |> drop() # ==> x[row, col, drop = TRUE]
 
}\if{html}{\out{</div>}}
}

\section{First, Last, and Shuffle}{

The indices are counted forward. I.e. \code{1} is the first element, not the last. \cr
One can use the \link[data.table]{last} function to get the last \code{N} indices. \cr
\cr
One can use the \link[data.table]{first} function to get the first \code{N} indices. \cr
\cr
To shuffle elements of indices, use the \link[base]{sample} function. \cr
\cr
}

