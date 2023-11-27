
<!-- README.md is generated from README.Rmd. Please edit that file -->

# subsets

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

subsets: an Easy Grammar of Subsets

 

## Motivation

    Among programming languages, 'R' has perhaps one of the most
    flexible and comprehensive sub-setting functionality. But with
    flexibility often comes confusion and (apparent) inconsistencies.
    And 'R' is no exception.

    This becomes quite apparent when one reads (online) documents such
    as "The R Inferno" by Patrick Burns, and "Frustration: One Year
    With R" by Reece Goding. These documents point out many
    inconsistencies, and sub-setting related inconsistencies make up a
    good portion of these documents.

    To my surprise, there is no comprehensive R-package (as far as I
    could see at least) that actually attempts to "fix" the
    subset-related issues laid out in these and other such documents.

    Famous subset-related R packages such as 'dplyr' and 'data.table'
    focus almost exclusive on data.frame-like objects, and
    occasionally even add more frustration in some aspects, like being
    not very programmatically friendly.

    Thus, this R package was born.

    Although this package was somewhat made for people who are new to
    'R' (especially when coming from another programming language),
    and found themselves confused, I trust this package will be useful
    even for those who are quite experienced in 'R'.

 

## Goal & Properties

The Goal of the ‘subsets’ package is not to replace the square-brackets
operators (`[`, `[[`, `[<-`, and `[[<-`) per-sé, but to provide
**alternative** sub-setting methods and functions, to be used in
situations where the square-brackets operators are inconvenient.

These are (hopefully) easier sub-setting methods and functions with the
following properties:

- *Programmatically friendly*:

  - Non-standard evaluation is quite controversial (and for good
    reasons), and therefore completely absent in this R package.

  - Name-based arguments instead of position-based arguments.

  - Missing arguments can be filled with `NULL`, instead of using dark
    magic like `base::quote(expr = )`.

  - Functions are pipe-friendly.

- *Beginner friendly*:

  - No (silent) vector recycling.

  - Extracting and removing subsets uses the same syntax.

  - All functions return a copy of the object, unless stated otherwise.

- *Class consistent*:

  - sub-setting of multi-dimensional objects by specifying dimensions
    (i.e. rows, columns, …) use `drop = FALSE`. So matrix in, matrix
    out.

  - The functions deliver the same results for data.frames, data.tables,
    tibbles, and tidytables. No longer does one have to re-learn the
    different brackets-based sub-setting rules for different types of
    data.frame-like objects. Powered by the subclass agnostic ‘C’-code
    from ‘collapse’ and ‘data.table’.

- *Explicit copy semantics*:

  - Sub-set operations that change its memory allocations, always return
    a modified copy of the object.

  - For sub-set operations that just change values in-place (similar to
    the \[\<- and \[\[\<- methods) the user can choose a method that
    modifies the object by *reference*, or choose a method that returns
    a *deep copy*.

- *Careful handling of names and other attributes*:

  - Sub-setting an object by index names returns ALL indices with that
    name, not just the first.

  - Data.frame-like objects (see supported classes below) are forced to
    have unique column names.

  - Attributes of data.frame-like objects (see supported classes below)
    are always preserved when sub-setting.

  - For other object types, the user can specify whether to preserve
    Attributes, or use R’s `[` attribute behaviour (i.e. drop most
    attributes). This is to ensure compatibility with R-packages that
    create their own attribute behaviour for sub-setting.

- *Support a wide variety of S3 classes*:

  - Support atomic objects (vectors, matrices, arrays).

  - Support factors.

  - Support lists.

  - Support the following data.frame-like objects: data.frame,
    data.table, tibble, and tidytable class, and objects derived from
    these classes.

  - Support for the column selection sub-setting used in ggplot2’s
    `aes()` function.

  - Support for sub-setting characters in a single string.

  - Since the main functions are S3 functions, other packages may add
    functionality for additional classes.

- *Concise function and argument names*.

- *Performance aware*: Despite the many checks performed, the functions
  are kept reasonably speedy, through the use of the ‘Rcpp’, ‘collapse’,
  and ‘data.table’ R-packages. Most of the heavy lifting in this package
  is done by the ‘collapse’ package.

 

## Methods and Functions

The main focus is on the following generic S3 methods:

- `sb_x()`: method to extract, exchange, or duplicate indices.

- `sb_rm()`: method to remove indices.

- `sb_set()`: method to modify (transform or replace values) subsets of
  an object by **reference**.

- `sb_mod()`: method to return a **copy** of an object with modified
  (transformed or replaced values) subsets.

- `sb_coe()`: coerce and transform a whole object, or a recursive subset
  of an object.

- `sb_before()`, `sb_after()`: methods to insert new values before or
  after an index along a dimension of an object.

- `sb_rec()`: not actually a method, but a function that can be combined
  with the above methods, for recursive sub-setting operations.

Beside these generic S3 methods, additional specialized sub-setting
functions are provided:

- `aes_pro()`: programmatically friendly and stable version of the
  `aes()` function.

- `sb_str()`: extract or replace a subset of characters of a single
  string (each single character is treated as a single element).

- `sb_a()`: extract multiple attributes from an object.

And finally, a couple of helper functions for creating ranges, sequences
and indices (often needed in sub-setting) are provided:

- `seq_rec()`: Recursive sequence generator (for example to generate a
  Fibonacci sequence)

- `seq_names()`: create a range of indices from a specified starting and
  ending name.

- `sub2coord()`, `coord2ind()`: Convert subscripts (array indices) to
  coordinates, coordinates to flat indices, and vice-versa.

 

## Help pages

For an explanation of the classes, and how each class is treated by
‘subsets’, see `subsets_classes`.

For an explanation of the common indexing arguments in the generic
methods, see `subsets_indx_args`.

 

## Installing & Loading

One can install ‘subsets’ from GitHub like so:

``` r
remotes::install_github("https://github.com/tony-aw/subsets")
```

Special care has been taken to make sure the function names are clear,
and that the function names are unlikely to conflict with core R, the
recommended R packages, the rstudioapi package, or major packages from
the fastverse. So one can attach the package - thus exposing its
functions to the namespace - using:

``` r
library(subsets)
```

Should the user wish to expose specific functions from ‘subsets’ ONLY
within a specific environment, like only within a specific function, one
can use the following:

``` r
tinycodet::import_LL("subsets", selection = ... )
```

 

## Benchmarks

Due to the many checks and conversions performed by the `subsets::`
functions, to make sub-setting more programmatically and beginner
friendly, the functions are almost necessarily slower than base R’s
`[`-like operators.

However, a considerable effort was made to keep the speed loss to a
minimum. Generally, the speed loss is indeed neglible, and in some cases
there is even speed improvement (thanks to the heavy lifting performed
by the ‘collapse’ pakackage).

Below are some benchmarks to give one an idea of the speed loss. These
are just examples; speed is determined by a great number of factors.

 

``` r
library(bench)
library(ggplot2)
```

### Matrix

``` r
x.mat <- matrix(seq_len(1000*1000), ncol = 1000)
colnames(x.mat) <- sample(c(letters, NA), 1000, TRUE)
sel.rows <- 1:100
sel.cols <- rep(sample(letters[1:13]), 10)
bm.matrix <- bench::mark(
  "subsets" = sb_x.matrix(x.mat, sel.rows, sel.cols),
  "base R" = x.mat[sel.rows, lapply(sel.cols, \(i) which(colnames(x.mat) == i)) |> unlist(), drop = FALSE],
  min_iterations = 1e4
)
summary(bm.matrix, relative = TRUE)
```

    #> # A tibble: 2 × 6
    #>   expression   min median `itr/sec` mem_alloc `gc/sec`
    #>   <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    #> 1 subsets     1      1         1.41      1.09     1.18
    #> 2 base R      1.42   1.44      1         1        1

 

### Array (3D)

``` r
x.dims <- c(1000, 900, 4)
x.3d <- array(1:prod(x.dims), x.dims)
sel.rows <- 1:900
sel.lyrs <- c(TRUE, FALSE, TRUE, FALSE)
bm.3d <- bench::mark(
  "subsets" =  sb_x.array(x.3d, rcl = n(sel.rows, NULL, sel.lyrs)),
  "base R + abind" = abind::asub(x.3d, idx = list(sel.rows, sel.lyrs), dims = c(1,3)),
  min_iterations = 1e4
)
summary(bm.3d, relative = TRUE)
```

    #> # A tibble: 2 × 6
    #>   expression       min median `itr/sec` mem_alloc `gc/sec`
    #>   <bch:expr>     <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    #> 1 subsets         1      1         1.01      1.00     1.03
    #> 2 base R + abind  1.00   1.02      1         1        1

 

### Data.frame

``` r
n <- 1e5
chrmat <- matrix(
  sample(letters, n*400, replace = TRUE), ncol = 400
)
intmat <- matrix(
  seq.int(n*400), ncol = 400
)
x <- cbind(chrmat, intmat) |> as.data.frame()
rm(list = c("chrmat", "intmat"))
colnames(x) <- make.names(colnames(x), unique = TRUE)
sel.cols <- rep(sample(names(x), 10), 4)
sel.rows <- 1:1000
bm.df <- bench::mark(
  "subsets" = sb_x.data.frame(x, sel.rows, sel.cols),
  "base R" = x[sel.rows, match(sel.cols, names(x)), drop = FALSE],
  min_iterations = 1e4
)
summary(bm.df, relative = TRUE)
```

    #> # A tibble: 2 × 6
    #>   expression   min median `itr/sec` mem_alloc `gc/sec`
    #>   <bch:expr> <dbl>  <dbl>     <dbl>     <dbl>    <dbl>
    #> 1 subsets     1      1         2.03      1        1.52
    #> 2 base R      2.78   1.96      1         1.10     1

 

### Plots

Plots of the benchmarks, same as the tables above.

``` r
library(ggplot2)
library(patchwork)
ggp.mat <- autoplot(bm.matrix) + ggtitle("matrix")
#> Loading required namespace: tidyr
ggp.3d <- autoplot(bm.3d) + ggtitle("3d array")
ggp.df <- autoplot(bm.df) + ggtitle("data.frame")
ggp.mat / ggp.3d / ggp.df
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

 

## See Also

‘subsets’ relies on the ‘Rcpp’, ‘collapse’ and ‘data.table’ R-packages
to ensure an acceptable performance of its functions despite the many
checks that these functions perform. I also recommend using these
packages for other sub-setting and data wrangling functionalities.
‘subsets’ uses a modified version of the `abind::abind()` function from
the ‘abind’ R-package. This R-package is recommended for binding and
sub-filling arrays of arbitrary dimensions.

Besides these package, the following R packages work very nicely
together with ‘subsets’:

- ‘stringi’: THE R package for fast and concise string manipulation - an
  essential part of any programming language.

- ‘tinycodet’: Helps the user with their coding etiquette. Focuses on 4
  aspects: (1) safe functionalities, (2) an import system that combines
  benefits of using without attaching and attaching a package, (3)
  extending the capabilities of the aforementioned ‘stringi’
  package, (4) functions to reduce repetitive code.

 

## Changelog (EXPERIMENTAL VERSIONS)

- 31 October 2023: Very first GitHub upload. Also: Halloween!
- 12 November 2023: Optimised the code more. Update the benchmarks with
  more representative sized objects. Merged `sb_rp()` and `sb_tf()` into
  `sb_mod()`, and added `sb_set()`. Placed explanations of the common
  indexing arguments in a separate help page for a better overview (and
  less typing for me). Replaced the `.attr` argument with the `rat`
  argument in `sb_x()` and `sb_rm()`. Added the `sub2ind` and `ind2sub`
  functions.
- 18 November 2023: Fixed several bugs in the `sb_set()` method. Added
  the `sb_coe()` method. Added considerably more tests. Expanded the
  documentation a bit. Expanded the `sub2ind` - functions into 4 more
  comprehensive functions. The `sb_str()` function now also has a
  replacement method.
- 22 November 2023: Made the arguments in the `sub2ind` - functions more
  consistent, and also expanded their documentation a bit. Added an
  expanded explanation of the classes for argument `x` in a separate
  help file for a better overview (and less typing for me). Added more
  badges. Improved the tests a bit. Added an `.onAttach()` message
  function. The `aes_pro()` function now uses formula input rather than
  character input.
- 27 November 2023: Added the `rcl` argument to the `sb_*.array()`
  methods, specifically for 3-dimensional arrays. Produced more tests,
  and re-organised the tests. Added the `with_pro()` function, which -
  like `aes_pro()` - is the programmatically friendly version of
  `with()`, and works with a formula instead of expression. Added
  reference to ‘abind’ in bibliography of the `sb_before()` and
  `sb_after()` methods. Normalized the order of the classes in the
  documentation of all the generic methods of ‘subsets’. Added the `n()`
  function, as short-hand for `list()`, and also functions as the nested
  version of `c()`. Added an additional help page explaining
  Non-Standard Evaluation (NSE); ‘subsets’ avoids the usage of NSE
  whenever possible.

 
