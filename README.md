
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
    And R is no exception.

    This becomes quite apparent when one reads (online) documents such
    as "The R Inferno" by Patrick Burns, and "Frustration: One Year
    With R" by Reece Goding. These documents point out many
    inconsistencies, and sub-setting related inconsistencies make up a
    good portion of these documents.

    To my surprise, there is no comprehensive R package (as far as I
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

The Goal of the ‘subsets’ package is NOT to replace the square-brackets
operators (`[`, `[[`, `[<-`, and `[[<-`), but to provide **alternative**
sub-setting methods and functions, to be used in situations where the
square-brackets operators are inconvenient.

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

  - Smart with sub-setting recursive lists.

- *Explicit copy semantics*:

  - Sub-set operations that increase or decrease the number of elements
    in an object, and thus change its memory allocations, always return
    a modified copy of the object.

  - For sub-set operations that just change values in-place (similar to
    the \[\<- and \[\[\<- methods) the user can choose a method that
    modify the object by *reference*, or choose a method that return a
    *deep copy*.

- *Careful handling of names and other attributes*:

  - Sub-setting object by index names returns ALL indices with that
    name, not just the first.

  - Data.frame-like objects (see supported classes below) are forced to
    have unique column names.

  - Selecting non-existing names always gives an error.

  - Attributes of data.frame-like objects (see supported classes below)
    are always preserved when sub-setting.

  - For other object types, the user can specify whether to preserve
    Attributes, or use R’s `[` attribute behaviour (i.e. drop most
    attributes). This is to ensure compatibility with R packages that
    create their own attribute behaviour for sub-setting.

- *Support a wide variety of data types*:

  - Support vector-like (atomic) objects (vectors, matrices, arrays).

  - Support lists.

  - Support factors.

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
  and ‘data.table’ R-packages.

 

## Methods and Functions

The main focus is on the following generic S3 methods:

- `sb_x`: method to extract, exchange, or duplicate indices.

- `sb_rm`: method to remove indices.

- `sb_mod`: method to return a **copy** of an object with modified
  (transformed or replaced values) subsets.

- `sb_set`: method to modify (transform or replace values) subsets of an
  object by **reference**.

- `sb_before`, `sb_after`: methods to insert new values before or after
  an index along a dimension of an object.

- `sb_rec`: not actually a method, but a function that can be combined
  with the above methods, for recursive sub-setting operations.

Beside these generic S3 methods, additional specialized sub-setting
functions are provided:

- `aes_pro`: programmatically friendly and stable version of ggplot2’s
  aesthetic sub-setting function.

- `sb_str`: extract or replace a subset of characters of a single string
  (each single character is treated as a single element).

- `sb_a`: extract multiple attributes from an object.

And finally, a couple of helper functions for creating ranges, sequences
and indices (sometimes needed in sub-setting) are provided:

- `seq_rec`: Generalized recursive sequence generator.

- `seq_names`: create a range of indices from a specified starting and
  ending name.

- `sub2ind`, `ind2sub`: Convert subscripts (array indices) to flat
  indices, and vice-versa.

 

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
minimum. The exact speed loss depends on the situation. For example, the
`subsets::` functions are about as fast as base R when sub-setting using
names, but base R is faster when sub-setting matrices using only
numbers. But sub-setting data.frames are actually faster with the
`subsets::` functions than in base R.

Below are some benchmarks to give one an idea of the speed loss. These
are just examples; speed is determined by a great number of factors.

 

### Matrix

``` r
x <- matrix(seq_len(1000*900), ncol = 900)
colnames(x) <- sample(c(letters, NA), 900, TRUE)
bm.matrix <- rbenchmark::benchmark(
  "subsets" = sb_x.matrix(x, 1:100, c("a", "a")),
  "base R" = x[1:100, lapply(c("a", "a"), \(i) which(colnames(x) == i)) |> unlist(), drop = FALSE],
  replications = 1e5,
  order = NULL
)
print(bm.matrix)
```

    #>      test replications elapsed relative user.self sys.self user.child sys.child
    #> 1 subsets       100000    3.06     1.01      1.61     0.08         NA        NA
    #> 2  base R       100000    3.03     1.00      1.66     0.03         NA        NA

 

### Array (4D)

``` r
x.dims <- c(1000, 900, 4)
x <- array(1:prod(x.dims), x.dims)
idx <- list(1:100, c(TRUE, TRUE, TRUE, FALSE))
dims <- c(1, 3)
bm.array <- rbenchmark::benchmark(
  "subsets" = sb_x.array(x, idx, dims),
  "base R + abind" = abind::asub(x, idx, dims),
  replications = 1e4,
  order = NULL
)
print(bm.array)
```

    #>             test replications elapsed relative user.self sys.self user.child
    #> 1        subsets        10000    9.55    1.000      4.33     0.61         NA
    #> 2 base R + abind        10000    9.72    1.018      3.19     0.43         NA
    #>   sys.child
    #> 1        NA
    #> 2        NA

 

### Data.frame

``` r

n <- 1e6
x <- data.frame(
  a = seq_len(n),
  b = sample(letters, size = n, replace = TRUE),
  c = seq_len(n) * -1,
  d = sample(rev(letters), size = n, replace = TRUE)
)
colsel <- rep("a", 4)
bm.df <- rbenchmark::benchmark(
  "subsets" = sb_x.data.frame(x, 1:1000, colsel),
  "base R" = x[1:1000, match(colsel, names(x))],
  replications = 1e4,
  order = NULL
)
print(bm.df)
```

    #>      test replications elapsed relative user.self sys.self user.child sys.child
    #> 1 subsets        10000    0.37    1.000      0.19        0         NA        NA
    #> 2  base R        10000    0.53    1.432      0.22        0         NA        NA

 

## See Also

‘subsets’ relies on the ‘Rcpp’, ‘collapse’ and ‘data.table’ R-packages
to ensure an acceptable performance of its functions despite the many
checks that these functions perform. I also recommend using these
packages for other subsetting and data wrangling functionalities.
Besides these package, the following R packages work very nicely
together with ‘subsets’:

- ‘stringi’: THE R package for fast and concise string manipulation - an
  essential part of any programming language.

- ‘abind’: Provides binding arrays along an arbitrary dimension.

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

 
