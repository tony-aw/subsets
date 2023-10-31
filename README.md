
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
    With R" by Reece Goding. These document point out many
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

 

## Properties

The subsets package provides easier subsetting functions with the
following properties:

- *Programmatically friendly*:

  - Non-standard evaluation is highly controversial (and for good
    reasons!), and therefore completely absent in this R package;

  - Name-based instead of position-based arguments;

  - Missing arguments can be filled with `NULL`, instead of using dark
    magic like `base::quote(expr = )`.

  - Functions are pipe-friendly.

- *Beginner friendly*:

  - No (silent) vector recycling;

  - Extracting and removing subsets uses the same syntax.

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

- *Careful handling of name-based indexing*:

  - Sub-setting object by index names returns ALL indices with that
    name, not just the first;

  - Data.frame-like objects are forced to have unique column names;

  - Selecting non-existing names always gives an error.

- *Support a wide variety of data types*:

  - Support vector-like (atomic) objects (vectors, matrices, arrays);

  - Support lists;

  - Support factors;

  - Support data.frame-like objects (data.frame, data.table, tibble,
    tidytable, etc.).

- *Concise function and argument names*.

- *Special functions*: for string subsetting, vectorized recursive list
  subsetting, and even for the column selection subsetting used in
  ggplot2’s `aes()` function.

- *Performance aware*: Despite the many checks performed, the functions
  are kept reasonably speedy, through the use of the ‘Rcpp’, ‘collapse’,
  and ‘data.table’ R-packages. See the benchmarks section below for some
  examples.

 

## Methods and Functions

The main focus is on the following generic S3 methods:

- `sb_x`: method to extract, exchange, or duplicate indices.

- `sb_rm`: method to remove indices.

- `sb_tf`: method to transform values of subsets.

- `sb_rp`: method to replace values of subsets.

- `sb_before`, `sb_after`: methods to insert new values before or after
  an index along a dimension of an object.

Beside these generic S3 methods, additional specialized sub-setting
functions are provided:

- `aes_pro`: programmatically friendly and stable version of ggplot2’s
  aesthetic sub-setting function.

- `sb_str`: subset a single string (each single character is treated as
  a single element).

- `sb_rec`: recursive sub-setting of lists.

And finally, a couple of helper functions for creating ranges and
sequences (occasionally needed in sub-setting) are provided:

- `seq_rec`: Generalized recursive sequence generator.

- `seq_names`: create a range of indices from a specified starting and
  ending name.

 

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

Due to the MANY, MANY checks and conversions performed by the
`subsets::` functions, to make sub-setting more programmatically and
beginner friendly, the functions are almost necessarily slower.

However, a considerable effort was made to keep the speed loss to a
minimum. The exact speed loss depends on the situation. For example, the
`subsets::` functions are about as fast as base R when sub-setting using
names, but base R is much faster when sub-setting matrices using only
numbers. But subsetting data.frames are actually faster with the
`subsets::` functions than in base R.

Below are some benchmarks to give one an idea of the speed loss. These
are just examples; speed is determined by a great number of factors.

``` r
library(rbenchmark)
library(subsets)
```

``` r
x <- matrix(1:50000, ncol = 20)
colnames(x) <- c(letters[1:18], "a", NA)
bm.matrix <- benchmark(
  "subsets" = sb_x(x, 1:100, c("a", "a")),
  "base R" = x[1:100, lapply(c("a", "a"), \(i) which(colnames(x) == i)) |> unlist(), drop = FALSE],
  replications = 100000
)
print(bm.matrix)
```

    #>      test replications elapsed relative user.self sys.self user.child sys.child
    #> 2  base R       100000    1.09     1.00      0.74        0         NA        NA
    #> 1 subsets       100000    1.70     1.56      1.05        0         NA        NA

``` r
x.dims <- c(1000,100,4)
x <- array(1:prod(x.dims), x.dims)
idx <- list(1:100, c(TRUE, TRUE, TRUE, FALSE))
dims <- c(1, 3)
bm.array <- benchmark(
  "subsets" = sb_x(x, idx, dims),
  "base R + abind" = abind::asub(x, idx, dims),
  replications = 10000
)
print(bm.array)
```

    #>             test replications elapsed relative user.self sys.self user.child
    #> 2 base R + abind        10000    1.20    1.026      0.72     0.16         NA
    #> 1        subsets        10000    1.17    1.000      0.56     0.11         NA
    #>   sys.child
    #> 2        NA
    #> 1        NA

``` r
n <- 1e5
x <- data.frame(
  a = seq_len(n),
  b = sample(letters, size = n, replace = TRUE),
  c = seq_len(n) * -1,
  d = sample(rev(letters), size = n, replace = TRUE)
)
bm.df <- benchmark(
  "subsets" = sb_x(x, 1:1000, c("a", "a")),
  "base R" = x[1:1000, lapply(c("a", "a"), \(i) which(colnames(x) == i)) |> unlist(), drop = FALSE],
  replications = 1e4
)
print(bm.df)
```

    #>      test replications elapsed relative user.self sys.self user.child sys.child
    #> 2  base R        10000    0.49    1.225       0.2        0         NA        NA
    #> 1 subsets        10000    0.40    1.000       0.3        0         NA        NA

 

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

 
