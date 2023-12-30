
<!-- README.md is generated from README.Rmd. Please edit that file -->

# subsets

<!-- badges: start -->

[![Project Status: WIP - Initial development is in progress, but there
has not yet been a stable, usable release suitable for the
public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)
[![](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![](https://img.shields.io/badge/ORCID-0000--0001--9498--8379-green.svg)](https://orcid.org/0000-0001-9498-8379)
<!-- badges: end -->

subsets: A Holistic Grammar of Subsets

 

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

    To my surprise, there is no comprehensive R-package
    (as far as I could see at least) that actually attempts to "fix" the
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
    data.table, tibble, and tidytable class, and classes that are
    straight-forward inheritors from these classes (such as
    sf-data.frames or sf-data.tables).

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

- `sb_x()`: S3 method to extract, exchange, or duplicate subsets.

- `sb_rm()`: S3 method to un-select (“remove”) subsets.

- `sb_set()`: S3 method to modify (transform or replace values) subsets
  of an object by **reference**. Since it’s by reference, it does not
  allow coercion.

- `sb_mod()`: S3 method to return a **copy** of an object with modified
  (transformed or replaced values) subsets. Has auto-coercion to a
  certain extent.

- `sb_coe()`: S3 method to coerce and transform a whole object, or a
  recursive subset of an object.

- `sb_before()`, `sb_after()`: S3 methods to insert new values before or
  after an index along a dimension of an object.

- `sb_rec()`: a function that can be combined with the above methods,
  for recursive sub-setting operations.

Beside these generic S3 methods, additional specialized sub-setting
functions are provided:

- `aes_pro()` and `with_pro()`: programmatically friendly and stable
  version of the `with()` and `ggplot2::aes()` functions.

- `sb_str()`: extract or replace a subset of characters of a single
  string (each single character is treated as a single element).

- `sb_a()`: extract multiple attributes from an object.

And finally, a couple of helper functions for creating ranges, sequences
and indices (often needed in sub-setting) are provided:

- `n()`: Nested version of `c()`, and short-hand for `list()`.

- `idx_by()`: Compute grouped indices.

- `seq_rec()`: Recursive sequence generator (for example to generate a
  Fibonacci sequence)

- `seq_names()`: create a range of indices from a specified starting and
  ending name.

- `sub2coord()`, `coord2ind()`: Convert subscripts (array indices) to
  coordinates, coordinates to flat indices, and vice-versa.

 

Most of the heavy lifting in the internal code of this package is done
by the ‘collapse’ package.

 

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
- 1 December 2023: In the `sb_mod()` method, auto-coercion in
  data.frame-like objects is now allowed, provided rows are not
  specified (i.e. whole columns only). Expanded the help page of the
  `sb_mod()` function considerably. Fixed a bug in `aes_pro()`. Added
  the `safer_partialmatch()` function. Added the `idx_by()` function to
  compute grouped indices. Added the `dt_*` functions for functional
  forms of some data.table operations. Cleaned up some internal code.
  Added yet more tests (now over 42,000 tests). Benchmark removed from
  the Read-Me: planning to move them to the website, which will be build
  once this package is a bit more stable.
- 14 December 2023: Experimental website published.
- 16 December 2023: Small documentation improvements.
- 19 December 2023: `sb_set.data.frame()` now has the same coercion
  rules as `sb_mod.data.frame()`. Auto-coercion rules moved to the
  `subsets_classes` help page.
- 30 December 2023: Removed the (arguably unnecessary) levels check in
  the internal code for improved speed. Added the `idx_ord_`-functions,
  and added some tests for these functions. Incorporated
  `` data.table::`%chin%` `` into the internal code for additional speed
  improvement. Added re-exports of the `data.table::first()` and
  `data.table::last()` functions. Added tests for
  `sb_mod.data.frame(x, rp = rp)` and `sb_set.data.frame(x, rp = rp)`.
  Added a test that checks if the return call is present in all
  functions where it is needed. Added some general tests here and there.
  With the newly added tests, About 47k tests in total are now present
  in this package.

 
