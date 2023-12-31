% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/safer_dollar.R
\name{safer_partialmatch}
\alias{safer_partialmatch}
\title{Set safer dollar, arguments, and attribute matching}
\usage{
safer_partialmatch()
}
\value{
Sets the options. Returns nothing.
}
\description{
This function simply calls the following:

\if{html}{\out{<div class="sourceCode r">}}\preformatted{options(
   warnPartialMatchDollar = TRUE,
   warnPartialMatchArgs = TRUE,
   warnPartialMatchAttr = TRUE
 )

}\if{html}{\out{</div>}}

Thus it forces 'R' to give a warning when partial matching occurs when using
the dollar (\link{$}) operator, or when other forms of partial matching occurs.
}
\examples{
\dontshow{if (interactive()) (if (getRversion() >= "3.4") withAutoprint else force)(\{ # examplesIf}
interactive()


safer_partialmatch()
data(iris)
head(iris)
iris$Sepal.Length <- iris$Sepal.Length^2
head(iris)
\dontshow{\}) # examplesIf}
}
