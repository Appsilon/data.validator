% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assertions.R
\name{validate_if}
\alias{validate_if}
\title{Verify if expression regarding data is TRUE}
\usage{
validate_if(
  data,
  expr,
  description = NA,
  obligatory = FALSE,
  skip_chain_opts = FALSE,
  success_fun = assertr::success_append,
  error_fun = assertr::error_append,
  defect_fun = assertr::defect_append
)
}
\arguments{
\item{data}{A data.frame or tibble to test}

\item{expr}{A Logical expression to test for, e.g. \code{var_name > 0}}

\item{description}{A character string with description of assertion.
The description is then displayed in the validation report}

\item{obligatory}{If TRUE and assertion failed the data is marked as defective.
For defective data, all the following rules are handled by defect_fun function}

\item{skip_chain_opts}{While wrapping data with \link{validate} function, \code{success_fun} and
\code{error_fun} parameters are rewritten with \code{success_append} and \code{error_append}
respectively. In order to use parameters assigned to the function directly set skip_chain_opts
to TRUE}

\item{success_fun}{Function that is called when the validation pass}

\item{error_fun}{Function that is called when the validation fails}

\item{defect_fun}{Function that is called when the data is marked as defective}
}
\description{
The function checks whether all the logical values returned by the expression are TRUE.
The function is meant for handling all the cases that cannot be reached by using
\link{validate_cols} and \link{validate_rows} functions.
}
\seealso{
validate_cols validate_rows
}
