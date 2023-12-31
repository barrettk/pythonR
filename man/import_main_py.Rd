% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup-python-pkgs.R
\name{import_main_py}
\alias{import_main_py}
\title{Import main module and default functions}
\usage{
import_main_py(envir = NULL)
}
\arguments{
\item{envir}{environment to load the main modules into}
}
\description{
Import main module and default functions
}
\details{
See \verb{main$} and \verb{builtins$} after running this function

Sourcing scripts \strong{after} importing the main modules will add those functions
under \verb{main$}
}
