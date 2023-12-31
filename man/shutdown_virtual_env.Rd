% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/setup-python-pkgs.R
\name{shutdown_virtual_env}
\alias{shutdown_virtual_env}
\title{Shut down virtual environment}
\usage{
shutdown_virtual_env(env_name = PYTHON_R_ENV, force = TRUE)
}
\arguments{
\item{env_name}{an environment name to use for \strong{new} virtual or conda environments.}

\item{force}{Logical (\code{TRUE}/\code{FALSE}). If \code{FALSE}, confirm before removing
packages or virtual environments}
}
\description{
Shut down virtual environment
}
\details{
Call this to shutdown a virtual environment

This will shut down the environment...
Shutting it down means you'd have to install the packages again, should we
need them again. Given that, you should only shut this down -if you are done-
using the python packages.
}
