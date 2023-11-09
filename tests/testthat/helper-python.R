# helper functions to skip tests if we don't have the 'foo' module

skip_if_no_scipy <- function() {
  have_scipy <- reticulate::py_module_available("scipy")
  if (!have_scipy)
    testthat::skip("scipy not available for testing")
}

skip_if_no_pandas <- function() {
  have_pandas <- reticulate::py_module_available("pandas")
  if (!have_pandas)
    testthat::skip("pandas not available for testing")
}

skip_if_no_numpy <- function() {
  have_numpy <- reticulate::py_module_available("numpy")
  if (!have_numpy)
    testthat::skip("numpy not available for testing")
}


skip_if_no_python <- function() {
  have_python <- python_is_installed()
  if (!have_python)
    testthat::skip("python not available for testing")
}

skip_if_no_conda <- function() {
  have_conda <- miniconda_is_installed()
  if (!(have_conda))
    testthat::skip("miniconda not available for testing")
}
