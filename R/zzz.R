

.onAttach <- function(libname, pkgname) {

  if (!getOption(".pythonR_initialized", default = FALSE) && isFALSE(testthat::is_testing())) {
    # Make sure python is installed:
    py_installed <- python_is_installed()
    if(isFALSE(py_installed)){
      user_permission <- utils::askYesNo("Install python? This will take some time")
      if (isTRUE(user_permission)) {
        reticulate::install_python()
      } else {
        packageStartupMessage("Python is required for this Package. Run `reticulate::install_python()` before proceeding")
      }
    }

    # make sure miniconda is installed:
    miniconda_installed <- miniconda_is_installed()
    if(isFALSE(miniconda_installed)){
      user_permission <- utils::askYesNo("Install miniconda? Downloads ~50MB and takes time")
      if (isTRUE(user_permission)) {
        reticulate::install_miniconda()
      } else {
        packageStartupMessage("miniconda is required for this Package. You should run `reticulate::install_miniconda()` before proceeding")
      }
    }

    # TODO: potentially load some required python packages

    # Set the flag to indicate package initialization
    options(.pythonR_initialized = TRUE)
  }
}
