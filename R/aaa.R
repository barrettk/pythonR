utils::globalVariables(".data")


# Make sure python is set up ----------------------------------------------

#' Check that python is installed
#'
#' @param use_environment Logical (`TRUE`/`FALSE`). Passed to
#'       [reticulate::py_discover_config()]
#'
#' @export
python_is_installed <- function(use_environment = NULL){
  config <- tryCatch({
    reticulate::py_discover_config(use_environment = use_environment)
  }, error = identity)

  if(inherits(config, "error")){
    FALSE
  }else{
    fs::file_exists(config$python)
  }
}

#' Check that minicoonda is installed at specified location
#'
#' @param path path to miniconda installation
#'
#' @note the assumed extension changes depending on operating system.
#'
#' @keywords internal
miniconda_is_installed <- function(path = reticulate::miniconda_path()){
  exe <- if (xfun::is_windows()){
    "condabin/conda.bat"
  }else{
    "bin/conda"
  }
  conda_path <- file.path(path, exe)
  file.exists(conda_path)
}
