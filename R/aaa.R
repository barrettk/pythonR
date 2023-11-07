utils::globalVariables(".data")


# Make sure python is set up ----------------------------------------------

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

is_miniconda_installed <- function(path = reticulate::miniconda_path()){
  exe <- if (xfun::is_windows()){
    "condabin/conda.bat"
  }else{
    "bin/conda"
  }
  file.path(path, exe)
}
