
#' Package directory containing example python scripts
#' @keywords internal
EXAMPLE_PYTHON_DIR <- system.file("python", package = "pythonR", mustWork = TRUE)

# Example functions -------------------------------------------------------




#' Retrieve example function from `pythonR_examples`
#'
#' @rdname pythonR_examples
#'
#' @param example The name of an example to load. See `ls(pythonR_examples)` for
#'        options.
#'
#' @export
get_py_example <- function(example = "py_add"){
  checkmate::assert_true(example %in% ls(pythonR_examples))
  example_func <- get(example, envir = pythonR_examples)
  eval(example_func)
}

py_add <- "{
# Simple python function
#
# @param x a number
# @param y a number
#
# @details
# This function does not require imports or virtual environment (much faster)
py_add <- function(
    x = 5,
    y = 10
){

  # source specific python script
  py_script <- file.path(EXAMPLE_PYTHON_DIR, 'script-add.py')

  # Source python script if python is installed
  if(isTRUE(python_is_installed())){
   reticulate::source_python(py_script)
  }

  # `py_add` is a function defined in `py_script`
  sum <- py_add(x, y)

  return(sum)
}
}"


py_check_path <- "{
# Check that file exists at location using python
#
# Example with working with `file_path` and importing a python package that
# *comes with python*
#
# @param file_path path to audio file
#
# @details
# The `os` python module comes with python:
# - Virtual environment (or conda) is only needed when we have to *install*
#   packages.
#
#
py_check_path <- function(
    file_path = file.path(EXAMPLE_PYTHON_DIR, 'script-check-path.py')
){

  # import main python functions
  import_main_py()

  # source specific python scripts
  py_script <- file.path(EXAMPLE_PYTHON_DIR, 'script-check-path.py')
  reticulate::source_python(py_script)

  # some python stuff
  # note: make sure the python function names dont overlap with R function names
  file_exists <- check_path_py(file_path)

  # since we called `import_main_py()` before sourcing the script, we can also
  # reference the python function `check_path_py` via `main$check_path_py`:
  file_exists2 <- main$check_path_py(file_path)
  file_exists == file_exists2

  return(file_exists)
}
}"


py_array <- "{
# Create list of arrays using pandas
#
# Example that requires installing and importing other python packages
#
py_array <- function(){

  # Check that required packages for this function have been installed
  required_py_pkgs <- c('pandas', 'scipy', 'numpy')
  checkmate::assert_true(check_py_pkgs_installed(required_py_pkgs))

  # import main python modules
  import_main_py()

  # import required python packages
  import_py_pkgs(py_pkgs = required_py_pkgs)

  # import other packages that come with python (e.g. `difflib`) as R functions:
  difflib <- reticulate::import('difflib')
  filecmp <- reticulate::import('filecmp')

  # This object (`py_objs`) will keep track of everything in your python environment
  # even after it's called
  py_objs <- reticulate::py_run_string(\"data={'col1': [1, 2], 'col2': [3, 4]}\")

  # R call that uses the `pandas` python library, and an object created using python
  data_df <- pandas$DataFrame(data=py_objs$data)

  # source specific python scripts
  # - this script -also- imports packages.
  # - you can import packages have been installed via `setup_py_env` or
  #  `install_py_pkgs` either via `import_py_pkgs` -or- within the script (you
  #   dont need both - this is redundant for illustration purposes)
  py_script <- file.path(EXAMPLE_PYTHON_DIR, 'script-pd-array.py')
  reticulate::source_python(py_script)

  # df2 appeared in `py_objs` after sourcing script-pd-array.py
  data_return <- py_objs$df2

  return(
    list(
      data_df,
      data_return
    )
  )
}
}"



#' Environment containing `pythonR` example functions
#'
#'
#' @examples
#' \dontrun{
#' # Get list of available examples.
#' ls(pythonR_examples)
#'
#'
#' # View the functions
#' get("py_add", envir = pythonR_examples)
#' get("py_check_path", envir = pythonR_examples)
#' get("py_array", envir = pythonR_examples)
#'
#' # Assign the function to use it
#' py_add <- get_py_example("py_add")
#' py_add(3, 4)
#'}
#'
#' @rdname pythonR_examples
#' @export
pythonR_examples <- list2env(
  list(
    "py_add" = parse(text = py_add),
    "py_check_path" = parse(text = py_check_path),
    "py_array" = parse(text = py_array)
  ),
  parent = emptyenv()
)
