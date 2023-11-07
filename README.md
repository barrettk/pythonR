
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pythonR

`pythonR` includes helper functions for using Python in R. It is
essentially a wrapper of the `reticulate` R package, and can helpful in
an R package development or project environment where you want more
readable code for setting up a python environment and installing any
necessary python dependencies.

It may decrease some of the flexibility offered via `reticulate` for
setting up virtual or conda environments (mostly in ignoring the
`RETICULATE_PYTHON` python path), but it allows you to reproducibly set
up the same environment each session, and install all python packages in
a single call.

You begin by setting up a python environment (conda or virtual) with the
required imports you will need:

``` r
 ## With a conda environment (the default) ##
 # much faster loading time after you've installed the packages once
 py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))
 py_env

 # Put the conda environment under a specific name if you want to store multiple
 # note: you must restart your R session if you've already generated an environment
 py_env <- setup_py_env(
   py_pkgs = c("pandas", "numpy", "scipy"),
   conda_name = PYTHON_R_ENV,
   update = TRUE
 )

 ## Using a virtual environment ##
 py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), virtual_env = TRUE)
 py_env

 # shutdown virtual environment
 shutdown_virtual_env(py_env$env_name)
```

Note that you can always install packages to the environment you created
at a later time:

``` r
 # Installing a package after setup (works with both environment types)
 setup_py_env(py_pkgs = c("pandas", "numpy"))
 install_py_pkgs(py_pkgs = c('scipy'))
```

Additional helper functions are provided for ensuring required python
packages are installed or importing them at various stages within an R
function/script. A simple example is provided below, but see
`?pythonR_examples` for fully fleshed out examples and more details.

``` r
my_py_function <- function(){
  # Check that python packages are installed
  required_py_pkgs <- c('pandas', 'scipy', 'numpy')
  checkmate::assert_true(check_py_pkgs_installed(required_py_pkgs))
  
    # import main python modules
  import_main_py()

  # import required python packages
  import_py_pkgs(py_pkgs = required_py_pkgs)
  
  # source python script or load python functions into R
  
  # do something...
}
```

``` r
# Set up a python environment once per R session
py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))

# Use any python functions coded in R
my_py_function()
```
