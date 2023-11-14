
<!-- README.md is generated from README.Rmd. Please edit that file -->

# pythonR <a href='https://github.com/barrettk/pythonR'><img src='man/figures/logo.png' align="right" height="140" /></a>

`pythonR` includes helper functions for using Python in R. It is
essentially a wrapper of the `reticulate` R package, and can helpful in
an R package development or project environment where you want more
readable code for setting up a python environment and installing any
necessary python dependencies.

You begin by setting up a python environment (conda or virtual) with the
required imports you will need:

``` r
## With a conda environment (the default) ##
# Much faster loading time after you've installed the packages once, but larger package size
py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))
py_env

# Specify a specific conda environment and conda installation
conda_envs <- reticulate::conda_list()
conda_paths <- pythonR::get_conda_paths()

py_env <- setup_py_env(
  py_pkgs = c("pandas", "numpy", "scipy"),
  conda_path = conda_paths[1],
  conda_env = conda_envs$python[1],
  update = TRUE
)

## Using a virtual environment ##
# Smaller package size, but packages must be re-installed for each R session
py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), py_env = "virtual")
py_env

# shutdown virtual environment
shutdown_virtual_env(py_env$env_name)
```

Note that you can always install packages to the environment you created
at a later time:

``` r
# Installing a package after setup (works with both environment types)
py_env <- setup_py_env(py_pkgs = c("pandas", "numpy"))
install_py_pkgs(py_pkgs = c('scipy'), env_name = py_env$env_name)
```

Additional helper functions are provided for ensuring required python
packages are installed or importing them at various stages within an R
function/script. A simple example is provided below, but see
`?pythonR_examples` for fully fleshed out examples and more details.
Note that some python modules may not be able to be installed/imported
this way. `tiktoken`, `openai`, and other modules may require specific
installation procedures and/or the presence of API keys.

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
py_env <- setup_py_env(
  py_pkgs = c("pandas", "numpy", "scipy"),
  py_env = "conda"
)

# Use any python functions coded in R
my_py_function()
```
