
#' Default environment name for python packages
#'
#' Can be use for virtual of conda environment
PYTHON_R_ENV <- "pythonR-python"

utils::globalVariables("main")
utils::globalVariables("builtins")


#' Set up a python environment with required packages
#'
#' @param py_pkgs vector of python packages to install
#' @param python_version The requested Python version. Ignored when attempting
#'        to install with a Python virtual environment.
#' @param py_env Environment type. Either a conda or virtual environment.
#' @param env_name an environment name to use for **new** virtual or conda environments.
#' @param conda_name name of the conda environment to use if `py_env = "conda"`.
#'        Run `reticulate::conda_list()` to see a list of available conda environments.
#'        The `conda_name` should match the `name` element of that list if you are loading
#'        *an existing* conda environment. If `conda_name` is not found in this list, a new
#'        one will be created matching the name of `env_name`.
#' @param update Logical (`TRUE`/`FALSE`). If `TRUE`, update packages that are
#'        already installed. Otherwise skip their installation.
#' @param required Logical (`TRUE`/`FALSE`). Is the requested copy of Python required?
#'        If TRUE, an error will be emitted if the requested copy of Python does not exist.
#'        If FALSE, the request is taken as a hint only, and scanning for other versions
#'        will still proceed. See [reticulate::use_condaenv()] for more details.
#'
#' @details
#'
#'
#' We will want to *install* all packages we need for the app once at the beginning,
#' and then *import* packages one at a time within each function that needs them
#'
#' If we use a virtual/conda environment, we are only using that for packages that dont
#' come with base python. i.e. the user still needs to have python installed.
#'
#' An environment will be required for each new R session.
#'
#' For a virtual environment, the user would be installing these packages *every time*
#' they want to run the app on a clean R session.
#'
#' For a conda environment, users would only need to install the required
#' packages once. I think this could be preferable, and would offer significant
#' speed improvements once the packages have been installed once.
#'
#' @note
#' You must restart your R session if you want to alternate between both environment types
#'
#'
#' @examples
#' \dontrun{
#'
#' ## With a conda environment (the default) ##
#' # much faster loading time after you've installed the packages once
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))
#' py_env
#'
#' # put the conda environment under a specific name
#' # note: you must restart your R session if you've already generated an environment
#' py_env <- setup_py_env(
#'   py_pkgs = c("pandas", "numpy", "scipy"),
#'   conda_name = PYTHON_R_ENV,
#'   update = TRUE
#' )
#'
#' ## With a virtual environment ##
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), py_env = "virtual")
#' py_env
#'
#' # shutdown virtual environment
#' shutdown_virtual_env(py_env$env_name)
#'
#' # Installing a package after setup (works with both environment types)
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy"), py_env = "conda")
#' install_py_pkgs(py_pkgs = c('scipy'), env_name = py_env$env_name)
#'
#' }
#'
#' @returns a named list of specifications pertaining to the python environment,
#' including the packages that are installed there.
#'
#' @export
setup_py_env <- function(
    py_pkgs = c("scipy", "pandas"),
    python_version = NULL,
    py_env = c("conda", "virtual"),
    env_name = PYTHON_R_ENV,
    conda_name = c("base", "r-reticulate", PYTHON_R_ENV),
    update = FALSE,
    required = FALSE
){

  # Make sure python is installed
  checkmate::assert_true(python_is_installed())

  py_env <- match.arg(py_env)
  conda_name <- match.arg(conda_name)

  if(py_env == "conda"){
    conda_envir_lst <- reticulate::conda_list()
    conda_envirs <- conda_envir_lst %>% dplyr::pull(.data$name)

    # pull previous conda environment if it exists, otherwise create new one
    if(conda_name %in% conda_envirs){
      # loads a local conda library
      env_path <- conda_envir_lst %>% dplyr::filter(.data$name == conda_name) %>%
        dplyr::pull(.data$python)
      # overwrite env_name with conda_name
      env_name <- conda_name
    }else{
      # Creates a local conda library
      env_path <- tryCatch(reticulate::conda_create(env_name), error = identity)
      # make sure the environment was created
      env_exists <- !inherits(env_path, "error") && fs::file_exists(env_path)
      if(isFALSE(env_exists)){
        cli::cli_abort(glue::glue("env {env_name} could not be created:\n\n {env_path$message}"))
      }
    }

    # Configure conda environment
    Sys.setenv(RETICULATE_PYTHON_ENV = env_path)
    reticulate::use_condaenv(condaenv = env_name,  required = required)
    message(glue::glue("a conda environment has been loaded at: {env_path}"))
  }else if(py_env == "virtual"){
    # create a new environment
    env_path <- tryCatch(reticulate::virtualenv_create(env_name), error = identity)

    # make sure the environment was created
    env_exists <- !inherits(env_path, "error") && reticulate::virtualenv_exists(env_name)
    if(isFALSE(env_exists)){
      cli::cli_abort(glue::glue("env {env_name} could not be created:\n\n {env_path$message}"))
    }

    # Configure vitual environment
    Sys.setenv(RETICULATE_PYTHON_ENV = env_path)
    reticulate::use_virtualenv(virtualenv = env_path, required = required)
    message(glue::glue("a virtual environment has been loaded at: {env_path}"))
  }

  # configure python in environment
  # suppress any warnings (will be returned via `use_condaenv` or `use_virtualenv`)
  config <- reticulate::py_config() %>% suppressWarnings()

  # install python packages
  installed_pkgs <- install_py_pkgs(
    py_pkgs,
    env_name = env_name,
    py_env = py_env,
    update = update,
    python_version = python_version
  )

  env_list <- list(
    env_name = env_name,
    env_path = env_path,
    config = config,
    installed_pkgs = installed_pkgs
  )

  # show all available conda environments if running a conda environment
  if(py_env == "conda"){
    env_list <- c(
      env_list,
      list(
        avail_conda_envirs = reticulate::conda_list()
      )
    )
  }

  return(env_list)
}

#' Install python packages to an environment
#'
#' @inheritParams setup_py_env
#'
#' @details
#' see `?reticulate::py_install` for more details
#'
#' @seealso [setup_py_env]
#'
#' @export
install_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    py_env = c("conda", "virtual"),
    env_name = PYTHON_R_ENV,
    update = FALSE,
    python_version = NULL
){

  py_env <- match.arg(py_env)

  virtual_env <- (py_env == "virtual")
  py_install_fn <- ifelse(isTRUE(virtual_env),
                          reticulate::virtualenv_install,
                          reticulate::conda_install
  )

  # Check if already installed
  if(isFALSE(update)){
    installed_pkgs <- reticulate::py_list_packages(env_name) %>%
      tibble::as_tibble()

    py_pkgs_installed <- py_pkgs[py_pkgs %in% installed_pkgs$package] %>%
      paste(collapse = ", ")
    if(!rlang::is_empty(py_pkgs_installed)){
      msg <- paste0(
        "The following packages have already been installed",
        " (set `update = TRUE` to override). Skipping...",
        glue::glue("\n\n{py_pkgs_installed}\n")
      )
      message(msg)
    }

    # filter list of packages to install
    to_install <- py_pkgs[!(py_pkgs %in% installed_pkgs$package)]
  }else{
    to_install <- py_pkgs
  }


  if(!is.null(to_install) && !rlang::is_empty(to_install)){
    purrr::walk(to_install, function(pkg){
      args <- list(pkg, python_version = python_version,
                   envname = env_name)
      do.call(py_install_fn, args)
    })
  }

  # Get new selection of installed packages
  installed_pkgs <- reticulate::py_list_packages(env_name) %>%
    tibble::as_tibble()

  # Check that packages can be loaded - warn if not
  check_py_pkgs_installed(py_pkgs, warn = TRUE)

  return(installed_pkgs)
}



#' Import python packages into the environment
#'
#' @inheritParams install_py_pkgs
#' @inheritParams import_main_py
#' @inheritParams py_module_available
#' @param delay_load Logical (`TRUE`/`FALSE`). Delay loading the module until it
#'        is first used? If FALSE, the module will be loaded immediately.
#'        See [reticulate::import()] for more information.
#' @details
#'
#' This function loops through `py_pkgs`, and does the equivalent of the following,
#' and assigns it to the parent environment:
#'
#' `scipy <- reticulate::import("scipy")`
#'
#' or
#'
#' `scipy <- reticulate::import_from_path("scipy", path = "path/to/module")`
#'
#'
#' @export
import_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    envir = NULL,
    path = get_py_path(),
    delay_load = FALSE
){

  if(is.null(envir)){
    envir <- parent.frame()
  }

  # import each package
  purrr::walk(py_pkgs, function(pkg_name){
    if(reticulate::py_module_available(pkg_name)){
      pkg <- if(is.na(path)){
        reticulate::import(pkg_name, delay_load = delay_load)
      }else{
        reticulate::import_from_path(pkg_name, path = path, delay_load = delay_load)
      }
      assign(pkg_name, pkg, envir = envir)
    }else{
      cli::cli_warn(
        glue::glue("Could not import {.code {{pkg_name}}}.
                    Please check that it is installed.",
                   .open = "{{", .close = "}}")
      )
    }
  })
}


#' Import main module and default functions
#'
#' @details
#'
#' See `main$` and `builtins$` after running this function
#'
#' Sourcing scripts **after** importing the main modules will add those functions
#' under `main$`
#'
#'
#' @param envir environment to load the main modules into
#'
#' @export
import_main_py <- function(envir = NULL){

  if(is.null(envir)){
    envir <- parent.frame()
  }

  # import main modules
  main <- reticulate::import_main()
  builtins <- reticulate::import_builtins()

  # assign to environment the function was -called- in
  assign("main", main, envir = envir)
  assign("builtins", builtins, envir = envir)
}





#' Shut down virtual environment
#'
#' @inheritParams install_py_pkgs
#' @param force Logical (`TRUE`/`FALSE`). If `FALSE`, confirm before removing
#'        packages or virtual environments
#'
#' @details
#' Call this to shutdown a virtual environment
#'
#' This will shut down the environment...
#' Shutting it down means you'd have to install the packages again, should we
#' need them again. Given that, you should only shut this down -if you are done-
#' using the python packages.
#'
#'
#' @export
shutdown_virtual_env <- function(env_name = PYTHON_R_ENV, force = TRUE){
  if(reticulate::virtualenv_exists(env_name)){
    reticulate::virtualenv_remove(env_name, confirm = !force)
    # Sys.setenv("RETICULATE_PYTHON_ENV" = "")
  }else{
    warning(glue::glue("Virtual environment {env_name} does not exist."))
  }
}



#' Helper function for ensuring required python packages are installed
#'
#' @param py_pkgs vector of python packages to check that are installed.
#' @inheritParams py_module_available
#' @param warn Logical (`TRUE`/`FALSE`). If `TRUE`, warn instead of throwing an error.
#'
#' @export
check_py_pkgs_installed <- function(py_pkgs, path = get_py_path(), warn = FALSE){
  pkgs_installed <- purrr::map_dfr(py_pkgs, function(pkg){
    py_module_available(pkg, path)
  })
  pkgs_avail <- pkgs_installed$availabile

  if(!is.null(pkgs_avail) && any(!pkgs_avail)){
    pkgs_not_installed <- py_pkgs[!pkgs_avail] %>% paste(collapse = ", ")
    errors <- paste("{.code", pkgs_installed$error[!pkgs_avail], "}")
    alert_fn <- ifelse(isTRUE(warn), cli::cli_warn, cli::cli_abort)

    cli::cli_div(theme = list(span.emph = list(color = "green"), span.code = list(color = "red")))
    alert_fn(
      c(
        "x" = glue::glue(
          "The following python modules are not available: {.emph {{pkgs_not_installed}}}",
          .open = "{{", .close = "}}"),
        "i" = "Reason(s):",
        " " = "",
        errors
      )
    )
  }

  return(invisible(all(pkgs_avail)))
}


#' Modified version of [reticulate::py_module_available()]
#'
#' @param module The name of the Python module.
#' @param path The path from which the module should be imported.
#'
#' @keywords internal
py_module_available <- function(module, path){
  mod_avail <- tryCatch({
    if(is.na(path)){
      reticulate::import(module)
      path <- Sys.getenv("RETICULATE_PYTHON")
    }else{
      reticulate::import_from_path(module, path = path)
    }
    tibble::tibble(
      module = module, availabile = TRUE, path = path, error = NA_character_
    )
  }, error = function(err){
    tibble::tibble(
      module = module, availabile = FALSE, path = path, error = err$msg
    )
  })
}

get_py_path <- function(){
  py_path <- Sys.getenv("RETICULATE_PYTHON_ENV", unset = NA)
  if(is.na(py_path)){
    py_path <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
  }
  return(py_path)
}
