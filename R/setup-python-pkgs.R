
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
#' @param conda_env path of the conda environment to use if `py_env = "conda"`.
#'        Run `reticulate::conda_list()` to see a list of available conda environments.
#'        The `conda_env` should match the `python` element of that list if you are loading
#'        *an existing* conda environment. If `conda_env` is not found in this list, a new
#'        one will be created matching the name of `env_name`.
#' @param conda_path The path to a conda executable. If `NULL`, will default to the first one found.
#'        See `get_conda_paths()` for details.
#' @param update Logical (`TRUE`/`FALSE`). If `TRUE`, update packages that are
#'        already installed. Otherwise skip their installation.
#' @param pip Logical (`TRUE`/`FALSE`). Use pip for package installation? This
#'        is only relevant when Conda environments are used, as otherwise packages
#'        will be installed from the Conda repositories.
#' @param required Logical (`TRUE`/`FALSE`). Is the requested copy of Python required?
#'        If TRUE, an error will be emitted if the requested copy of Python does not exist.
#'        If FALSE, the request is taken as a hint only, and scanning for other versions
#'        will still proceed. See [reticulate::use_condaenv()] for more details.
#'
#' @details
#'
#'
#' An environment will be required for each new R session.
#'
#' For a virtual environment, the user would be installing these packages *every time*
#' they want to run python on a clean R session. It is a smaller package size that
#' a conda environment.
#'
#' For a conda environment, users only need to install the required packages once. This
#' may be less ideal if many python modules are required, or you are developing an R package.
#'
#' @note
#' You must restart your R session if you want to alternate between both environment types
#'
#'
#' @examples
#' \dontrun{
#'
#' ## With a conda environment (the default) ##
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"))
#' py_env
#'
#' # Specify a specific conda environment and conda installation
#' conda_envs <- reticulate::conda_list()
#' conda_paths <- pythonR::get_conda_paths()
#'
#' py_env <- setup_py_env(
#'   py_pkgs = c("pandas", "numpy", "scipy", "tiktoken"),
#'   conda_path = conda_paths[1],
#'   conda_env = conda_envs$python[1],
#'   required = TRUE,
#'   update = TRUE,
#'   pip = TRUE
#' )
#'
#'
#' # Installing a package after setup (works with both environment types)
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy"), py_env = "conda")
#' install_py_pkgs(py_pkgs = c('scipy'), env_name = py_env$env_name)
#'
#' # Check that installed modules can be imported
#' check_py_pkgs_installed(c("pandas", "numpy", "scipy"))
#'
#'
#' ## With a virtual environment ##
#' py_env <- setup_py_env(py_pkgs = c("pandas", "numpy", "scipy"), py_env = "virtual")
#' py_env
#'
#' # shutdown virtual environment
#' shutdown_virtual_env(py_env$env_name)
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
    conda_env = get_conda_envs(),
    conda_path = NULL,
    update = FALSE,
    pip = FALSE,
    required = TRUE
){

  # Make sure python is installed
  checkmate::assert_true(python_is_installed())

  # Set conda path
  if(is.null(conda_path)){
    conda_path <- get_conda_paths()[1]
  }

  py_env <- match.arg(py_env)

  if(py_env == "conda"){
    conda_envir_lst <- reticulate::conda_list(conda = conda_path)
    conda_envirs <- conda_envir_lst %>% dplyr::pull(.data$python)

    # choose first environment if multiple are provided via `get_conda_envs`
    conda_env <- conda_env[1]

    # pull previous conda environment if it exists, otherwise create new one
    if(!is.null(conda_env) && conda_env %in% conda_envirs){
      # loads a local conda library
      conda_env_df <- conda_envir_lst %>% dplyr::filter(.data$python == conda_env)
      env_path <- conda_env_df %>% dplyr::pull(.data$python)
      # overwrite env_name with conda name
      env_name <- conda_env_df %>% dplyr::pull(.data$name)
    }else{
      # Creates a local conda library
      env_path <- tryCatch(reticulate::conda_create(env_name), error = identity)
      # make sure the environment was created
      env_exists <- !inherits(env_path, "error") && fs::file_exists(env_path) &&
        reticulate::condaenv_exists(env_path)
      if(isFALSE(env_exists)){
        cli::cli_abort(glue::glue("env {env_name} could not be created:\n\n {env_path$message}"))
      }else{
        message(glue::glue("Created new conda environment ({env_name}) at: {env_path}"))
      }
    }

    # Configure conda environment
    Sys.setenv(RETICULATE_PYTHON_ENV = env_path)
    reticulate::use_condaenv(condaenv = env_path, conda = conda_path, required = required)
    message(glue::glue("Loading conda environment {env_name}. Path: {env_path}"))
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
    message(glue::glue("A virtual environment has been loaded at: {env_path}"))
  }

  # configure python in environment
  # suppress any warnings (will be returned via `use_condaenv` or `use_virtualenv`)
  config <- reticulate::py_config() %>% suppressWarnings()

  # install python packages
  installed_pkgs <- install_py_pkgs(
    py_pkgs,
    env_name = env_name,
    py_env = py_env,
    conda_path = conda_path,
    update = update,
    pip = pip,
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


#' @describeIn setup_py_env Install python packages to an environment
#'
#'
#' @details
#' see `?reticulate::py_install` for more details
#'
#' @seealso [import_py_pkgs]
#'
#' @export
install_py_pkgs <- function(
    py_pkgs = c("scipy", "pandas"),
    py_env = c("conda", "virtual"),
    conda_path = NULL,
    env_name = PYTHON_R_ENV,
    pip = FALSE,
    update = FALSE,
    python_version = NULL
){

  if(is.null(conda_path)){
    conda_path <- get_conda_paths()[1]
  }

  py_env <- match.arg(py_env)
  virtual_env <- (py_env == "virtual")

  # Check if already installed
  if(isFALSE(update)){
    installed_pkgs <- reticulate::py_list_packages(env_name) %>%
      tibble::as_tibble()

    py_pkgs_installed <- py_pkgs[py_pkgs %in% installed_pkgs$package] %>%
      paste(collapse = ", ")
    if(!rlang::is_empty(py_pkgs_installed) && nzchar(py_pkgs_installed)){
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
    reticulate::py_install(
      to_install,
      envname = env_name,
      method = ifelse(isTRUE(virtual_env), "virtualenv", "conda"),
      conda = ifelse(isTRUE(virtual_env), "auto", conda_path),
      python_version = python_version,
      pip = ifelse(isTRUE(virtual_env), FALSE, pip),
      ignore_installed = isTRUE(update) && isFALSE(virtual_env)
    )
    # purrr::walk(to_install, function(pkg){
    #   args <- list(
    #     pkg,
    #     envname = env_name,
    #     method = ifelse(isTRUE(virtual_env), "virtualenv", "conda"),
    #     conda = ifelse(isTRUE(virtual_env), "auto", conda_path),
    #     python_version = python_version,
    #     pip = pip,
    #     ignore_installed = isTRUE(update) && isFALSE(virtual_env)
    #   )
    #   do.call(reticulate::py_install, args)
    # })
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

  pkgs_installed <- purrr::map_dfr(py_pkgs, function(pkg){
    py_module_available(pkg, path)
  })
  pkgs_avail <- pkgs_installed$availabile

  # import each package
  purrr::walk2(py_pkgs, pkgs_avail, function(pkg_name, pkg_avail){
    if(isTRUE(pkg_avail)){
      pkg <- if(is.na(path)){
        reticulate::import(pkg_name, delay_load = delay_load)
      }else{
        reticulate::import_from_path(pkg_name, path = path, delay_load = delay_load)
      }
      assign(pkg_name, pkg, envir = envir)
    }else{
      error = pkgs_installed$error[pkgs_installed$module == pkg_name]

      cli::cli_div(theme = list(span.emph = list(color = "green"), span.code = list(color = "red")))
      cli::cli_warn(
        c(
          "x" = glue::glue(
            "Could not import {.emph {{pkg_name}}}",
            .open = "{{", .close = "}}"),
          "i" = glue::glue("Reason(s): {error}")
        )
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
    # Sys.unsetenv("RETICULATE_PYTHON_ENV")
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
    errors <- paste0("{.code ", pkgs_installed$error[!pkgs_avail], "}")
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
      path <- Sys.getenv("RETICULATE_PYTHON_ENV")
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

  return(mod_avail)
}

get_py_path <- function(){
  py_path <- Sys.getenv("RETICULATE_PYTHON_ENV", unset = NA)
  if(is.na(py_path)){
    py_path <- Sys.getenv("RETICULATE_PYTHON", unset = NA)
  }
  return(py_path)
}

get_conda_envs <- function(){
  conda_lst <- reticulate::conda_list()
  sort(conda_lst$python)
}



#' @describeIn setup_py_env Search for conda installations
#' @details
#' Inspired from `reticulate:::find_conda()`, though this function
#' will return all paths instead of the first one found in a hierarchy of priorities
#'
#' @export
get_conda_paths <- function(){
  conda_paths <- c()
  # allow specification of conda executable
  conda <- getOption("reticulate.conda_binary")
  if (!is.null(conda)) conda_paths <- c(conda_paths, conda)

  conda <- Sys.getenv("RETICULATE_CONDA", unset = NA)
  if (!is.na(conda)) conda_paths <- c(conda_paths, conda)

  # if miniconda is installed, use it
  conda <- r_conda_path()
  if (file.exists(conda)){
    conda_paths <- c(conda_paths, conda)
  }

  # if there is a conda executable on the PATH, use it
  conda <- Sys.which("conda")
  if (nzchar(conda)) conda_paths <- c(conda_paths, unname(conda))

  # otherwise, search common locations for conda
  prefixes <- c("~/opt/", "~/", "/opt/", "/")
  names <- c("anaconda", "miniconda", "miniforge")
  versions <- c("", "2", "3", "4")
  combos <- expand.grid(versions, names, prefixes, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  combos <- combos[rev(seq_along(combos))]
  conda_locations <- unlist(.mapply(paste0, combos, NULL))

  # find the potential conda binary path in each case
  conda_locations <- if (xfun::is_windows()) {
    paste0(conda_locations, "/condabin/conda.bat")
  } else {
    paste0(conda_locations, "/bin/conda")
  }

  # look for caskroom version
  caskroom_conda <- "/opt/homebrew/Caskroom/miniforge/base/bin/conda"
  if (file.exists(caskroom_conda))
    conda_locations <- c(conda_locations, caskroom_conda)

  # ensure we expand tilde prefixes
  conda_locations <- path.expand(conda_locations)

  # append paths
  conda_paths <- unique(c(conda_paths, conda_locations))

  # keep only conda locations that exist
  conda_paths <- conda_paths[file.exists(conda_paths)]

  # explicitly return NULL when no conda found
  if (!length(conda_paths)) return(NULL)

  return(conda_paths)
}

r_conda_path <- function(path = reticulate::miniconda_path()){
  exe <- if (xfun::is_windows()){
    "condabin/conda.bat"
  }else{
    "bin/conda"
  }
  file.path(path, exe)
}
