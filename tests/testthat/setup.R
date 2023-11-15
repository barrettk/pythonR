Sys.unsetenv("RETICULATE_PYTHON")

# Set environment for all tests
# py_env <- withr::with_envvar(new = c("RETICULATE_PYTHON" = NA), {
#   setup_py_env(py_pkgs = NULL) %>% suppressMessages()
# })

py_env <- setup_py_env(py_pkgs = "pandas") %>% suppressMessages() %>%
  suppressWarnings()

ENV_NAME <- py_env$env_name
