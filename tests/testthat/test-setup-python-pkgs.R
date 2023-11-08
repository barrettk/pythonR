
testthat::test_that("python setup - conda", {
  skip_if_no_conda()
  # Sys.unsetenv("RETICULATE_PYTHON")
  py_env <- setup_py_env(
    py_pkgs = c("pandas", "numpy", "scipy")
  )

  testthat::expect_true(is.list(py_env))
  expect_equal(
    names(py_env),
    c("env_name", "env_path", "config", "installed_pkgs", "avail_conda_envirs")
  )
})
