
testthat::test_that("python setup - setup_py_env: conda", {
  skip_if_no_conda()

  py_env <- setup_py_env(
    py_pkgs = c("pandas", "numpy"),
    env_name = ENV_NAME
  ) %>% suppressWarnings()

  testthat::expect_true(is.list(py_env))
  expect_equal(
    names(py_env),
    c("env_name", "env_path", "conda_path", "config", "installed_pkgs", "avail_conda_envirs")
  )
  # Check that python modules are installed
  expect_true(check_py_pkgs_installed(c("pandas", "numpy")))
})


testthat::test_that("python setup - install_py_pkgs", {
  skip_if_no_conda()

  install_py_pkgs("scipy", env_name = ENV_NAME)

  # Check that python modules are installed
  expect_true(check_py_pkgs_installed("scipy"))
})
