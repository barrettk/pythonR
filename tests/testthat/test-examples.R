
testthat::test_that("python example - py_add", {
  skip_if_no_python()

  py_add <- get_py_example("py_add")
  expect_equal(py_add(3,4), 7)
})

testthat::test_that("python example - py_check_path", {
  skip_if_no_python()

  py_check_path <- get_py_example("py_check_path")
  expect_true(py_check_path())
})

testthat::test_that("python example - py_array", {
  skip_if_no_python()
  skip_if_no_pandas()
  skip_if_no_scipy()
  skip_if_no_numpy()

  py_array <- get_py_example("py_array")
  expect_true(is.list(py_array()))
})

