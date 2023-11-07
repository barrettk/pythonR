
testthat::test_that("python setup", {
  skip_if_no_python()

  py_add <- get_py_example("py_add")
  expect_equal(py_add(3,4), 7)
})
