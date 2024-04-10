
testthat::test_that("importing python packages - unit", {
  skip_if_no_conda()
  expect_false(exists("pandas"))
  import_py_pkgs("pandas")
  on.exit(rm(pandas))
  expect_true(exists("pandas"))
  expect_true(inherits(pandas, "python.builtin.module"))
})

testthat::test_that("importing main python packages - unit", {
  skip_if_no_conda()
  expect_false(exists("main"))
  import_main_py()
  on.exit(rm(main))
  expect_true(exists("main"))
  expect_true(inherits(main, "python.builtin.module"))
})

testthat::test_that("importing python packages - integration", {
  skip_if_no_conda()

  py_array <- function(){
    # Check that required packages for this function have been installed
    required_py_pkgs <- c('pandas', 'scipy', 'numpy')
    checkmate::assert_true(check_py_pkgs_installed(required_py_pkgs))

    # import main python modules
    import_main_py()
    # import required python packages
    import_py_pkgs(py_pkgs = required_py_pkgs)

    # This object (`py_objs`) will keep track of everything in your python environment
    # even after it's called
    py_objs <- reticulate::py_run_string("data={'col1': [1, 2], 'col2': [3, 4]}")

    # R call that uses the `pandas` python library
    data_df <- pandas$DataFrame(data=py_objs$data)

    return(data_df)
  }

  data <- py_array()
  expect_true(inherits(data, "data.frame"))
  expect_equal(dim(data), c(2, 2))
})


