test_that("tsci_recode_variables recodes selected variables", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4),
    sex = c("M", "F", "M", "F"),
    age = c(20, 30, 40, 50)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_recode_variables(
    data = test_object,
    recodes = list(
      sex = function(x) ifelse(x == "M", 1, 2),
      age = function(x) ifelse(x >= 40, "40+", "<40")
    )
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$data$id, c(1, 2, 3, 4))
  expect_equal(result$data$sex, c(1, 2, 1, 2))
  expect_equal(result$data$age, c("<40", "<40", "40+", "40+"))
})

test_that("tsci_recode_variables rejects an unknown variable", {
  test_data <- data.frame(
    id = c(1, 2),
    y = c(10, 20)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_recode_variables(
      data = test_object,
      recodes = list(unknown = function(x) x)
    ),
    "Variables not found"
  )
})

test_that("tsci_recode_variables rejects non-function recodes", {
  test_data <- data.frame(
    id = c(1, 2),
    y = c(10, 20)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_recode_variables(
      data = test_object,
      recodes = list(y = "not a function")
    ),
    "Every element of 'recodes' must be a function"
  )
})
