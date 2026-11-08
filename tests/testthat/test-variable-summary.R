test_that("tsci_variable_summary summarizes all variables", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4),
    age = c(20, 30, NA, 40),
    sex = c("M", "F", "M", "F"),
    score = c(10, 20, 30, 40)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_variable_summary(test_object)

  expect_equal(nrow(result), 4L)
  expect_equal(result$variable, c("id", "age", "sex", "score"))
  expect_equal(result$n, c(4, 4, 4, 4))
  expect_equal(result$missing, c(0, 1, 0, 0))
  expect_equal(result$unique, c(4, 3, 2, 4))
})

test_that("tsci_variable_summary summarizes selected variables", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4),
    age = c(20, 30, NA, 40),
    sex = c("M", "F", "M", "F")
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_variable_summary(
    data = test_object,
    variables = c("age", "sex")
  )

  expect_equal(result$variable, c("age", "sex"))
  expect_equal(result$missing, c(1, 0))
  expect_equal(result$unique, c(3, 2))
})

test_that("tsci_variable_summary rejects unknown variables", {
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
    tsci_variable_summary(
      data = test_object,
      variables = "unknown"
    ),
    "Variables not found"
  )
})
