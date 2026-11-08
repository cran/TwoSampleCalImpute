test_that("tsci_missing_summary summarizes all variables", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4, 5),
    age = c(20, 30, NA, 40, NA),
    sex = c("M", "F", "M", NA, "F"),
    score = c(10, NA, 30, 40, 50)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_missing_summary(test_object)

  expect_equal(nrow(result), 4L)
  expect_equal(result$variable, c("id", "age", "sex", "score"))
  expect_equal(result$n, c(5, 5, 5, 5))
  expect_equal(result$missing, c(0, 2, 1, 1))
  expect_equal(result$observed, c(5, 3, 4, 4))
  expect_equal(result$missing_percent, c(0, 40, 20, 20))
})

test_that("tsci_missing_summary summarizes selected variables", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4, 5),
    age = c(20, 30, NA, 40, NA),
    score = c(10, NA, 30, 40, 50)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_missing_summary(
    data = test_object,
    variables = c("age", "score")
  )

  expect_equal(result$variable, c("age", "score"))
  expect_equal(result$missing, c(2, 1))
  expect_equal(result$observed, c(3, 4))
  expect_equal(result$missing_percent, c(40, 20))
})

test_that("tsci_missing_summary rejects unknown variables", {
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
    tsci_missing_summary(
      data = test_object,
      variables = "unknown"
    ),
    "Variables not found"
  )
})
