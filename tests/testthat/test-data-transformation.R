test_that("tsci_transform_data transforms selected variables", {
  test_data <- data.frame(
    id = c(1, 2, 3),
    y = c(2, 4, 6),
    z = c(10, 20, 30)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_transform_data(
    data = test_object,
    variables = "y",
    fun = log
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$data$id, c(1, 2, 3))
  expect_equal(result$data$y, log(c(2, 4, 6)))
  expect_equal(result$data$z, c(10, 20, 30))
})

test_that("tsci_transform_data transforms multiple variables", {
  test_data <- data.frame(
    id = c(1, 2, 3),
    y = c(2, 4, 6),
    z = c(10, 20, 30)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_transform_data(
    data = test_object,
    variables = c("y", "z"),
    fun = log
  )

  expect_equal(result$data$y, log(c(2, 4, 6)))
  expect_equal(result$data$z, log(c(10, 20, 30)))
})

test_that("tsci_transform_data rejects unknown variables", {
  test_data <- data.frame(
    id = c(1, 2, 3),
    y = c(2, 4, 6)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_transform_data(
      data = test_object,
      variables = "unknown",
      fun = log
    ),
    "Variables not found"
  )
})
