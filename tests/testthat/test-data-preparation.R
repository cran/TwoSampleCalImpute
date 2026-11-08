test_that("tsci_prepare_data returns a tsci_data object", {
  test_data <- data.frame(
    id = c(1, 2, NA, 4),
    y = c(10, 20, NA, 40),
    z = c(5, 6, NA, 8)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_prepare_data(
    data = test_object,
    remove_empty_rows = TRUE,
    remove_empty_columns = FALSE
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "primary")
  expect_equal(nrow(result$data), 3L)
  expect_equal(ncol(result$data), 3L)
})

test_that("tsci_prepare_data removes empty columns when requested", {
  test_data <- data.frame(
    id = c(1, 2, 3),
    y = c(10, 20, 30),
    empty = c(NA, NA, NA)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_prepare_data(
    data = test_object,
    remove_empty_rows = FALSE,
    remove_empty_columns = TRUE
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(nrow(result$data), 3L)
  expect_equal(ncol(result$data), 2L)
  expect_false("empty" %in% names(result$data))
})
