test_that("tsci_subset_data returns the requested observations", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4),
    y = c(10, 20, 30, 40),
    z = c(5, 6, 7, 8)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_subset_data(
    data = test_object,
    subset = test_object$data$y >= 30
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "primary")
  expect_equal(nrow(result$data), 2L)
  expect_equal(result$data$id, c(3, 4))
  expect_equal(result$data$y, c(30, 40))
  expect_equal(result$data$z, c(7, 8))
})

test_that("tsci_subset_data rejects an invalid subset", {
  test_data <- data.frame(
    id = c(1, 2, 3),
    y = c(10, 20, 30)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_subset_data(
      data = test_object,
      subset = c(TRUE, FALSE)
    ),
    "logical vector of length equal to the number of observations"
  )

  expect_error(
    tsci_subset_data(
      data = test_object,
      subset = c(TRUE, NA, TRUE)
    ),
    "contain no NA values"
  )
})
