test_that("tsci_merge_data merges two TSCI data objects", {
  x <- data.frame(
    id = c(1, 2, 3),
    y = c(10, 20, 30)
  )

  y <- data.frame(
    id = c(1, 2, 3),
    z = c(100, 200, 300)
  )

  x_object <- tsci_import_data(
    data = x,
    sample_type = "primary",
    id = "id"
  )

  y_object <- tsci_import_data(
    data = y,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_merge_data(
    x = x_object,
    y = y_object,
    by = "id"
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "primary")
  expect_equal(nrow(result$data), 3L)
  expect_equal(ncol(result$data), 3L)
  expect_equal(result$data$id, c(1, 2, 3))
  expect_equal(result$data$y, c(10, 20, 30))
  expect_equal(result$data$z, c(100, 200, 300))
})

test_that("tsci_merge_data retains unmatched observations when all is TRUE", {
  x <- data.frame(
    id = c(1, 2),
    y = c(10, 20)
  )

  y <- data.frame(
    id = c(2, 3),
    z = c(200, 300)
  )

  x_object <- tsci_import_data(
    data = x,
    sample_type = "primary",
    id = "id"
  )

  y_object <- tsci_import_data(
    data = y,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_merge_data(
    x = x_object,
    y = y_object,
    by = "id",
    all = TRUE
  )

  expect_equal(nrow(result$data), 3L)
  expect_true(all(c(1, 2, 3) %in% result$data$id))
})

test_that("tsci_merge_data rejects an unknown merge key", {
  x <- data.frame(id = 1:2, y = c(10, 20))
  y <- data.frame(id = 1:2, z = c(100, 200))

  x_object <- tsci_import_data(
    data = x,
    sample_type = "primary",
    id = "id"
  )

  y_object <- tsci_import_data(
    data = y,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_merge_data(
      x = x_object,
      y = y_object,
      by = "unknown"
    ),
    "Merge key"
  )
})
