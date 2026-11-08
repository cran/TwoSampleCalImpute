test_that("tsci_clean_data preserves data when no cleaning is requested", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, NA, 30),
    z = c(5, 6, 7)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_clean_data(data_object)

  expect_s3_class(result, "tsci_data")
  expect_equal(result$data, dat)
})

test_that("tsci_clean_data removes completely empty rows", {
  dat <- data.frame(
    id = c(1, 2, NA, 4),
    y = c(10, 20, NA, 40),
    z = c(5, 6, NA, 8)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_clean_data(
    data_object,
    remove_empty_rows = TRUE
  )

  expect_equal(nrow(result$data), 3)
  expect_equal(result$data$id, c(1, 2, 4))
})

test_that("tsci_clean_data removes completely empty columns", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30),
    empty = c(NA, NA, NA)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_clean_data(
    data_object,
    remove_empty_columns = TRUE
  )

  expect_equal(names(result$data), c("id", "y"))
  expect_equal(ncol(result$data), 2)
})

test_that("tsci_clean_data can remove empty rows and columns", {
  dat <- data.frame(
    id = c(1, 2, NA),
    y = c(10, 20, NA),
    empty = c(NA, NA, NA)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_clean_data(
    data_object,
    remove_empty_rows = TRUE,
    remove_empty_columns = TRUE
  )

  expect_equal(nrow(result$data), 2)
  expect_equal(ncol(result$data), 2)
  expect_equal(names(result$data), c("id", "y"))
})

test_that("tsci_clean_data rejects invalid objects", {
  expect_error(
    tsci_clean_data(data.frame(id = 1:3)),
    "must be a.*tsci_data"
  )
})

test_that("tsci_clean_data validates remove_empty_rows", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_clean_data(
      data_object,
      remove_empty_rows = NA
    ),
    "single TRUE or FALSE"
  )
})

test_that("tsci_clean_data validates remove_empty_columns", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_clean_data(
      data_object,
      remove_empty_columns = NA
    ),
    "single TRUE or FALSE"
  )
})
