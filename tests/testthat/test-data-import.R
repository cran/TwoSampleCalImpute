test_that("tsci_import_data accepts a data frame", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  result <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "primary")
  expect_equal(result$id, "id")
  expect_equal(result$data, dat)
})

test_that("tsci_import_data accepts an auxiliary data frame", {
  dat <- data.frame(
    id = 1:3,
    x = c(1, 2, 3)
  )

  result <- tsci_import_data(
    data = dat,
    sample_type = "auxiliary",
    id = "id"
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "auxiliary")
})

test_that("tsci_import_data imports a CSV file", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  csv_file <- tempfile(fileext = ".csv")
  utils::write.csv(dat, csv_file, row.names = FALSE)
  on.exit(unlink(csv_file), add = TRUE)

  result <- tsci_import_data(
    data = csv_file,
    sample_type = "primary",
    id = "id"
  )

  expect_s3_class(result, "tsci_data")
  expect_equal(result$data, dat)
})

test_that("tsci_import_data rejects a non-existent file", {
  expect_error(
    tsci_import_data("file_that_does_not_exist.csv"),
    "File does not exist"
  )
})

test_that("tsci_import_data rejects unsupported file types", {
  temp_file <- tempfile(fileext = ".txt")
  writeLines("test", temp_file)
  on.exit(unlink(temp_file), add = TRUE)

  expect_error(
    tsci_import_data(temp_file),
    "Only CSV files are currently supported"
  )
})

test_that("tsci_import_data rejects invalid input", {
  expect_error(
    tsci_import_data(1:10),
    "must be a data.frame or a path to a CSV file"
  )
})

test_that("tsci_import_data rejects an invalid identifier", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  expect_error(
    tsci_import_data(
      data = dat,
      sample_type = "primary",
      id = "missing_id"
    ),
    "missing_id"
  )
})
