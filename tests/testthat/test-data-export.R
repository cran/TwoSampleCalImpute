test_that("tsci_export_data exports a tsci_data object", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  output_file <- tempfile(fileext = ".csv")
  on.exit(unlink(output_file), add = TRUE)

  result <- tsci_export_data(
    data = data_object,
    file = output_file
  )

  expect_true(file.exists(output_file))
  expect_s3_class(result, "tsci_data")

  imported <- utils::read.csv(output_file)
  expect_equal(imported, dat)
})

test_that("tsci_export_data rejects invalid objects", {
  output_file <- tempfile(fileext = ".csv")
  on.exit(unlink(output_file), add = TRUE)

  expect_error(
    tsci_export_data(
      data = data.frame(id = 1:3),
      file = output_file
    ),
    "must be a.*tsci_data"
  )
})

test_that("tsci_export_data rejects invalid file paths", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  expect_error(
    tsci_export_data(
      data = data_object,
      file = NA_character_
    ),
    "single character path"
  )
})

test_that("tsci_export_data rejects unsupported file types", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  output_file <- tempfile(fileext = ".txt")
  on.exit(unlink(output_file), add = TRUE)

  expect_error(
    tsci_export_data(
      data = data_object,
      file = output_file
    ),
    "Only CSV files are currently supported"
  )
})

test_that("tsci_export_data validates row_names", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  data_object <- tsci_import_data(
    data = dat,
    sample_type = "primary",
    id = "id"
  )

  output_file <- tempfile(fileext = ".csv")
  on.exit(unlink(output_file), add = TRUE)

  expect_error(
    tsci_export_data(
      data = data_object,
      file = output_file,
      row_names = NA
    ),
    "single TRUE or FALSE"
  )
})
