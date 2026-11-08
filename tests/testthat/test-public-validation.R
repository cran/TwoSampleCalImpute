test_that("tsci_validate_data validates a valid data frame", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, NA, 30)
  )

  result <- tsci_validate_data(dat)

  expect_s3_class(result, "tsci_validation")
  expect_true(result$valid)
  expect_equal(result$n, 3)
  expect_equal(result$p, 2)
  expect_equal(result$variables, c("id", "y"))
})

test_that("tsci_validate_data reports missing values", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, NA, 30)
  )

  result <- tsci_validate_data(dat)

  expect_equal(
    result$missing$missing_count[result$missing$variable == "y"],
    1
  )

  expect_equal(
    result$missing$missing_percent[result$missing$variable == "y"],
    100 / 3
  )
})

test_that("tsci_validate_data detects duplicate records", {
  dat <- data.frame(
    id = c(1, 2, 2),
    y = c(10, 20, 20)
  )

  result <- tsci_validate_data(dat)

  expect_true(result$duplicates$has_duplicates)
  expect_equal(result$duplicates$n_duplicates, 1)
})

test_that("tsci_validate_data validates selected variables", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  result <- tsci_validate_data(
    dat,
    variables = c("id", "y")
  )

  expect_true(result$valid)
})

test_that("tsci_validate_data rejects missing variables", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  expect_error(
    tsci_validate_data(
      dat,
      variables = c("id", "weight")
    ),
    "weight"
  )
})

test_that("tsci_validate_data validates positive weights", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  result <- tsci_validate_data(
    dat,
    weights = c(1, 2, 3)
  )

  expect_true(result$valid)
})

test_that("tsci_validate_data rejects invalid weights", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30)
  )

  expect_error(
    tsci_validate_data(
      dat,
      weights = c(1, 0, 3)
    ),
    "strictly positive"
  )
})

test_that("tsci_validate_data uses a supplied key", {
  dat <- data.frame(
    id = c(1, 2, 2),
    y = c(10, 20, 30)
  )

  result <- tsci_validate_data(
    dat,
    key = "id"
  )

  expect_true(result$duplicates$has_duplicates)
  expect_equal(result$duplicates$n_duplicates, 1)
})

test_that("tsci_validate_data rejects non-data-frame input", {
  expect_error(
    tsci_validate_data(c(1, 2, 3)),
    "must be a data.frame"
  )
})
