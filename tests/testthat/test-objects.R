test_that(".build_data_object creates a valid primary object", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  result <- TwoSampleCalImpute:::.build_data_object(
    data = dat, sample_type = "primary", id = "id"
  )
  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "primary")
  expect_equal(result$id, "id")
  expect_equal(nrow(result$data), 3)
  expect_equal(result$variables, c("id", "y"))
})

test_that(".build_data_object creates a valid auxiliary object", {
  dat <- data.frame(id = 1:3, x = c(1, 2, 3))
  result <- TwoSampleCalImpute:::.build_data_object(
    data = dat, sample_type = "auxiliary", id = "id"
  )
  expect_s3_class(result, "tsci_data")
  expect_equal(result$sample_type, "auxiliary")
  expect_equal(result$id, "id")
})

test_that(".build_data_object rejects invalid sample type", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  expect_error(
    TwoSampleCalImpute:::.build_data_object(
      data = dat, sample_type = "invalid"
    )
  )
})

test_that(".build_data_object rejects missing identifier", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  expect_error(
    TwoSampleCalImpute:::.build_data_object(
      data = dat, sample_type = "primary", id = "missing_id"
    ),
    "missing_id"
  )
})

test_that(".build_design_object creates a valid design object", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, 20, 30),
    strata = c("A", "A", "B")
  )
  data_object <- TwoSampleCalImpute:::.build_data_object(
    data = dat, sample_type = "primary", id = "id"
  )
  result <- TwoSampleCalImpute:::.build_design_object(
    data_object = data_object,
    weights = c(1, 2, 3),
    id = "id", strata = "strata"
  )
  expect_s3_class(result, "tsci_design")
  expect_equal(result$weights, c(1, 2, 3))
  expect_equal(result$id, "id")
  expect_equal(result$strata, "strata")
})

test_that(".build_design_object accepts an FPC", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  data_object <- TwoSampleCalImpute:::.build_data_object(
    data = dat, sample_type = "primary", id = "id"
  )
  result <- TwoSampleCalImpute:::.build_design_object(
    data_object = data_object,
    weights = c(1, 2, 3),
    fpc = c(100, 100, 100)
  )
  expect_equal(result$fpc, c(100, 100, 100))
})

test_that(".build_design_object rejects invalid weights", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  data_object <- TwoSampleCalImpute:::.build_data_object(
    data = dat, sample_type = "primary", id = "id"
  )
  expect_error(
    TwoSampleCalImpute:::.build_design_object(
      data_object = data_object, weights = c(1, 0, 3)
    ),
    "strictly positive"
  )
})

test_that(".build_design_object rejects invalid FPC", {
  dat <- data.frame(id = 1:3, y = c(10, 20, 30))
  data_object <- TwoSampleCalImpute:::.build_data_object(
    data = dat, sample_type = "primary", id = "id"
  )
  expect_error(
    TwoSampleCalImpute:::.build_design_object(
      data_object = data_object, fpc = c(100, NA, 100)
    ),
    "missing or non-finite"
  )
})
