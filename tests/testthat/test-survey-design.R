test_data <- data.frame(
  id = c(1, 2, 3, 4),
  y = c(10, 20, 30, 40),
  weight = c(2, 2, 4, 4),
  strata = c("A", "A", "B", "B"),
  cluster = c(1, 1, 2, 2),
  fpc = c(100, 100, 100, 100)
)

design_test_object <- tsci_import_data(
  data = test_data,
  sample_type = "primary",
  id = "id"
)

test_that("tsci_create_design creates a valid design object", {
  result <- tsci_create_design(
    data = design_test_object,
    weights = "weight",
    strata = "strata",
    fpc = "fpc"
  )

  expect_s3_class(result, "tsci_design")
  expect_identical(result$id, "id")
  expect_identical(result$strata, "strata")
  expect_equal(result$weights, c(2, 2, 4, 4))
  expect_equal(result$fpc, c(100, 100, 100, 100))
})

test_that("tsci_create_design rejects invalid weight variable", {
  expect_error(
    tsci_create_design(
      data = design_test_object,
      weights = "missing_weight"
    ),
    "Weight variable"
  )
})

test_that("tsci_create_design rejects invalid strata variable", {
  expect_error(
    tsci_create_design(
      data = design_test_object,
      weights = "weight",
      strata = "missing_strata"
    ),
    "Strata variable"
  )
})

test_that("tsci_create_design supports clusters", {
  result <- tsci_create_design(
    data = design_test_object,
    weights = "weight",
    strata = "strata",
    clusters = "cluster",
    fpc = "fpc"
  )

  expect_s3_class(result, "tsci_design")
  expect_identical(result$clusters, "cluster")
})
