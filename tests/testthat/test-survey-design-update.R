test_data <- data.frame(
  id = c(1, 2, 3, 4),
  y = c(10, 20, 30, 40),
  weight = c(2, 2, 4, 4),
  strata = c("A", "A", "B", "B"),
  cluster = c(1, 1, 2, 2),
  fpc = c(100, 100, 100, 100)
)

data_object <- tsci_import_data(
  data = test_data,
  sample_type = "primary",
  id = "id"
)

design_test_result <- tsci_create_design(
  data = data_object,
  weights = "weight",
  strata = "strata",
  clusters = "cluster",
  fpc = "fpc"
)

test_that("tsci_update_design updates a survey design object", {
  result <- tsci_update_design(
    design = design_test_result,
    weights = "weight",
    strata = "strata",
    clusters = "cluster",
    fpc = "fpc"
  )

  expect_s3_class(result, "tsci_design")
  expect_identical(result$id, "id")
  expect_identical(result$strata, "strata")
  expect_identical(result$clusters, "cluster")
  expect_equal(result$weights, c(2, 2, 4, 4))
  expect_equal(result$fpc, c(100, 100, 100, 100))
})
