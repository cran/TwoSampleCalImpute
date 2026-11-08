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

test_that("tsci_weights extracts survey weights", {
  result <- tsci_weights(design = design_test_result)

  expect_type(result, "double")
  expect_length(result, 4L)
  expect_equal(result, c(2, 2, 4, 4))
  expect_true(all(is.finite(result)))
  expect_true(all(result > 0))
})

test_that("tsci_weights rejects an invalid design object", {
  expect_error(
    tsci_weights(data.frame(x = 1:3)),
    "design.*tsci_design"
  )
})
