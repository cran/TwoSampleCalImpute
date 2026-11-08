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

test_that("tsci_design_summary summarises a survey design", {
  result <- tsci_design_summary(
    design = design_test_result
  )

  expect_s3_class(result, "data.frame")
  expect_identical(result$sample_type, "primary")
  expect_identical(result$observations, 4L)
  expect_identical(result$variables, 6L)
  expect_identical(result$id, "id")
  expect_identical(result$weights, TRUE)
  expect_identical(result$strata, "strata")
  expect_identical(result$fpc, TRUE)
})

test_that("tsci_design_summary rejects an invalid design object", {
  expect_error(
    tsci_design_summary(data.frame(x = 1:3)),
    "design.*tsci_design"
  )
})
