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

design_test_result <- tsci_create_design(
  data = design_test_object,
  weights = "weight",
  strata = "strata",
  clusters = "cluster",
  fpc = "fpc"
)

test_that("tsci_strata extracts the stratification variable", {
  result <- tsci_strata(design = design_test_result)

  expect_type(result, "character")
  expect_length(result, 1L)
  expect_identical(result, "strata")
  expect_true(result %in% names(design_test_result$data$data))
})

test_that("tsci_strata rejects an invalid design object", {
  expect_error(
    tsci_strata(data.frame(x = 1:3)),
    "design.*tsci_design"
  )
})

test_that("tsci_strata returns NULL when no strata are specified", {
  no_strata_design <- tsci_create_design(
    data = design_test_object,
    weights = "weight"
  )

  expect_null(tsci_strata(no_strata_design))
})
