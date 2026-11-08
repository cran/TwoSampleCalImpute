test_that("tsci_fpc extracts the finite-population correction", {

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

  design <- tsci_create_design(
    data = data_object,
    weights = "weight",
    strata = "strata",
    clusters = "cluster",
    fpc = "fpc"
  )

  result <- tsci_fpc(design)

  expect_equal(result, c(100, 100, 100, 100))
})
