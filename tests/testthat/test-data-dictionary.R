test_that("tsci_data_dictionary creates a dictionary", {
  test_data <- data.frame(
    id = c(1, 2, 3, 4),
    age = c(20, 30, NA, 40),
    sex = c("M", "F", "M", "F")
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id",
    metadata = list(
      description = c(
        id = "Unique participant identifier",
        age = "Participant age",
        sex = "Participant sex"
      )
    )
  )

  result <- tsci_data_dictionary(test_object)

  expect_equal(nrow(result), 3L)
  expect_equal(result$variable, c("id", "age", "sex"))
  expect_equal(result$n, c(4, 4, 4))
  expect_equal(result$missing, c(0, 1, 0))
  expect_equal(
    result$description,
    c(
      "Unique participant identifier",
      "Participant age",
      "Participant sex"
    )
  )
})

test_that("tsci_data_dictionary handles missing descriptions", {
  test_data <- data.frame(
    id = c(1, 2),
    y = c(10, 20)
  )

  test_object <- tsci_import_data(
    data = test_data,
    sample_type = "primary",
    id = "id"
  )

  result <- tsci_data_dictionary(test_object)

  expect_equal(nrow(result), 2L)
  expect_true(all(is.na(result$description)))
})

test_that("tsci_data_dictionary rejects invalid data", {
  expect_error(
    tsci_data_dictionary(data.frame(id = 1:2)),
    "'data' must be a 'tsci_data' object"
  )
})
