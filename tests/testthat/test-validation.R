test_that(".validate_data accepts a valid data frame", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, NA, 30)
  )

  expect_true(
    TwoSampleCalImpute:::.validate_data(dat)
  )
})


test_that(".validate_data rejects non-data-frame input", {
  expect_error(
    TwoSampleCalImpute:::.validate_data(c(1, 2, 3)),
    "must be a data.frame"
  )
})


test_that(".validate_variables detects missing variables", {
  dat <- data.frame(
    id = 1:3,
    y = 1:3
  )

  expect_true(
    TwoSampleCalImpute:::.validate_variables(
      dat,
      c("id", "y")
    )
  )

  expect_error(
    TwoSampleCalImpute:::.validate_variables(
      dat,
      c("id", "weight")
    ),
    "weight"
  )
})


test_that(".validate_weights accepts valid positive weights", {
  expect_true(
    TwoSampleCalImpute:::.validate_weights(
      c(1, 2, 3),
      n = 3
    )
  )
})


test_that(".validate_weights rejects invalid weights", {
  expect_error(
    TwoSampleCalImpute:::.validate_weights(
      c(1, 0, 3),
      n = 3
    ),
    "strictly positive"
  )

  expect_error(
    TwoSampleCalImpute:::.validate_weights(
      c(1, NA, 3),
      n = 3
    ),
    "missing or non-finite"
  )
})


test_that(".validate_auxiliary checks compatible variables", {
  primary <- data.frame(
    id = 1:3,
    age = c(20, 30, 40)
  )

  auxiliary <- data.frame(
    id = 4:6,
    age = c(25, 35, 45)
  )

  expect_true(
    TwoSampleCalImpute:::.validate_auxiliary(
      primary,
      auxiliary,
      variables = "age"
    )
  )
})


test_that(".validate_auxiliary detects variable type mismatch", {
  primary <- data.frame(
    age = c(20, 30, 40)
  )

  auxiliary <- data.frame(
    age = as.character(c(20, 30, 40))
  )

  expect_error(
    TwoSampleCalImpute:::.validate_auxiliary(
      primary,
      auxiliary,
      variables = "age"
    ),
    "Variable type mismatch"
  )
})


test_that(".check_missing reports missing values without changing data", {
  dat <- data.frame(
    id = 1:3,
    y = c(10, NA, 30)
  )

  result <- TwoSampleCalImpute:::.check_missing(dat)

  expect_equal(
    result$missing_count[result$variable == "y"],
    1
  )

  expect_equal(
    result$missing_percent[result$variable == "y"],
    100 / 3
  )
})


test_that(".check_duplicates detects duplicate rows", {
  dat <- data.frame(
    id = c(1, 2, 2),
    y = c(10, 20, 20)
  )

  result <- TwoSampleCalImpute:::.check_duplicates(dat)

  expect_true(result$has_duplicates)
  expect_equal(result$n_duplicates, 1)
})


test_that(".check_arguments accepts named arguments", {
  expect_true(
    TwoSampleCalImpute:::.check_arguments(
      data = data.frame(x = 1:3),
      method = "test"
    )
  )
})
