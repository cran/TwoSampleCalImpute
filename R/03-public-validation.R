#' Validate a survey data set
#'
#' Performs data-integrity checks before Two-Sample Calibrated
#' Imputation analysis, including input validation, optional variable
#' and weight checks, missingness summaries, and duplicate-key checks.
#'
#' @param data A data frame to validate.
#' @param data_name A character label used in validation messages.
#'   Defaults to `"data"`.
#' @param variables Optional character vector of variables to validate.
#'   Defaults to `NULL`.
#' @param weights Optional survey weights to validate. Defaults to `NULL`.
#' @param key Optional character vector identifying records for duplicate
#'   checks. Defaults to `NULL`.
#'
#' @return An object of class `c("tsci_validation", "list")` containing
#'   `valid` (validation status), `data_name` (the supplied data label),
#'   `n` (number of rows), `p` (number of columns), `variables` (column
#'   names), `missing` (missingness-check results), and `duplicates`
#'   (duplicate-key-check results). Invalid inputs cause an error.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, NA, 1),
#'   weight = c(1, 1, 1, 1)
#' )
#'
#' validation <- tsci_validate_data(
#'   survey_data,
#'   variables = c("outcome", "weight"),
#'   weights = survey_data$weight,
#'   key = "id"
#' )
#'
#' validation$valid
#' validation$n
#' validation$missing
#'
#' @export
tsci_validate_data <- function(data,
                                data_name = "data",
                                variables = NULL,
                                weights = NULL,
                                key = NULL) {

  .validate_data(
    data = data,
    data_name = data_name
  )

  if (!is.null(variables)) {
    .validate_variables(
      data = data,
      variables = variables,
      data_name = data_name
    )
  }

  if (!is.null(weights)) {
    .validate_weights(
      weights = weights,
      n = nrow(data),
      data_name = "weights"
    )
  }

  missing_summary <- .check_missing(data)

  duplicate_summary <- .check_duplicates(
    data = data,
    key = key
  )

  result <- list(
    valid = TRUE,
    data_name = data_name,
    n = nrow(data),
    p = ncol(data),
    variables = names(data),
    missing = missing_summary,
    duplicates = duplicate_summary
  )

  class(result) <- c(
    "tsci_validation",
    "list"
  )

  result
}

