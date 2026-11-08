#' Prepare survey data
#'
#' Prepares a `tsci_data` object for subsequent validation and
#' Two-Sample Calibrated Imputation analysis by applying the
#' specified empty-row and empty-column cleaning operations.
#'
#' @param data A `tsci_data` object.
#' @param remove_empty_rows Logical; whether completely empty rows
#'   should be removed. Defaults to `FALSE`.
#' @param remove_empty_columns Logical; whether completely empty
#'   columns should be removed. Defaults to `FALSE`.
#'
#' @return A prepared `tsci_data` object. Its data reflect the
#'   requested cleaning operations; its sample type, identifier,
#'   and metadata are preserved by the cleaning operation.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = c(1, 2, 3),
#'   outcome = c(1, NA, 0),
#'   empty_variable = c(NA, NA, NA)
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' prepared <- tsci_prepare_data(
#'   imported,
#'   remove_empty_columns = TRUE
#' )
#' names(prepared$data)
#'
#' @export
tsci_prepare_data <- function(data,
                               remove_empty_rows = FALSE,
                               remove_empty_columns = FALSE) {

  if (!inherits(data, "tsci_data")) {
    stop(
      "'data' must be a 'tsci_data' object.",
      call. = FALSE
    )
  }

  prepared_data <- tsci_clean_data(
    data = data,
    remove_empty_rows = remove_empty_rows,
    remove_empty_columns = remove_empty_columns
  )

  prepared_data
}

