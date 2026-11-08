#' Clean survey data
#'
#' Performs controlled data-cleaning operations on a `tsci_data`
#' object without silently changing observed values. Completely empty
#' rows or columns are removed only when the corresponding option is
#' set to `TRUE`.
#'
#' @param data A `tsci_data` object.
#' @param remove_empty_rows Logical; whether completely empty rows
#'   should be removed. Defaults to `FALSE`.
#' @param remove_empty_columns Logical; whether completely empty
#'   columns should be removed. Defaults to `FALSE`.
#'
#' @return A cleaned `tsci_data` object containing the resulting data,
#'   with its sample type, identifier, and metadata preserved. The
#'   variable list is updated to include only variables still present.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = c(1, 2, 3),
#'   outcome = c(1, NA, 0),
#'   auxiliary = c(NA, NA, NA)
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' cleaned <- tsci_clean_data(
#'   imported,
#'   remove_empty_columns = TRUE
#' )
#' names(cleaned$data)
#'
#' @export
tsci_clean_data <- function(data,
                             remove_empty_rows = FALSE,
                             remove_empty_columns = FALSE) {

  if (!inherits(data, "tsci_data")) {
    stop(
      "'data' must be a 'tsci_data' object.",
      call. = FALSE
    )
  }

  if (!is.logical(remove_empty_rows) ||
      length(remove_empty_rows) != 1L ||
      is.na(remove_empty_rows)) {
    stop(
      "'remove_empty_rows' must be a single TRUE or FALSE value.",
      call. = FALSE
    )
  }

  if (!is.logical(remove_empty_columns) ||
      length(remove_empty_columns) != 1L ||
      is.na(remove_empty_columns)) {
    stop(
      "'remove_empty_columns' must be a single TRUE or FALSE value.",
      call. = FALSE
    )
  }

  cleaned_data <- data$data

  if (isTRUE(remove_empty_rows)) {
    keep_rows <- !apply(
      cleaned_data,
      1L,
      function(x) all(is.na(x))
    )

    cleaned_data <- cleaned_data[keep_rows, , drop = FALSE]
  }

  if (isTRUE(remove_empty_columns)) {
    keep_columns <- !vapply(
      cleaned_data,
      function(x) all(is.na(x)),
      logical(1L)
    )

    cleaned_data <- cleaned_data[, keep_columns, drop = FALSE]
  }

  .build_data_object(
    data = cleaned_data,
    sample_type = data$sample_type,
    id = data$id,
    variables = intersect(data$variables, names(cleaned_data)),
    metadata = data$metadata
  )
}

