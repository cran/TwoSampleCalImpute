#' Summarize variables in a TSCI data object
#'
#' Produces descriptive summaries for selected variables in a
#' `tsci_data` object.
#'
#' @param data A `tsci_data` object.
#' @param variables Optional character vector of variables to summarize.
#'   If `NULL`, all variables are summarized.
#'
#' @return A data frame with one row per selected variable and five
#'   columns: `variable` (variable name), `class` (R class),
#'   `n` (number of observations), `missing` (number of missing
#'   values), and `unique` (number of distinct non-missing values).
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, NA, 1),
#'   group = c("A", "B", "A", "B")
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' tsci_variable_summary(imported, variables = "outcome")
#'
#' @export

tsci_variable_summary <- function(data, variables = NULL) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  available_variables <- names(data$data)

  if (is.null(variables)) {
    variables <- available_variables
  } else {
    if (!is.character(variables) || length(variables) < 1L ||
        anyNA(variables) || any(!nzchar(variables))) {
      stop("'variables' must be a non-empty character vector or NULL.",
           call. = FALSE)
    }

    if (!all(variables %in% available_variables)) {
      missing_variables <- variables[!variables %in% available_variables]
      stop(
        sprintf(
          "Variables not found: %s.",
          paste(missing_variables, collapse = ", ")
        ),
        call. = FALSE
      )
    }
  }

  summary_list <- lapply(
    variables,
    function(variable) {
      x <- data$data[[variable]]

      data.frame(
        variable = variable,
        class = paste(class(x), collapse = ", "),
        n = length(x),
        missing = sum(is.na(x)),
        unique = length(unique(x[!is.na(x)])),
        stringsAsFactors = FALSE
      )
    }
  )

  do.call(rbind, summary_list)
}
