#' Transform variables in a TSCI data object
#'
#' Applies a user-supplied transformation function to one or more
#' variables in a `tsci_data` object while preserving the TSCI data
#' object structure.
#'
#' @param data A `tsci_data` object.
#' @param variables Character vector of variable names to transform.
#' @param fun A function used to transform each selected variable.
#' @param ... Additional arguments passed to `fun`.
#'
#' @return A `tsci_data` object containing the transformed variables.
#' @export
#'
#' @examples
#' x <- data.frame(id = 1:3, y = c(2, 4, 6))
#' obj <- tsci_import_data(x, sample_type = "primary", id = "id")
#' tsci_transform_data(obj, variables = "y", fun = log)

tsci_transform_data <- function(data, variables, fun, ...) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!is.character(variables) || length(variables) < 1L ||
      anyNA(variables) || any(!nzchar(variables))) {
    stop("'variables' must be a non-empty character vector.", call. = FALSE)
  }

  if (!all(variables %in% names(data$data))) {
    missing_variables <- variables[!variables %in% names(data$data)]
    stop(
      sprintf(
        "Variables not found: %s.",
        paste(missing_variables, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.function(fun)) {
    stop("'fun' must be a function.", call. = FALSE)
  }

  transformed_data <- data$data

  for (variable in variables) {
    transformed_data[[variable]] <- fun(
      transformed_data[[variable]],
      ...
    )
  }

  .build_data_object(
    data = transformed_data,
    sample_type = data$sample_type,
    id = data$id,
    variables = data$variables,
    metadata = data$metadata
  )
}
