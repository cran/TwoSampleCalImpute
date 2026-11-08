#' Recode variables in a TSCI data object
#'
#' Applies user-supplied recoding functions to one or more variables
#' in a `tsci_data` object.
#'
#' @param data A `tsci_data` object.
#' @param recodes A named list of functions. Each name must identify
#'   a variable in `data`.
#'
#' @return A `tsci_data` object containing the recoded variables.
#'   The sample type, identifier, variable list, and metadata are
#'   preserved from the input object.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   response = c("Yes", "No", "Yes", "No")
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' recoded <- tsci_recode_variables(
#'   imported,
#'   recodes = list(response = function(x) {
#'     ifelse(x == "Yes", 1, 0)
#'   })
#' )
#'
#' recoded$data$response
#'
#' @export

tsci_recode_variables <- function(data, recodes) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!is.list(recodes) || is.null(names(recodes)) ||
      anyNA(names(recodes)) || any(!nzchar(names(recodes))) ||
      length(recodes) < 1L) {
    stop("'recodes' must be a non-empty named list.", call. = FALSE)
  }

  variables <- names(recodes)

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

  if (!all(vapply(recodes, is.function, logical(1L)))) {
    stop("Every element of 'recodes' must be a function.", call. = FALSE)
  }

  recoded_data <- data$data

  for (variable in variables) {
    recoded_data[[variable]] <- recodes[[variable]](
      recoded_data[[variable]]
    )
  }

  .build_data_object(
    data = recoded_data,
    sample_type = data$sample_type,
    id = data$id,
    variables = data$variables,
    metadata = data$metadata
  )
}
