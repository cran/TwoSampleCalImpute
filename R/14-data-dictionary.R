#' Create a data dictionary for a TSCI data object
#'
#' Produces a structured data dictionary describing the variables
#' in a `tsci_data` object.
#'
#' @param data A `tsci_data` object.
#'
#' @return A data frame with one row per variable and five columns:
#'   `variable` (variable name), `class` (R class or classes),
#'   `n` (number of observations), `missing` (number of missing
#'   values), and `description` (variable description from the
#'   object's metadata, or `NA` if no matching description is available).
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:3,
#'   outcome = c(1, NA, 0),
#'   age = c(35, 42, 29)
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id",
#'   metadata = list(
#'     description = c(
#'       outcome = "Recorded outcome",
#'       age = "Age in years"
#'     )
#'   )
#' )
#'
#' tsci_data_dictionary(imported)
#'
#' @export

tsci_data_dictionary <- function(data) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  variables <- names(data$data)
  metadata <- data$metadata

  description <- rep(NA_character_, length(variables))

  if (is.list(metadata) && !is.null(metadata$description)) {
    if (is.character(metadata$description) &&
        !is.null(names(metadata$description))) {
      matched <- variables %in% names(metadata$description)
      description[matched] <- unname(
        metadata$description[variables[matched]]
      )
    }
  }

  data.frame(
    variable = variables,
    class = vapply(
      data$data,
      function(x) paste(class(x), collapse = ", "),
      character(1L)
    ),
    n = nrow(data$data),
    missing = vapply(
      data$data,
      function(x) sum(is.na(x)),
      integer(1L)
    ),
    description = description,
    stringsAsFactors = FALSE
  )
}
