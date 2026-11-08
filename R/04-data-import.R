#' Import survey data
#'
#' Imports a data set from a supported file or an existing R object
#' and returns it as a standardized `tsci_data` object.
#'
#' @param data A data frame, or a character path to a CSV file.
#' @param sample_type Either `"primary"` or `"auxiliary"`.
#' @param id Optional character identifier variable.
#' @param variables Optional character vector of variables to retain.
#' @param metadata Optional list of metadata.
#'
#' @return A `tsci_data` object, represented as a list containing the
#' imported data, sample type, identifier, variable names, and metadata.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:3,
#'   outcome = c(1, 0, 1),
#'   age = c(35, 42, 29)
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' print(imported)
#'
#' @export
tsci_import_data <- function(data,
                              sample_type = c("primary", "auxiliary"),
                              id = NULL,
                              variables = NULL,
                              metadata = list()) {

  sample_type <- match.arg(sample_type)

  if (is.data.frame(data)) {
    imported_data <- data
  } else if (is.character(data) && length(data) == 1L) {

    if (!file.exists(data)) {
      stop(
        sprintf("File does not exist: '%s'.", data),
        call. = FALSE
      )
    }

    extension <- tolower(
      tools::file_ext(data)
    )

    if (extension != "csv") {
      stop(
        "Only CSV files are currently supported.",
        call. = FALSE
      )
    }

    imported_data <- utils::read.csv(
      data,
      stringsAsFactors = FALSE
    )

  } else {

    stop(
      "'data' must be a data.frame or a path to a CSV file.",
      call. = FALSE
    )
  }

  .build_data_object(
    data = imported_data,
    sample_type = sample_type,
    id = id,
    variables = variables,
    metadata = metadata
  )
}

