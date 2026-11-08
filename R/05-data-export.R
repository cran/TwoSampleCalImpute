#' Export survey data
#'
#' Exports the data contained in a `tsci_data` object to a CSV file.
#'
#' @param data A `tsci_data` object.
#' @param file Character path for the output CSV file. The filename must
#'   have a `.csv` extension.
#' @param row_names Logical; whether row names should be written.
#'
#' @return Invisibly returns the original `tsci_data` object supplied
#'   through `data`. The object retains its existing data and metadata.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:3,
#'   outcome = c(1, 0, 1)
#' )
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' output_file <- tempfile(fileext = ".csv")
#' returned <- tsci_export_data(imported, file = output_file)
#' file.exists(output_file)
#' identical(returned, imported)
#' unlink(output_file)
#'
#' @export
tsci_export_data <- function(data,
                              file,
                              row_names = FALSE) {

  if (!inherits(data, "tsci_data")) {
    stop(
      "'data' must be a 'tsci_data' object.",
      call. = FALSE
    )
  }

  if (!is.character(file) || length(file) != 1L || is.na(file)) {
    stop(
      "'file' must be a single character path.",
      call. = FALSE
    )
  }

  if (!is.logical(row_names) || length(row_names) != 1L || is.na(row_names)) {
    stop(
      "'row_names' must be a single TRUE or FALSE value.",
      call. = FALSE
    )
  }

  extension <- tolower(tools::file_ext(file))

  if (extension != "csv") {
    stop(
      "Only CSV files are currently supported.",
      call. = FALSE
    )
  }

  utils::write.csv(
    data$data,
    file = file,
    row.names = row_names
  )

  invisible(data)
}

