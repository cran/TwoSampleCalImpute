#' Subset a TSCI data object
#'
#' Subsets observations in a `tsci_data` object using a logical
#' condition while preserving the TSCI data-object structure.
#'
#' @param data A `tsci_data` object.
#' @param subset A logical vector identifying observations to retain.
#'
#' @return A `tsci_data` object containing the selected observations.
#'   The sample type, identifier, and metadata are preserved, and the
#'   variable list is restricted to variables present in the result.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0)
#' )
#'
#' imported <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' selected <- tsci_subset_data(
#'   imported,
#'   subset = c(TRUE, FALSE, TRUE, FALSE)
#' )
#'
#' nrow(selected$data)
#' selected$data$id
#'
#' @export

tsci_subset_data <- function(data, subset) {
  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!is.logical(subset) || length(subset) != nrow(data$data) ||
      anyNA(subset)) {
    stop(
      "'subset' must be a logical vector of length equal to the number of observations and contain no NA values.",
      call. = FALSE
    )
  }

  subsetted_data <- data$data[subset, , drop = FALSE]

  .build_data_object(
    data = subsetted_data,
    sample_type = data$sample_type,
    id = data$id,
    variables = intersect(data$variables, names(subsetted_data)),
    metadata = data$metadata
  )
}
