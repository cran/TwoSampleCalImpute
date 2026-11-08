#' Summarise a survey design
#'
#' Produces a concise summary of the main components of a
#' `tsci_design` object.
#'
#' @param design A `tsci_design` object.
#'
#' @return A one-row data frame with seven columns:
#'   `sample_type` (type of sample represented by the underlying data),
#'   `observations` (number of observations),
#'   `variables` (number of variables in the data),
#'   `id` (identifier variable name, or `NA` if none is specified),
#'   `weights` (logical indicator of whether survey weights are stored),
#'   `strata` (stratification variable name, or `NA` if none is specified),
#'   and `fpc` (logical indicator of whether finite-population-correction
#'   values are stored).
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0),
#'   weight = c(1, 2, 1, 2),
#'   stratum = c("A", "A", "B", "B")
#' )
#'
#' data_object <- tsci_import_data(
#'   survey_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' design_object <- tsci_create_design(
#'   data_object,
#'   weights = "weight",
#'   strata = "stratum"
#' )
#'
#' tsci_design_summary(design_object)
#'
#' @export
tsci_design_summary <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  data_object <- design$data
  data_frame <- data_object$data

  data.frame(
    sample_type = data_object$sample_type,
    observations = nrow(data_frame),
    variables = ncol(data_frame),
    id = if (is.null(design$id)) NA_character_ else design$id,
    weights = if (is.null(design$weights)) FALSE else TRUE,
    strata = if (is.null(design$strata)) NA_character_ else design$strata,
    fpc = if (is.null(design$fpc)) FALSE else TRUE,
    stringsAsFactors = FALSE
  )
}
