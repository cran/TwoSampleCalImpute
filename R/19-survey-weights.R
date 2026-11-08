#' Extract survey weights
#'
#' Extracts and validates the numeric survey weights stored in a
#' `tsci_design` object.
#'
#' @param design A `tsci_design` object containing survey weights.
#'
#' @return A numeric vector containing the survey weights in their
#'   stored order. The vector has one element per observation and
#'   contains only finite, strictly positive values. An error is
#'   raised if the design object is invalid, weights are absent,
#'   weights are non-numeric, or any weight is missing, non-finite,
#'   or non-positive.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0),
#'   weight = c(1, 2, 1, 2)
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
#'   weights = "weight"
#' )
#'
#' tsci_weights(design_object)
#'
#' @export
tsci_weights <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  if (is.null(design$weights)) {
    stop("The survey design does not contain survey weights.",
         call. = FALSE)
  }

  weights <- design$weights

  if (!is.numeric(weights)) {
    stop("Survey weights must be numeric.", call. = FALSE)
  }

  if (anyNA(weights) || any(!is.finite(weights))) {
    stop("Survey weights contain missing or non-finite values.",
         call. = FALSE)
  }

  if (any(weights <= 0)) {
    stop("Survey weights must be strictly positive.",
         call. = FALSE)
  }

  weights
}
