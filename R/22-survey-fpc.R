#' Extract the finite-population correction
#'
#' Extracts and validates the finite-population-correction (FPC)
#' values stored in a \code{tsci_design} object.
#'
#' @param design A \code{tsci_design} object containing the survey
#'   design specification and its associated data.
#'
#' @return A numeric vector containing one finite, strictly positive
#'   FPC value per observation, in the stored order, or \code{NULL}
#'   if no FPC was specified. An error is raised if \code{design}
#'   is not a \code{tsci_design} object, or if the stored FPC is
#'   non-numeric, has an incorrect length, contains missing or
#'   non-finite values, or contains non-positive values.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0),
#'   weight = c(1, 2, 1, 2),
#'   fpc = c(100, 100, 200, 200)
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
#'   fpc = "fpc"
#' )
#'
#' tsci_fpc(design_object)
#'
#' design_without_fpc <- tsci_create_design(
#'   data_object,
#'   weights = "weight"
#' )
#' tsci_fpc(design_without_fpc)
#'
#' @export
tsci_fpc <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop(
      "'design' must be a 'tsci_design' object.",
      call. = FALSE
    )
  }

  fpc <- design$fpc

  if (is.null(fpc)) {
    return(NULL)
  }

  if (!is.numeric(fpc)) {
    stop(
      "The stored FPC specification must be numeric.",
      call. = FALSE
    )
  }

  if (length(fpc) != nrow(design$data$data)) {
    stop(
      "The stored FPC must have the same length as the number of observations.",
      call. = FALSE
    )
  }

  if (anyNA(fpc) || any(!is.finite(fpc))) {
    stop(
      "The stored FPC contains missing or non-finite values.",
      call. = FALSE
    )
  }

  if (any(fpc <= 0)) {
    stop(
      "The stored FPC must contain strictly positive values.",
      call. = FALSE
    )
  }

  fpc
}
