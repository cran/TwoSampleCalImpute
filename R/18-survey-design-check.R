#' Validate survey-design information
#'
#' Checks the structural and numerical validity of a `tsci_design`
#' object, including its underlying data object, number of observations,
#' survey weights, stratification variable, and finite-population
#' correction values.
#'
#' @param design A `tsci_design` object to validate.
#'
#' @return An object of class `c("tsci_design_check", "list")`
#'   containing:
#'   \describe{
#'     \item{valid}{Logical value indicating whether all checks pass.}
#'     \item{checks}{Named list of logical results for the individual
#'       structural and numerical checks.}
#'     \item{sample_type}{Sample type recorded in the underlying
#'       `tsci_data` object.}
#'     \item{observations}{Number of rows in the underlying data.}
#'     \item{id}{Identifier information stored in the design object.}
#'     \item{strata}{Stratification variable information stored in the
#'       design object.}
#'     \item{has_weights}{Logical value indicating whether survey
#'       weights are stored.}
#'     \item{has_fpc}{Logical value indicating whether finite-population
#'       correction values are stored.}
#'   }
#'   The `valid` field is `TRUE` only when every individual check passes.
#'   An input that is not a `tsci_design` object causes an error.
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
#' check_result <- tsci_design_check(design_object)
#' check_result$valid
#' check_result$checks
#'
#' @export
tsci_design_check <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  data_object <- design$data
  data_frame <- data_object$data
  n <- nrow(data_frame)

  checks <- list(
    data_object = inherits(data_object, "tsci_data"),
    observations = n > 0L,
    weights = !is.null(design$weights),
    weights_length = !is.null(design$weights) && length(design$weights) == n,
    weights_numeric = !is.null(design$weights) && is.numeric(design$weights),
    weights_finite = !is.null(design$weights) &&
      all(is.finite(design$weights)),
    weights_positive = !is.null(design$weights) &&
      all(design$weights > 0),
    strata = is.null(design$strata) ||
      (is.character(design$strata) &&
       length(design$strata) == 1L &&
       design$strata %in% names(data_frame)),
    fpc = is.null(design$fpc) ||
      (is.numeric(design$fpc) &&
       length(design$fpc) == n &&
       all(is.finite(design$fpc)) &&
       all(design$fpc > 0))
  )

  valid <- all(unlist(checks))

  result <- list(
    valid = valid,
    checks = checks,
    sample_type = data_object$sample_type,
    observations = n,
    id = design$id,
    strata = design$strata,
    has_weights = !is.null(design$weights),
    has_fpc = !is.null(design$fpc)
  )

  class(result) <- c("tsci_design_check", "list")

  result
}
