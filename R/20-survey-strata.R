#' Extract the Stratification Variable
#'
#' Extracts the name of the stratification variable stored in a
#' \code{tsci_design} object. If no stratification variable was
#' specified, the function returns \code{NULL}. The stored
#' specification is validated against the variables in the
#' design data before the name is returned.
#'
#' @param design A \code{tsci_design} object containing the survey
#'   design specification and its associated data.
#'
#' @return A character string of length one containing the name
#'   of the stratification variable, or \code{NULL} if no
#'   stratification variable was specified. An error is raised if
#'   \code{design} is not a \code{tsci_design} object, if the stored
#'   stratification specification is invalid, or if the specified
#'   variable is absent from the design data.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0),
#'   weight = c(1, 1, 1, 1),
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
#' tsci_strata(design_object)
#'
#' design_without_strata <- tsci_create_design(
#'   data_object,
#'   weights = "weight"
#' )
#' tsci_strata(design_without_strata)
#'
#' @export
tsci_strata <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  strata <- design$strata

  if (is.null(strata)) {
    return(NULL)
  }

  if (!is.character(strata) ||
      length(strata) != 1L ||
      is.na(strata) ||
      !nzchar(strata)) {
    stop("The stored strata specification is invalid.",
         call. = FALSE)
  }

  if (!strata %in% names(design$data$data)) {
    stop(
      sprintf(
        "The strata variable '%s' is not present in the design data.",
        strata
      ),
      call. = FALSE
    )
  }

  strata
}
