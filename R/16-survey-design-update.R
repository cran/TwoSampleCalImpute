#' Update a survey design object
#'
#' Updates selected components of an existing `tsci_design` object.
#' Non-`NULL` values supplied for `weights`, `strata`, and `fpc`
#' replace the corresponding stored components. The `clusters`
#' argument is passed to the design-object constructor, so omitting it
#' does not guarantee that a previously specified cluster setting is
#' retained.
#'
#' @param design A `tsci_design` object to update.
#' @param weights Optional character string naming the survey-weight
#'   variable in the underlying data. If `NULL`, the existing stored
#'   weights are used.
#' @param strata Optional character string naming the stratification
#'   variable in the underlying data. If `NULL`, the existing stored
#'   strata setting is used.
#' @param clusters Optional character string naming the cluster
#'   variable in the underlying data. Defaults to `NULL`.
#' @param fpc Optional character string naming the finite-population-
#'   correction variable in the underlying data. If `NULL`, the
#'   existing stored FPC values are used.
#'
#' @return An object of class `tsci_design` containing the updated
#'   survey-design components and the underlying data-object
#'   information. The weights and FPC values are checked for numeric,
#'   finite, strictly positive values when their variable names are
#'   supplied. Invalid design inputs cause an error.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0),
#'   weight = c(1, 2, 1, 2),
#'   updated_weight = c(2, 2, 3, 3),
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
#' updated_design <- tsci_update_design(
#'   design_object,
#'   weights = "updated_weight"
#' )
#'
#' print(updated_design)
#'
#' @export
tsci_update_design <- function(design,
                                weights = NULL,
                                strata = NULL,
                                clusters = NULL,
                                fpc = NULL) {

  if (!inherits(design, "tsci_design")) {
    stop("'design' must be a 'tsci_design' object.", call. = FALSE)
  }

  data <- design$data
  available_variables <- names(data$data)

  check_variable <- function(x, argument_name) {
    if (is.null(x)) {
      return(NULL)
    }

    if (!is.character(x) ||
        length(x) != 1L ||
        is.na(x) ||
        !nzchar(x)) {
      stop(
        sprintf("'%s' must be a single variable name or NULL.",
                argument_name),
        call. = FALSE
      )
    }

    if (!x %in% available_variables) {
      stop(
        sprintf(
          "Variable '%s' is not present in the design data.",
          x
        ),
        call. = FALSE
      )
    }

    x
  }

  if (!is.null(clusters)) {
    if (!is.character(clusters) ||
        length(clusters) != 1L ||
        is.na(clusters) ||
        !nzchar(clusters)) {
      stop(
        "'clusters' must be a single variable name or NULL.",
        call. = FALSE
      )
    }

    if (!clusters %in% available_variables) {
      stop(
        sprintf(
          "Cluster variable '%s' is not present in the design data.",
          clusters
        ),
        call. = FALSE
      )
    }
  }

  weights <- check_variable(weights, "weights")
  strata <- check_variable(strata, "strata")
  fpc <- check_variable(fpc, "fpc")

  updated_weights <- design$weights
  updated_strata <- design$strata
  updated_fpc <- design$fpc

  if (!is.null(weights)) {
    updated_weights <- data$data[[weights]]

    if (!is.numeric(updated_weights)) {
      stop(
        sprintf("Weight variable '%s' must be numeric.", weights),
        call. = FALSE
      )
    }

    if (anyNA(updated_weights) ||
        any(!is.finite(updated_weights))) {
      stop(
        sprintf(
          "Weight variable '%s' contains missing or non-finite values.",
          weights
        ),
        call. = FALSE
      )
    }

    if (any(updated_weights <= 0)) {
      stop(
        sprintf(
          "Weight variable '%s' must contain strictly positive values.",
          weights
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(strata)) {
    updated_strata <- strata
  }

  if (!is.null(fpc)) {
    updated_fpc <- data$data[[fpc]]

    if (!is.numeric(updated_fpc)) {
      stop(
        sprintf("FPC variable '%s' must be numeric.", fpc),
        call. = FALSE
      )
    }

    if (anyNA(updated_fpc) ||
        any(!is.finite(updated_fpc))) {
      stop(
        sprintf(
          "FPC variable '%s' contains missing or non-finite values.",
          fpc
        ),
        call. = FALSE
      )
    }

    if (any(updated_fpc <= 0)) {
      stop(
        sprintf(
          "FPC variable '%s' must contain strictly positive values.",
          fpc
        ),
        call. = FALSE
      )
    }
  }

  .build_design_object(
    data_object = data,
    weights = updated_weights,
    id = design$id,
    strata = updated_strata,
    clusters = clusters,
    fpc = updated_fpc
  )
}

