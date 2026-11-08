#' Create a survey design object
#'
#' Constructs a `tsci_design` object from a `tsci_data` object and
#' specified survey-design variables. The weight variable must contain
#' numeric, finite, strictly positive values. If supplied, the
#' finite-population-correction (FPC) variable must also contain
#' numeric, finite, strictly positive values.
#'
#' @param data A `tsci_data` object containing the survey observations.
#' @param weights Character string naming the survey-weight variable
#'   in `data$data`.
#' @param strata Optional character string naming the stratification
#'   variable in `data$data`. Defaults to `NULL`.
#' @param clusters Optional character string naming the cluster
#'   variable in `data$data`. Defaults to `NULL`.
#' @param fpc Optional character string naming the finite-population-
#'   correction variable in `data$data`. Defaults to `NULL`.
#'
#' @return An object of class `tsci_design`, represented as a list
#'   containing the survey-design information constructed from the input
#'   data object. The object includes the survey weights and the
#'   specified design variables, with the underlying data-object
#'   information retained according to the package's design-object
#'   constructor.
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
#' print(design_object)
#'
#' @export
tsci_create_design <- function(data,
                               weights,
                               strata = NULL,
                               clusters = NULL,
                               fpc = NULL) {

  if (!inherits(data, "tsci_data")) {
    stop("'data' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!is.character(weights) ||
      length(weights) != 1L ||
      is.na(weights) ||
      !nzchar(weights)) {
    stop("'weights' must be a single variable name.", call. = FALSE)
  }

  if (!weights %in% names(data$data)) {
    stop(
      sprintf(
        "Weight variable '%s' is not present in the data.",
        weights
      ),
      call. = FALSE
    )
  }

  if (!is.null(strata)) {
    if (!is.character(strata) ||
        length(strata) != 1L ||
        is.na(strata) ||
        !nzchar(strata)) {
      stop("'strata' must be a single variable name or NULL.",
           call. = FALSE)
    }

    if (!strata %in% names(data$data)) {
      stop(
        sprintf(
          "Strata variable '%s' is not present in the data.",
          strata
        ),
        call. = FALSE
      )
    }
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

    if (!clusters %in% names(data$data)) {
      stop(
        sprintf(
          "Cluster variable '%s' is not present in the data.",
          clusters
        ),
        call. = FALSE
      )
    }
  }

  if (!is.null(fpc)) {
    if (!is.character(fpc) ||
        length(fpc) != 1L ||
        is.na(fpc) ||
        !nzchar(fpc)) {
      stop("'fpc' must be a single variable name or NULL.",
           call. = FALSE)
    }

    if (!fpc %in% names(data$data)) {
      stop(
        sprintf(
          "FPC variable '%s' is not present in the data.",
          fpc
        ),
        call. = FALSE
      )
    }
  }

  weight_values <- data$data[[weights]]

  if (!is.numeric(weight_values)) {
    stop(
      sprintf(
        "Weight variable '%s' must be numeric.",
        weights
      ),
      call. = FALSE
    )
  }

  if (anyNA(weight_values) ||
      any(!is.finite(weight_values))) {
    stop(
      sprintf(
        "Weight variable '%s' contains missing or non-finite values.",
        weights
      ),
      call. = FALSE
    )
  }

  if (any(weight_values <= 0)) {
    stop(
      sprintf(
        "Weight variable '%s' must contain strictly positive values.",
        weights
      ),
      call. = FALSE
    )
  }

  fpc_values <- NULL

  if (!is.null(fpc)) {
    fpc_values <- data$data[[fpc]]

    if (!is.numeric(fpc_values)) {
      stop(
        sprintf(
          "FPC variable '%s' must be numeric.",
          fpc
        ),
        call. = FALSE
      )
    }

    if (anyNA(fpc_values) ||
        any(!is.finite(fpc_values))) {
      stop(
        sprintf(
          "FPC variable '%s' contains missing or non-finite values.",
          fpc
        ),
        call. = FALSE
      )
    }

    if (any(fpc_values <= 0)) {
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
    weights = weight_values,
    id = data$id,
    strata = strata,
    clusters = clusters,
    fpc = fpc_values
  )
}

