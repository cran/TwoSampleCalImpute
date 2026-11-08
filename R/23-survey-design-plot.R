#' Plot the Survey-Design Structure
#'
#' Produces a base-R plot indicating whether weights, strata,
#' clusters, and finite-population correction (FPC) values are
#' specified in a \code{tsci_design} object. A component is
#' displayed as specified when its corresponding stored value
#' is not \code{NULL}.
#'
#' @param design A \code{tsci_design} object containing the survey
#'   design specification and its associated data.
#'
#' @return The input \code{tsci_design} object, returned invisibly.
#'   The function also produces a plot with one indicator for each
#'   of the four design components: weights, strata, clusters,
#'   and FPC. An error is raised if \code{design} is not a
#'   \code{tsci_design} object.
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
#' returned_design <- tsci_design_plot(design_object)
#' stopifnot(inherits(returned_design, "tsci_design"))
#'
#' @export
tsci_design_plot <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop(
      "'design' must be a 'tsci_design' object.",
      call. = FALSE
    )
  }

  data_object <- design$data
  data_frame <- data_object$data

  n <- nrow(data_frame)

  components <- c(
    "Weights",
    "Strata",
    "Clusters",
    "FPC"
  )

  present <- c(
    !is.null(design$weights),
    !is.null(design$strata),
    !is.null(design$clusters),
    !is.null(design$fpc)
  )

  plot(
    x = seq_along(components),
    y = as.numeric(present),
    type = "h",
    xaxt = "n",
    yaxt = "n",
    xlab = "Survey-design component",
    ylab = "Specified",
    main = "TSCI Survey-Design Structure",
    ylim = c(0, 1.2)
  )

  graphics::axis(
    side = 1,
    at = seq_along(components),
    labels = components
  )

  graphics::axis(
    side = 2,
    at = c(0, 1),
    labels = c("No", "Yes")
  )

  invisible(design)
}
