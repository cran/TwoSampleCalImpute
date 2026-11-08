#' Extract the Cluster Variable
#'
#' Extracts the name of the cluster variable stored in a
#' \code{tsci_design} object. If no cluster variable was specified,
#' the function returns \code{NULL}. The stored specification is
#' validated against the variables in the design data before the
#' name is returned.
#'
#' @param design A \code{tsci_design} object containing the survey
#'   design specification and its associated data.
#'
#' @return A character string of length one containing the name
#'   of the cluster variable, or \code{NULL} if no cluster variable
#'   was specified. An error is raised if \code{design} is not a
#'   \code{tsci_design} object, if the stored cluster specification
#'   is invalid, or if the specified variable is absent from the
#'   design data.
#'
#' @examples
#' survey_data <- data.frame(
#'   id = 1:4,
#'   outcome = c(1, 0, 1, 0),
#'   weight = c(1, 1, 1, 1),
#'   cluster = c("C1", "C1", "C2", "C2")
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
#'   clusters = "cluster"
#' )
#'
#' tsci_clusters(design_object)
#'
#' design_without_clusters <- tsci_create_design(
#'   data_object,
#'   weights = "weight"
#' )
#' tsci_clusters(design_without_clusters)
#'
#' @export
tsci_clusters <- function(design) {

  if (!inherits(design, "tsci_design")) {
    stop(
      "'design' must be a 'tsci_design' object.",
      call. = FALSE
    )
  }

  clusters <- design$clusters

  if (is.null(clusters)) {
    return(NULL)
  }

  if (!is.character(clusters) ||
      length(clusters) != 1L ||
      is.na(clusters) ||
      !nzchar(clusters)) {
    stop(
      "The stored clusters specification is invalid.",
      call. = FALSE
    )
  }

  if (!clusters %in% names(design$data$data)) {
    stop(
      sprintf(
        "The cluster variable '%s' is not present in the design data.",
        clusters
      ),
      call. = FALSE
    )
  }

  clusters
}
