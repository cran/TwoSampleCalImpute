#' Merge two TSCI data objects
#'
#' Merge two TSCI data objects
#'
#' Merges two `tsci_data` objects using one or more key variables.
#'
#' @param x A `tsci_data` object.
#' @param y A second `tsci_data` object.
#' @param by Character vector of variables used as merge keys.
#' @param all Logical value indicating whether to retain unmatched
#'   observations from both objects. Defaults to `FALSE`.
#'
#' @return A `tsci_data` object containing the merged data. The result
#'   retains the sample type and metadata from `x`; its identifier is
#'   retained only if that identifier is present in the merged data.
#'
#' @examples
#' primary_data <- data.frame(
#'   id = c(1, 2, 3),
#'   outcome = c(1, 0, 1)
#' )
#'
#' auxiliary_data <- data.frame(
#'   id = c(1, 2, 3),
#'   age_group = c("young", "middle", "older")
#' )
#'
#' primary <- tsci_import_data(
#'   primary_data,
#'   sample_type = "primary",
#'   id = "id"
#' )
#'
#' auxiliary <- tsci_import_data(
#'   auxiliary_data,
#'   sample_type = "auxiliary",
#'   id = "id"
#' )
#'
#' merged <- tsci_merge_data(
#'   primary,
#'   auxiliary,
#'   by = "id"
#' )
#'
#' names(merged$data)
#'
#' @export

tsci_merge_data <- function(x, y, by, all = FALSE) {
  if (!inherits(x, "tsci_data")) {
    stop("'x' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!inherits(y, "tsci_data")) {
    stop("'y' must be a 'tsci_data' object.", call. = FALSE)
  }

  if (!is.character(by) || length(by) < 1L ||
      anyNA(by) || any(!nzchar(by))) {
    stop("'by' must be a non-empty character vector.", call. = FALSE)
  }

  if (!is.logical(all) || length(all) != 1L || is.na(all)) {
    stop("'all' must be a single TRUE or FALSE value.", call. = FALSE)
  }

  if (!all(by %in% names(x$data))) {
    missing_x <- by[!by %in% names(x$data)]
    stop(
      sprintf(
        "Merge key(s) not found in 'x': %s.",
        paste(missing_x, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!all(by %in% names(y$data))) {
    missing_y <- by[!by %in% names(y$data)]
    stop(
      sprintf(
        "Merge key(s) not found in 'y': %s.",
        paste(missing_y, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  merged_data <- merge(
    x$data,
    y$data,
    by = by,
    all = all,
    sort = FALSE
  )

  .build_data_object(
    data = merged_data,
    sample_type = x$sample_type,
    id = if (length(x$id) == 1L && x$id %in% names(merged_data)) x$id else NULL,
    variables = names(merged_data),
    metadata = x$metadata
  )
}
