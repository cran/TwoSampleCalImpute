# Internal validation utilities for TwoSampleCalImpute

.validate_data <- function(data,
                           data_name = "data",
                           allow_empty = FALSE) {

  if (!is.data.frame(data)) {
    stop(
      sprintf("'%s' must be a data.frame.", data_name),
      call. = FALSE
    )
  }

  if (is.null(names(data))) {
    stop(
      sprintf("'%s' must have column names.", data_name),
      call. = FALSE
    )
  }

  if (anyDuplicated(names(data)) > 0L) {
    stop(
      sprintf("'%s' contains duplicated column names.", data_name),
      call. = FALSE
    )
  }

  if (!allow_empty && nrow(data) == 0L) {
    stop(
      sprintf("'%s' must contain at least one observation.", data_name),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.validate_variables <- function(data,
                                variables,
                                data_name = "data") {

  .validate_data(data, data_name = data_name)

  if (is.null(variables) || length(variables) == 0L) {
    stop(
      "'variables' must contain at least one variable name.",
      call. = FALSE
    )
  }

  if (!is.character(variables)) {
    stop(
      "'variables' must be a character vector.",
      call. = FALSE
    )
  }

  missing_variables <- setdiff(variables, names(data))

  if (length(missing_variables) > 0L) {
    stop(
      sprintf(
        "The following variables are missing from '%s': %s.",
        data_name,
        paste(missing_variables, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.validate_weights <- function(weights,
                              n = NULL,
                              data_name = "weights") {

  if (!is.numeric(weights)) {
    stop(
      sprintf("'%s' must be numeric.", data_name),
      call. = FALSE
    )
  }

  if (!is.null(n) && length(weights) != n) {
    stop(
      sprintf(
        "'%s' must have length %d.",
        data_name,
        n
      ),
      call. = FALSE
    )
  }

  if (anyNA(weights) || any(!is.finite(weights))) {
    stop(
      sprintf(
        "'%s' contains missing or non-finite values.",
        data_name
      ),
      call. = FALSE
    )
  }

  if (any(weights <= 0)) {
    stop(
      sprintf(
        "'%s' must contain strictly positive values.",
        data_name
      ),
      call. = FALSE
    )
  }

  invisible(TRUE)
}


.validate_auxiliary <- function(primary,
                                auxiliary,
                                variables = NULL) {

  .validate_data(primary, data_name = "primary")
  .validate_data(auxiliary, data_name = "auxiliary")

  if (is.null(variables) || length(variables) == 0L) {
    stop(
      "'variables' must contain at least one common variable.",
      call. = FALSE
    )
  }

  .validate_variables(
    primary,
    variables,
    data_name = "primary"
  )

  .validate_variables(
    auxiliary,
    variables,
    data_name = "auxiliary"
  )

  for (variable in variables) {

    primary_class <- class(primary[[variable]])[1L]
    auxiliary_class <- class(auxiliary[[variable]])[1L]

    if (!identical(primary_class, auxiliary_class)) {
      stop(
        sprintf(
          "Variable type mismatch for '%s': primary is '%s', auxiliary is '%s'.",
          variable,
          primary_class,
          auxiliary_class
        ),
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}


.check_missing <- function(data) {

  .validate_data(data)

  missing_count <- vapply(
    data,
    function(x) sum(is.na(x)),
    integer(1L)
  )

  missing_percent <- 100 * missing_count / nrow(data)

  data.frame(
    variable = names(data),
    missing_count = missing_count,
    missing_percent = missing_percent,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}


.check_duplicates <- function(data,
                              key = NULL) {

  .validate_data(data)

  if (is.null(key)) {

    duplicate_rows <- duplicated(data) | duplicated(data, fromLast = TRUE)

    has_duplicates <- any(duplicate_rows)
    n_duplicates <- sum(duplicated(data))

  } else {

    .validate_variables(
      data,
      key,
      data_name = "data"
    )

    key_data <- data[key]

    duplicate_rows <- duplicated(key_data) |
      duplicated(key_data, fromLast = TRUE)

    has_duplicates <- any(duplicate_rows)
    n_duplicates <- sum(duplicated(key_data))
  }

  list(
    has_duplicates = has_duplicates,
    n_duplicates = n_duplicates,
    duplicate_rows = duplicate_rows
  )
}


.check_arguments <- function(...) {

  arguments <- list(...)

  if (length(arguments) == 0L) {
    return(invisible(TRUE))
  }

  argument_names <- names(arguments)

  if (is.null(argument_names) ||
      any(is.na(argument_names)) ||
      any(argument_names == "")) {

    stop(
      "All arguments must be named.",
      call. = FALSE
    )
  }

  invisible(TRUE)
}
