#' Compare ARDs
#'
#' @description
#' `compare_ard()` compares columns of two ARDs row-by-row using a shared set
#' of key columns. Rows where the column values differ are returned.
#'
#' @param x (`card`)\cr
#'
#'   first ARD to compare.
#' @param y (`card`)\cr
#'   second ARD to compare.
#' @param keys ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns identifying unique records. The intersection of the selected
#'   columns in both ARDs is used. Default is
#'   `c(all_ard_groups(), all_ard_variables(), any_of(c("variable", "variable_level", "stat_name")))`.
#' @param compare ([`tidy-select`][dplyr::dplyr_tidy_select])\cr
#'   columns to compare between the two ARDs. Default is
#'   `any_of(c("stat_label", "stat", "stat_fmt"))`.
#'
#' @return a named list of class `"ard_comparison"` containing:
#'
#'   - `rows_in_x_not_y`: data frame of rows present in `x` but not in `y`
#'     (based on key columns)
#'   - `rows_in_y_not_x`: data frame of rows present in `y` but not in `x`
#'     (based on key columns)
#'   - `compare`: a named list where each element is a data frame containing
#'     the key columns and the compared column values from both ARDs for rows
#'     where values differ
#'
#' @export
#'
#' @examples
#' ard_base <- ard_summary(ADSL, variables = AGE)
#' ard_modified <- ard_summary(dplyr::mutate(ADSL, AGE = AGE + 1), variables = AGE)
#'
#' compare_ard(ard_base, ard_modified)$compare$stat
#'
compare_ard <- function(x,
                        y,
                        keys = c(all_ard_groups(), all_ard_variables(), any_of(c("variable", "variable_level", "stat_name"))),
                        compare = any_of(c("stat_label", "stat", "stat_fmt"))) {
  set_cli_abort_call()

  check_class(x, cls = "card")
  check_class(y, cls = "card")

  # process keys and compare arguments -----------------------------------------
  keys <- .process_keys_arg(x, y, keys = {{ keys }})
  compare <- .process_compare_arg(x, y, compare = {{ compare }})

  # check for duplicates in keys -----------------------------------------------
  .check_keys_unique(x, keys, arg_name = "x")
  .check_keys_unique(y, keys, arg_name = "y")

  # initialize results list ----------------------------------------------------

  results <- rlang::rep_named(c("rows_in_x_not_y", "rows_in_y_not_x"), list(NULL))
  results[["compare"]] <- rlang::rep_named(compare, list(NULL))

  # find rows present in one ARD but not the other -----------------------------
  results[["rows_in_x_not_y"]] <- .compare_rows(x, y, keys)
  results[["rows_in_y_not_x"]] <- .compare_rows(y, x, keys)

  # compare columns and find mismatches ----------------------------------------
  results[["compare"]] <- .compare_columns(x, y, keys, compare)

  # return results with class --------------------------------------------------
  structure(results, class = c("ard_comparison", class(results)))
}


# Helper functions -------------------------------------------------------------

#' Process keys Argument
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param keys tidyselect expression for key columns
#'
#' @return character vector of key column names
#' @keywords internal
#' @noRd
.process_keys_arg <- function(x, y, keys) {
  keys <- intersect(
    cards_select({{ keys }}, data = x),
    cards_select({{ keys }}, data = y)
  )
  .check_not_empty(keys)
  cli::cli_inform("The comparison {.arg keys} are {.val {keys}}.")
  keys
}

#' Process compare Argument
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param compare tidyselect expression for columns to compare
#'
#' @return character vector of column names to compare
#' @keywords internal
#' @noRd
.process_compare_arg <- function(x, y, compare) {
  compare <- union(
    cards_select({{ compare }}, data = x),
    cards_select({{ compare }}, data = y)
  )
  .check_not_empty(compare)
  cli::cli_inform("The comparison {.arg compare} columns are {.val {compare}}.")
  compare
}

#' Check Argument is Not Empty
#'
#' @param x object to check
#' @param arg_name name of argument for error message
#'
#' @return invisible x
#' @keywords internal
#' @noRd
.check_not_empty <- function(x, arg_name = rlang::caller_arg(x)) {
  if (rlang::is_empty(x)) {
    cli::cli_abort(
      "The {.arg {arg_name}} argument cannot be empty.",
      call = get_cli_abort_call()
    )
  }
  invisible(x)
}

#' Check Keys Uniquely Identify Rows
#'
#' @param data data frame to check
#' @param keys character vector of key column names
#' @param arg_name name of argument for error message
#'
#' @return invisible NULL
#' @keywords internal
#' @noRd
.check_keys_unique <- function(data, keys, arg_name) {
  if (anyDuplicated(dplyr::select(data, dplyr::all_of(keys))) > 0) {
    duplicated_keys <- .format_duplicate_keys(data, keys)

    cli::cli_abort(
      c(
        "!" = "Duplicate key combinations detected in {.arg {arg_name}}.",
        "i" = "Key columns: {.val {keys}}.",
        duplicated_keys
      ),
      call = get_cli_abort_call()
    )
  }

  invisible(NULL)
}

#' Format Duplicate Keys for Error Message
#'
#' @param data data frame
#' @param keys character vector of key column names
#'
#' @return character vector of formatted duplicate key descriptions
#' @keywords internal
#' @noRd
.format_duplicate_keys <- function(data, keys, limit = 5L) {
  key_data <- dplyr::select(data, dplyr::all_of(keys))
  duplicated_rows <- duplicated(key_data) | duplicated(key_data, fromLast = TRUE)

  if (!any(duplicated_rows)) {
    return(character())
  }

  unique_duplicates <- utils::head(unique(key_data[duplicated_rows, , drop = FALSE]), limit)

  vapply(
    seq_len(nrow(unique_duplicates)),
    function(row_index) {
      row <- unique_duplicates[row_index, , drop = FALSE]
      formatted <- vapply(
        names(row),
        function(column) {
          value <- row[[column]]
          paste0(column, " = ", .format_key_value(value))
        },
        character(1)
      )
      paste(formatted, collapse = ", ")
    },
    character(1)
  ) |>
    paste0("- ", x = _)
}

#' Format a Single Key Value
#'
#' @param value value to format
#'
#' @return formatted string
#' @keywords internal
#' @noRd
.format_key_value <- function(value) {
  value <- value[[1]]

  if (is.factor(value)) {
    value <- as.character(value)
  }

  if (is.character(value)) {
    if (is.na(value)) {
      return("NA")
    }
    return(encodeString(value, quote = "\""))
  }

  if (is.logical(value)) {
    if (is.na(value)) {
      return("NA")
    }
    return(if (value) "TRUE" else "FALSE")
  }

  if (is.numeric(value)) {
    if (is.na(value)) {
      return("NA")
    }
    return(format(value, trim = TRUE))
  }

  if (inherits(value, "Date") || inherits(value, "POSIXt")) {
    if (is.na(value)) {
      return("NA")
    }
    return(encodeString(as.character(value), quote = "\""))
  }

  value_chr <- as.character(value)
  if (length(value_chr) == 0 || is.na(value_chr)) {
    return("NA")
  }

  encodeString(value_chr, quote = "\"")
}

#' Compare Rows Between Two ARDs
#'
#' Returns rows present in x but not in y based on key columns.
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param keys character vector of key column names
#'
#' @return data frame of rows in x not in y
#' @keywords internal
#' @noRd
.compare_rows <- function(x, y, keys) {
  dplyr::anti_join(
    dplyr::select(x, dplyr::all_of(keys)),
    dplyr::select(y, dplyr::all_of(keys)),
    by = keys
  )
}

#' Compare Columns Between Two ARDs
#'
#' Loops through columns to compare and returns a named list of data frames
#' where each data frame contains rows that are not equal between x and y.
#'
#' @param x (`card`)\cr first ARD
#' @param y (`card`)\cr second ARD
#' @param keys character vector of key column names
#' @param compare character vector of column names to compare
#'
#' @return named list of data frames with mismatched rows
#' @keywords internal
#' @noRd
.compare_columns <- function(x, y, keys, compare) {
  # select relevant columns
  x_selected <- dplyr::select(x, dplyr::all_of(keys), dplyr::any_of(compare))
  y_selected <- dplyr::select(y, dplyr::all_of(keys), dplyr::any_of(compare))

  # ensure all compare columns exist in both data frames
  for (column in compare) {
    if (!column %in% names(x_selected)) {
      x_selected[[column]] <- vector("list", nrow(x_selected))
    }
    if (!column %in% names(y_selected)) {
      y_selected[[column]] <- vector("list", nrow(y_selected))
    }
  }

  # perform inner join to compare only matching rows
  comparison <- dplyr::inner_join(
    x_selected,
    y_selected,
    by = keys,
    suffix = c(".x", ".y")
  )

  # build mismatch data frame for each compare column
  lapply(
    stats::setNames(compare, compare),
    function(column) {
      column_x <- paste0(column, ".x")
      column_y <- paste0(column, ".y")

      # find rows where values differ
      matches <- mapply(
        identical,
        comparison[[column_x]],
        comparison[[column_y]],
        SIMPLIFY = TRUE,
        USE.NAMES = FALSE
      )

      mismatches <- comparison[!matches, , drop = FALSE]

      dplyr::select(
        mismatches,
        dplyr::all_of(c(keys, column_x, column_y))
      )
    }
  )
}
