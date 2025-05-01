#' Select non-numeric columns with optional allowance for empty result
#'
#' `select_non_numeric()` filters a data frame to retain only non-numeric
#' (e.g. character, factor, logical) columns, optionally pre-subsetting to a
#' set of variables. By default it will error if no non-numeric columns remain;
#' set `empty = TRUE` to return an empty data frame instead.
#'
#' @param df A data frame or tibble.
#' @param vars Unquoted tidy-select specification of columns to keep before
#'   non-numeric filtering (e.g. `c(a, b, starts_with("x"))`). If `NULL`, all
#'   columns are used. If the argument is omitted entirely, the function
#'   throws an error.
#' @param empty Logical; if `FALSE` (default), the function errors when zero
#'   non-numeric columns are found after filtering. If `TRUE`, it returns the
#'   (empty) data frame without error.
#' @param stop_msg Optional character string to override the default error
#'   message when no non-numeric columns remain.
#'
#' @return A data frame containing only the non-numeric columns from the
#'   (possibly subsetted) input. If no non-numeric columns are found and
#'   `empty = FALSE`, an error is thrown; if `empty = TRUE`, an empty data
#'   frame is returned.
#'
#' @details
#' * Captures `vars` with `rlang::enquo()` and applies it via
#'   `dplyr::select()` when provided.
#' * Uses `dplyr::select(df, -dplyr::where(is.numeric))` to filter out
#'   numeric columns.
#' * If `vars` is missing entirely, a `"Missing variables"` error is raised.
#' * Customize the error message for no-non-numeric-columns via `stop_msg`.
#'
#' @examples
#' # All non-numeric columns in iris
#' select_non_numeric(iris)
#'
#' # Pre-filter then select non-numeric: only Species
#' select_non_numeric(iris, c(Sepal.Length, Species))
#'
#' # Return empty data frame instead of error
#' select_non_numeric(mtcars, mpg, empty = TRUE)
#'
#' @export
select_non_numeric <- function(df, vars = NULL, empty = FALSE, stop_msg = NULL) {
  vars_q <- rlang::enquo(vars)

  df <- if (rlang::quo_is_null(vars_q)) {
    df
  } else {
    dplyr::select(df, !!vars_q)
  }

  df_non_num <- dplyr::select(df, -dplyr::where(is.numeric))
  if (rlang::quo_is_missing(vars_q)) {
    stop("Missing variables (possible argument values missing).", call. = FALSE)
  }
  if (ncol(df_non_num) == 0 && !empty) {
    msg <- {
      if (is.null(stop_msg)) "No variables selected or none are non-numeric."
      else stop_msg
    }
    stop(msg, call. = FALSE)
  }
  df_non_num
}
