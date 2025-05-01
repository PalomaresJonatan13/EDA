#' Contingency and Joint Relative Frequency Tables
#'
#' `contingency_table()` computes both the absolute counts and the joint
#' relative frequencies for two categorical (non-numeric) variables in a data
#' frame.
#'
#' @param df A data frame or tibble.
#' @param var1 Unquoted name of a non-numeric (categorical) column in `df`.
#' @param var2 Unquoted name of a non-numeric (categorical) column in `df`.
#'
#' @return A named list with two elements:
#' \describe{
#'   \item{absolute}{A data frame of absolute counts (contingency table) with
#'     row names for `var1` levels and column names for `var2` levels.}
#'   \item{relative}{A data frame of joint relative frequencies (summing to 1)
#'     with the same row and column structure.}
#' }
#'
#' @details
#' * Both `var1` and `var2` are first validated and extracted as non-numeric
#'   vectors via `select_non_numeric()`, and an error is raised if they are
#'   not categorical.
#' * `NA` values are included as a separate level in the contingency table.
#' * Joint relative frequencies are computed with `stats::prop.table()` on the
#'   full contingency table.
#'
#' @examples
#' # Using the built-in iris dataset (Species only has categories)
#' contingency_table(iris, Species, Species)
#'
#' # Custom example
#' df <- data.frame(
#'   A = sample(c("x", "y", NA), 20, TRUE),
#'   B = sample(c("u", "v"),       20, TRUE),
#'   C = rnorm(20)
#' )
#' contingency_table(df, A, B)
#'
#' @export
contingency_table <- function(df, var1, var2) {
  var1_q <- rlang::enquo(var1)
  var2_q <- rlang::enquo(var2)
  v1 <- dplyr::pull(select_non_numeric(df, !!var1_q,
                           stop_msg="`var1` must be non-numeric"), !!var1_q)
  v2 <- dplyr::pull(select_non_numeric(df, !!var2_q,
                               stop_msg="`var2` must be non-numeric"), !!var2_q)

  abs_tbl <- table(v1, v2, useNA = "ifany")
  abs_df  <- as.data.frame.matrix(abs_tbl)

  rel_tbl <- prop.table(abs_tbl)
  rel_df  <- as.data.frame.matrix(rel_tbl)

  list(absolute = abs_df, relative = rel_df)
}


# TESTS
# tab1 = contingency_table(tea, slimming, where)
# tab1$absolute
# tab1$relative
# contingency_table(tea, age, where)
# contingency_table(tea, where, age)
