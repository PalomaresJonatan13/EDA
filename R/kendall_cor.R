#' Kendall’s Tau Correlation Matrix Between Two Sets of Variables
#'
#' `kendall_cor()` computes Kendall’s τ rank correlation coefficients between
#' every pair of selected variables from two subsets of a data frame, and
#' optionally displays a heatmap of the results.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Optional unquoted tidy-select specification of columns to use as
#'   the *y* variables.  Columns must be numeric or ordered factors.  If `NULL`
#'   (default), all numeric columns are used.
#' @param cols Optional unquoted tidy-select specification of columns to use as
#'   the *x* variables.  Columns must be numeric or ordered factors.  If `NULL`,
#'   all numeric columns are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the computed correlations before returning the data frame.
#'
#' @details
#' - **Applicable pairs** include:
#'   - continuous–continuous (numeric, numeric)
#'   - continuous–ordinal   (numeric, ordered factor)
#'   - dichotomous–ordinal  (binary factor, ordered factor)
#'   - ordinal–ordinal      (ordered factor, ordered factor)
#' - Ordinal inputs must be provided as **ordered factors**; otherwise they
#'   will be ignored.
#' - Kendall’s τ assesses the strength of a *monotonic* relationship by counting
#'   concordant and discordant pairs.
#' - **Assumptions:**
#'   1. Observations are independent.
#'   2. Variables are at least ordinal.
#'   3. No requirement of linearity or normality, making τ robust to
#'      outliers and non-linear but monotonic associations.
#' - Missing values are handled *pairwise* (`use = "pairwise.complete.obs"`).
#'
#' @return A data frame whose rows correspond to the selected `rows` variables
#'   and whose columns correspond to the selected `cols` variables; each entry
#'   is Kendall’s τ correlation coefficient.
#'
#' @export
kendall_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  df_rows <- select_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_numeric(df, !!rlang::enquo(cols))

  cor_df <- stats::cor(
    as.matrix(df_rows),
    as.matrix(df_cols),
    method = "kendall",
    use    = "pairwise.complete.obs"
  ) |>
    as.data.frame()

  if (heatmap) {
    heatmap_cor(cor_df, title = "Kendall correlation heatmap")
  }

  cor_df
}


# TESTS
# level_list = list(slimming=c("No.slimming", "slimming"),
#                   where=c("chain store", "tea shop", "chain store+tea shop"))
# df1 = tea |>
#   select(slimming, where) |>
#   mutate(across(
#     all_of(names(level_list)),               # select only those columns
#     ~ factor(.x, levels = level_list[[dplyr::cur_column()]]) |>
#       as.integer(),
#     .names = "{.col}"                        # keep same names
#   ))
# cor1 = kendall_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = kendall_cor(tea)
# cor2
# cor3 = kendall_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = kendall_cor(df1, heatmap=T)
# cor5 = kendall_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = kendall_cor(dplyr::sample_n(diamonds, size=1000), heatmap=T)
# cor7 = kendall_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
