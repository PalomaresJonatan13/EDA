#' Spearman Rank Correlation Matrix Between Two Sets of Variables
#'
#' `spearman_cor()` computes Spearman’s rank correlation coefficients between
#' every pair of selected variables from two subsets of a data frame, optionally
#' displaying a heatmap of the results.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Optional unquoted tidy-select specification of columns to use as
#'   the *y* variables.  Columns must be numeric or ordered factors (internally
#'   converted to ranks).  If `NULL` (default), all numeric columns are used.
#' @param cols Optional unquoted tidy-select specification of columns to use as
#'   the *x* variables.  Columns must be numeric or ordered factors.  If `NULL`,
#'   all numeric columns are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor(..., title =
#'   "Spearman correlation heatmap")` to display a tile-based heatmap of the
#'   computed correlations before returning.
#'
#' @return A data frame whose rows correspond to the selected `rows` variables
#'   and whose columns correspond to the selected `cols` variables; each entry
#'   is Spearman’s rank correlation coefficient (ρ).
#'
#' @details
#' - Spearman’s ρ measures the strength of a *monotonic* relationship between
#'   two variables by computing Pearson’s correlation on their ranks.
#' - Applicable variable pairs include:
#'   - continuous–continuous
#'   - continuous–ordinal
#'   - ordinal–ordinal
#'   - binary–ordinal, etc.
#' - Input columns must be numeric or ordered factors; unordered factors will
#'   be dropped.
#' - Missing values are handled *pairwise* (`use = "pairwise.complete.obs"`).
#' - More robust than Pearson’s *r* for non-linear but monotonic associations
#'   and less sensitive to outliers.
#'
#' @export
spearman_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  df_rows <- select_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_numeric(df, !!rlang::enquo(cols))

  cor_df <- stats::cor(
    as.matrix(df_rows),
    as.matrix(df_cols),
    method = "spearman",
    use    = "pairwise.complete.obs"
  ) |>
    as.data.frame()

  if (heatmap) {
    heatmap_cor(cor_df, title = "Spearman correlation heatmap")
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
# cor1 = spearman_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = spearman_cor(tea)
# cor2
# cor3 = spearman_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = spearman_cor(df1, heatmap=T)
# cor5 = spearman_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = spearman_cor(diamonds, heatmap=T)
# cor7 = spearman_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
