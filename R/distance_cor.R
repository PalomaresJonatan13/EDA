#' Distance Correlation Between Continuous Variables
#'
#' `distance_cor()` computes the distance correlation—a non‐parametric measure
#' of association that detects both linear and non‐linear dependence—between
#' every pair of selected numeric variables from two subsets of a data frame.
#' Optionally, it can display a heatmap of the resulting correlations.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Optional unquoted tidy‐select specification of numeric columns
#'   to use as the *y* variables. If `NULL` (default), all numeric columns in
#'   `df` are used.
#' @param cols Optional unquoted tidy‐select specification of numeric columns
#'   to use as the *x* variables. If `NULL`, all numeric columns in `df` are
#'   used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile‐
#'   based heatmap of the computed distance correlations (with limits from 0
#'   to 1) before returning the data frame.
#'
#' @details
#' - **Applicable pairs:** continuous–continuous (numeric, numeric) only.
#' - Distance correlation (`energy::dcor`) measures any kind of dependence,
#'   not just linear association.
#' - Values range from 0 (independence) to 1 (perfect dependence).
#' - Missing values are implicitly handled by computing each pair on their
#'   non‐missing observations.
#'
#' @return A data frame whose rows correspond to the selected `rows`
#'   variables and whose columns correspond to the selected `cols`
#'   variables; each entry is the distance correlation coefficient.
#'
#' @export
distance_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  df_rows <- select_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_numeric(df, !!rlang::enquo(cols))

  mat_rows <- as.matrix(df_rows)
  mat_cols <- as.matrix(df_cols)
  nr <- ncol(mat_rows); nc <- ncol(mat_cols)
  cor_mat <- matrix(
    NA_real_,
    nrow = nr,
    ncol = nc,
    dimnames = list(colnames(df_rows), colnames(df_cols))
  )

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cor_mat[i, j] <- energy::dcor(mat_rows[, i], mat_cols[, j])
    }
  }
  cor_df <- as.data.frame(cor_mat)

  if (heatmap) {
    heatmap_cor(cor_df, title = "Distance correlation heatmap", limits=c(0, 1))
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

