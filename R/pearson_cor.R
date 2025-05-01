#' Pearson Correlation Matrix Between Two Sets of Numeric Variables
#'
#' `pearson_cor()` computes the Pearson product–moment correlation coefficients
#' between every pair of selected numeric variables from two subsets of a data
#' frame.  It can also optionally display a heatmap of the results.
#'
#' @param df A data frame or tibble containing the variables.
#' @param cols Optional unquoted tidy-select specification of numeric columns
#'   to use as the *x* variables.  If `NULL` (default), all numeric columns are
#'   used.
#' @param rows Optional unquoted tidy-select specification of numeric columns
#'   to use as the *y* variables.  If `NULL`, all numeric columns are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the computed correlations before returning the data frame.
#'
#' @return A data frame whose row names correspond to the selected `rows`
#'   variables and whose column names correspond to the selected `cols`
#'   variables; each entry is the Pearson correlation coefficient.
#'
#' @details
#' - Only **numeric** columns are considered; any non-numeric columns in the
#'   selections are ignored.
#' - Correlations are computed with `stats::cor(..., method = "pearson",
#'   use = "pairwise.complete.obs")`, meaning that each pair of variables is
#'   correlated on the subset of observations without missing values for that
#'   pair.
#' - **Applicability:** Suitable for pairs of continuous variables where you
#'   wish to assess linear association.
#' - **Assumptions:** Assumes a roughly linear relationship between each pair,
#'   approximate bivariate normality for inference, and homoscedasticity of
#'   paired values.  Outliers can have a strong influence on Pearson's *r*.
#'
#' @export
pearson_cor <- function(df, cols = NULL, rows = NULL, heatmap = FALSE) {
  df_rows <- select_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_numeric(df, !!rlang::enquo(cols))

  cor_df <- stats::cor(
    as.matrix(df_rows),
    as.matrix(df_cols),
    method = "pearson",
    use    = "pairwise.complete.obs"
  ) |>
    as.data.frame()

  if (heatmap) {
    heatmap_cor(cor_df, title = "Pearson correlation heatmap")
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
# cor1 = pearson_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = pearson_cor(tea)
# cor2
# cor3 = pearson_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = pearson_cor(df1, heatmap=T)
# cor5 = pearson_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = pearson_cor(diamonds, heatmap=T)
# cor7 = pearson_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
