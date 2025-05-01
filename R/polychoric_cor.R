#' Polychoric Correlation Matrix for Ordinal Variables
#'
#' `polychoric_cor()` computes pairwise polychoric correlation coefficients
#' between two sets of ordinal variables in a data frame, optionally displaying
#' a heatmap of the results.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Unquoted tidy-select specification of columns to use as the *y*
#'   variables.  Variables must be ordinal (ordered factors) or factors
#'   representing ordered categories.  If `NULL`, all non-numeric columns are used.
#' @param cols Unquoted tidy-select specification of columns to use as the *x*
#'   variables.  Variables must be ordinal or factors.  If `NULL`, all
#'   non-numeric columns are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the computed correlations before returning.
#'
#' @details
#' - **Applicable pairs:** ordinal–ordinal only.
#' - Estimates the correlation between unobserved continuous latent variables
#'   underlying the observed ordinal outcomes via `polycor::polychor()`.
#' - **Assumptions:**
#'   1. The latent variables follow a bivariate normal distribution.
#'   2. Observations are independent.
#'   3. Each input variable is a true ordinal scale (ordered factor).
#'   4. Adequate category frequencies; extremely sparse categories may yield
#'      unstable estimates.
#' - Missing values are handled pairwise: each correlation is computed on the
#'   subset of rows where both variables are non-missing.
#'
#' @return A data frame whose row names correspond to the selected `rows`
#'   variables and whose column names correspond to the selected `cols`
#'   variables; each entry is the polychoric correlation coefficient.
#'
#' @export
polychoric_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  df_rows <- select_non_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_non_numeric(df, !!rlang::enquo(cols))

  nr <- ncol(df_rows); nc <- ncol(df_cols)
  cor_mat <- matrix(NA_real_, nrow = nr, ncol = nc,
                    dimnames = list(colnames(df_rows), colnames(df_cols)))
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cor_mat[i, j] <- polycor::polychor(df_rows[[i]], df_cols[[j]])
    }
  }
  cor_df <- as.data.frame(cor_mat)

  if (heatmap) {
    heatmap_cor(cor_df, title = "Polychoric correlation heatmap")
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
# cor1 = polychoric_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = polychoric_cor(tea)
# cor2
# cor3 = polychoric_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = polychoric_cor(df1, heatmap=T)
# cor5 = polychoric_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = polychoric_cor(dplyr::sample_n(diamonds, size=1000), heatmap=T)
# cor7 = polychoric_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
