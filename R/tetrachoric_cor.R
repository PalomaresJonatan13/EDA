#' Tetrachoric Correlation for Dichotomous Variables
#'
#' `tetrachoric_cor()` computes the tetrachoric correlation coefficient—a measure
#' of the latent-continuous association—between every pair of dichotomous
#' (binary) variables selected from a data frame.  Optionally, a heatmap of the
#' resulting correlations can be displayed.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Unquoted tidy-select specification of columns to use as the *y*
#'   variables.  Only variables with exactly two distinct non-missing values
#'   are retained.  If `NULL` (default), all such dichotomous variables are used.
#' @param cols Unquoted tidy-select specification of columns to use as the *x*
#'   variables.  Only dichotomous variables are retained.  If `NULL`, all
#'   dichotomous variables are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the computed tetrachoric correlations before returning
#'   the data frame.
#'
#' @details
#' - **Applicable pairs:** dichotomous–dichotomous (both variables must have
#'   exactly two levels).
#' - Estimates the correlation between two unobserved continuous latent
#'   variables underlying the observed binary outcomes, using
#'   `psych::tetrachoric()`.
#' - **Assumptions:**
#'   1. Observations are independent.
#'   2. Each binary variable represents a dichotomization of an underlying
#'      bivariate normal distribution.
#'   3. Sufficient contingency cell counts; extremely sparse tables may lead
#'      to unstable estimates.
#' - Missing values are handled pairwise: each correlation is computed on the
#'   subset of rows where both variables are non-missing.
#'
#' @return A data frame whose row names correspond to the selected `rows`
#'   variables and whose column names correspond to the selected `cols`
#'   variables; each entry is the tetrachoric correlation coefficient
#'   (the `rho` element from `psych::tetrachoric()`).
#'
#' @export
tetrachoric_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  rows_q <- rlang::enquo(rows)
  cols_q <- rlang::enquo(cols)

  df_rows <- select_non_numeric(df, !!rows_q)
  df_rows <- dplyr::select(df_rows, dplyr::where(~ dplyr::n_distinct(.x, na.rm = TRUE) == 2))
  if (ncol(df_rows) == 0) {
    stop("No dichotomous variables found in 'rows'.", call. = FALSE)
  }

  df_cols <- select_non_numeric(df, !!cols_q)
  df_cols <- dplyr::select(df_cols, dplyr::where(~ dplyr::n_distinct(.x, na.rm = TRUE) == 2))
  if (ncol(df_cols) == 0) {
    stop("No dichotomous variables found in 'cols'.", call. = FALSE)
  }

  nr <- ncol(df_rows); nc <- ncol(df_cols)
  cor_mat <- matrix(NA_real_, nrow = nr, ncol = nc,
                    dimnames = list(colnames(df_rows), colnames(df_cols)))
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      tab <- table(df_rows[[i]], df_cols[[j]], useNA = "no")
      cor_mat[i, j] <- psych::tetrachoric(tab)$rho
    }
  }
  cor_df <- as.data.frame(cor_mat)

  if (heatmap) {
    heatmap_cor(cor_df, title = "Tetrachoric correlation heatmap")
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
# cor1 = tetrachoric_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = tetrachoric_cor(tea)
# cor2
# cor3 = tetrachoric_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = tetrachoric_cor(df1, heatmap=T)
# cor5 = tetrachoric_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = tetrachoric_cor(dplyr::sample_n(diamonds, size=1000), heatmap=T)
# cor7 = tetrachoric_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
