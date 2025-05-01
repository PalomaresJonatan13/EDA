#' Cramer's V Association Between Categorical Variables
#'
#' `cramer_v_cor()` computes Cramer's V—a measure of association between two
#' categorical variables—between every pair of selected variables from a data
#' frame, and optionally displays a heatmap of the results.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Unquoted tidy-select specification of non-numeric columns to
#'   use as the *y* variables.  Variables may be nominal factors, ordered
#'   factors, or character vectors.  If `NULL` (default), all non-numeric
#'   columns are used.
#' @param cols Unquoted tidy-select specification of non-numeric columns to
#'   use as the *x* variables.  Variables may be nominal factors, ordered
#'   factors, or character vectors.  If `NULL`, all non-numeric columns are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the computed Cramer's V values (ranging from 0 to 1)
#'   before returning the data frame.
#'
#' @details
#' - **Applicable pairs:**
#'   - dichotomous–nominal (binary vs. nominal)
#'   - ordinal–nominal (ordered factor vs. nominal)
#'   - nominal–nominal (two nominal variables)
#' - Computes Cramer's V via
#'   \deqn{V = \sqrt{\chi^2 / (n \, \min(r-1, c-1))}}
#'   where \(\chi^2\) is the Pearson chi-square statistic of the contingency
#'   table, \(n\) is the total sample size, and \(r, c\) are the numbers of
#'   rows and columns in the table.
#' - **Assumptions:**
#'   1. Observations are independent.
#'   2. Contingency tables are not too sparse; expected cell counts should be
#'      sufficiently large for the chi-square approximation to hold.
#'   3. Variables have at least two levels.
#' - Missing values are handled pairwise: each V is computed on the subset of
#'   rows where both variables are non-missing.
#'
#' @return A data frame whose row names correspond to the selected `rows`
#'   variables and whose column names correspond to the selected `cols`
#'   variables; each entry is the computed Cramer's V coefficient.
#'
#' @export
cramer_v_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  df_rows <- select_non_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_non_numeric(df, !!rlang::enquo(cols))

  nr <- ncol(df_rows)
  nc <- ncol(df_cols)
  cor_mat <- matrix(NA_real_, nrow = nr, ncol = nc,
                    dimnames = list(colnames(df_rows),
                                    colnames(df_cols)))

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      tab  <- table(df_rows[[i]], df_cols[[j]])
      chi2 <- suppressWarnings(stats::chisq.test(tab, correct = FALSE)$statistic)
      n    <- sum(tab)
      r    <- nrow(tab)
      c    <- ncol(tab)
      denom <- n * min(r - 1, c - 1)
      cor_mat[i, j] <- if (denom > 0) sqrt(as.numeric(chi2) / denom) else NA_real_
    }
  }

  cor_df <- as.data.frame(cor_mat)

  if (heatmap) {
    heatmap_cor(cor_df, title = "Cramer's V correlation heatmap", limits=c(0, 1))
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
# cor1 = cramer_v_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = cramer_v_cor(tea)
# cor2
# cor3 = cramer_v_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = cramer_v_cor(df1, heatmap=T)
# cor5 = cramer_v_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = cramer_v_cor(dplyr::sample_n(diamonds, size=1000), heatmap=T)
# cor7 = cramer_v_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
