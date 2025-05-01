#' Phi Coefficient Between Binary Variables
#'
#' `phi_cor()` computes the phi correlation coefficient (equivalent to Pearson’s
#' *r* on 0/1–coded data) between every pair of dichotomous (binary) variables
#' selected from a data frame. Optionally, the results can be displayed as a
#' heatmap.
#'
#' @param df A data frame or tibble.
#' @param rows Optional unquoted tidy‐select specification of columns to use as
#'   the *y* variables.  Only columns with exactly two distinct non‐missing
#'   values are retained.  If `NULL` (default), all such binary variables are
#'   used.
#' @param cols Optional unquoted tidy‐select specification of columns to use as
#'   the *x* variables.  Only binary columns are retained.  If `NULL`, all
#'   binary variables are used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the computed phi coefficients before returning the data
#'   frame.
#'
#' @details
#' - **Applicable pairs:** binary–binary only (both variables must have exactly
#'   two levels).
#' - Each binary factor is internally recoded to 0/1 via `as.integer(factor) - 1`.
#' - Phi is computed as the Pearson correlation of these 0/1 codes:
#'   \deqn{\phi = \frac{n_{11}n_{00} - n_{10}n_{01}}{\sqrt{n_{1\cdot}n_{0\cdot}n_{\cdot1}n_{\cdot0}}}}
#' - **Assumptions:**
#'   1. Observations are independent.
#'   2. Variables are truly dichotomous (no more than two levels).
#'   3. Adequate cell counts to avoid zero‐division (all four contingency cells
#'      should have at least one observation for stable estimates).
#' - Missing values are handled pairwise: each coefficient is computed on the
#'   subset of rows where both variables are non‐missing.
#'
#' @return A data frame whose row names correspond to the selected `rows`
#'   variables and whose column names correspond to the selected `cols`
#'   variables; each entry is the phi correlation coefficient.
#'
#' @export
phi_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE) {
  rows_q <- rlang::enquo(rows)
  cols_q <- rlang::enquo(cols)

  df_rows <- select_non_numeric(df, !!rows_q) |>
    dplyr::select(dplyr::where(~ dplyr::n_distinct(.x, na.rm = TRUE) == 2))
  if (ncol(df_rows) == 0) {
    stop("No dichotomous variables found in 'rows'.", call. = FALSE)
  }

  df_cols <- select_non_numeric(df, !!cols_q) |>
    dplyr::select(dplyr::where(~ dplyr::n_distinct(.x, na.rm = TRUE) == 2))
  if (ncol(df_cols) == 0) {
    stop("No dichotomous variables found in 'cols'.", call. = FALSE)
  }

  nr <- ncol(df_rows); nc <- ncol(df_cols)
  cor_mat <- matrix(
    NA_real_,
    nrow = nr,
    ncol = nc,
    dimnames = list(colnames(df_rows), colnames(df_cols))
  )

  # compute phi = Pearson on 0/1 coding
  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      x_fac <- df_rows[[i]]
      y_fac <- df_cols[[j]]
      x_num <- as.integer(factor(x_fac)) - 1L
      y_num <- as.integer(factor(y_fac)) - 1L
      cor_mat[i, j] <- stats::cor(x_num, y_num, use = "pairwise.complete.obs")
    }
  }

  cor_df <- as.data.frame(cor_mat)

  if (heatmap) {
    heatmap_cor(cor_df, title = "Phi correlation heatmap")
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
# cor1 = phi_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = phi_cor(tea)
# cor2
# cor3 = phi_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = phi_cor(df1, heatmap=T)
# cor5 = phi_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = phi_cor(dplyr::sample_n(diamonds, size=1000), heatmap=T)
# cor7 = phi_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
