#' Biweight Midcorrelation Between Continuous Variables
#'
#' `biweight_cor()` computes the biweight midcorrelation—a robust measure of
#' association less sensitive to outliers—between every pair of selected numeric
#' variables from two subsets of a data frame.  Optionally, a heatmap of the
#' resulting correlations can be displayed.
#'
#' @param df A data frame or tibble containing the variables.
#' @param rows Optional unquoted tidy-select specification of numeric columns
#'   to use as the *y* variables.  If `NULL` (default), all numeric columns in
#'   `df` are used.
#' @param cols Optional unquoted tidy-select specification of numeric columns
#'   to use as the *x* variables.  If `NULL`, all numeric columns in `df` are
#'   used.
#' @param heatmap Logical; if `TRUE`, calls `heatmap_cor()` to display a tile-
#'   based heatmap of the biweight midcorrelations before returning.
#' @param cval Numeric tuning constant for Tukey’s biweight function (default = 9);
#'   larger values down-weight outliers less aggressively.
#'
#' @details
#' - **Applicable pairs:** continuous–continuous (numeric, numeric) only.
#' - Requires at least three non-missing paired observations per variable pair.
#' - Centers each variable on its median and scales by its MAD (`stats::mad()`).
#'   Observations with standardized distance ≥ 1 are down-weighted to zero.
#' - Computes weighted covariance and variances to form the midcorrelation.
#' - **Outlier Robustness:** Highly robust to extreme values compared to Pearson’s *r*.
#' - Missing values are handled pairwise: each pair is correlated on the
#'   subset of observations where both variables are non-missing.
#'
#' @return A data frame whose rows correspond to the selected `rows` variables
#'   and whose columns correspond to the selected `cols` variables; each entry
#'   is the biweight midcorrelation coefficient.
#'
#' @export
biweight_cor <- function(df, rows = NULL, cols = NULL, heatmap = FALSE, cval = 9) {
  df_rows <- select_numeric(df, !!rlang::enquo(rows))
  df_cols <- select_numeric(df, !!rlang::enquo(cols))

  bicor_pair <- function(x, y, cval) {
    # pairwise drop NA
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]; y <- y[ok]
    if (length(x) < 3) return(NA_real_)
    # center
    x0 <- x - stats::median(x)
    y0 <- y - stats::median(y)
    # robust scale
    madx <- stats::mad(x0, constant = 1)
    mady <- stats::mad(y0, constant = 1)
    if (madx == 0 || mady == 0) return(NA_real_)
    # compute weights
    tx <- x0 / (cval * madx)
    ty <- y0 / (cval * mady)
    wx <- (1 - tx^2)^2; wx[abs(tx) >= 1] <- 0
    wy <- (1 - ty^2)^2; wy[abs(ty) >= 1] <- 0
    # weighted covariance & variances
    xw <- x0 * wx; yw <- y0 * wy
    num <- sum(xw * yw)
    den <- sqrt(sum(xw^2) * sum(yw^2))
    if (den == 0) return(NA_real_)
    num / den
  }


  mat_rows <- as.matrix(df_rows)
  mat_cols <- as.matrix(df_cols)
  nr <- ncol(mat_rows); nc <- ncol(mat_cols)
  cor_mat <- matrix(NA_real_, nrow = nr, ncol = nc,
                    dimnames = list(colnames(df_rows), colnames(df_cols)))

  for (i in seq_len(nr)) {
    for (j in seq_len(nc)) {
      cor_mat[i, j] <- bicor_pair(mat_rows[, i], mat_cols[, j], cval)
    }
  }
  cor_df <- as.data.frame(cor_mat)

  if (heatmap) {
    heatmap_cor(cor_df, title = "Biweight midcorrelation heatmap")
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
# cor1 = biweight_cor(mtcars, rows=c(mpg, cyl, disp, drat), cols=c(qsec, vs, gear, cyl, carb), heatmap=T)
# cor2 = biweight_cor(tea)
# cor2
# cor3 = biweight_cor(
#   iris,
#   rows    = c(Sepal.Length, Sepal.Width),
#   heatmap = TRUE
# )
# cor4 = biweight_cor(df1, heatmap=T)
# cor5 = biweight_cor(tea, rows=c(breakfast, evening, sugar, SPC), cols=c(tea.time, friends, pub, where, age_Q, evening), heatmap=T)
# cor6 = biweight_cor(diamonds, heatmap=T)
# cor7 = biweight_cor(iris, cols=c(dplyr::starts_with("S")), heatmap=T)
# cor7
