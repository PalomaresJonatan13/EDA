#' Pairwise Scatter Plots with Optional Grouping
#'
#' `scatter()` creates a grid of scatter plots for every combination of two
#' subsets of numeric variables, optionally colored by a categorical variable.
#' It prints a combined faceted plot (rows × columns) and invisibly returns a
#' data frame of the individual `ggplot` objects, indexed by row and column
#' variable names.
#'
#' @param df A data frame or tibble containing the data.
#' @param rows Optional unquoted tidy-select specification of numeric columns
#'   to use as the *y* variables.  Defaults to all numeric columns if `NULL`.
#' @param cols Optional unquoted tidy-select specification of numeric columns
#'   to use as the *x* variables.  Defaults to all numeric columns if `NULL`.
#' @param cat_var Optional unquoted name of a non-numeric (categorical)
#'   column in `df`.  If provided, points are colored by this grouping;
#'   otherwise all points are drawn in a single color.
#'
#' @details
#' - Numeric selections for `rows` and `cols` are handled via
#'   `select_numeric()`.
#' - The combined plot uses `ggplot2::facet_grid(row_v ~ col_v, scales = "free")`
#'   so each scatter panel has its own axis ranges.
#' - A light gray panel border is drawn around each facet.
#' - If `cat_var` is supplied, a legend is shown; otherwise the color guide is
#'   suppressed.
#'
#' @return Invisibly returns a data frame of class `data.frame` whose
#'   row names are the `rows` variables and column names are the `cols`
#'   variables; each cell contains a single-element list with the corresponding
#'   `ggplot` scatter object.
#'
#' @examples
#' # All numeric vs itself in mtcars
#' plots <- scatter(mtcars)
#'
#' # Sepal vs Petal dimensions in iris, colored by Species
#' plots2 <- scatter(
#'   iris,
#'   rows    = c(Sepal.Length, Petal.Length),
#'   cols    = c(Sepal.Width,  Petal.Width),
#'   cat_var = Species
#' )
#'
#' # Access the plot for Sepal.Length (row) vs Sepal.Width (col)
#' plots2["Sepal.Length", "Sepal.Width"]
#'
#' @export
scatter <- function(df, rows = NULL, cols = NULL, cat_var = NULL) {
  rows_q <- rlang::enquo(rows)
  cols_q <- rlang::enquo(cols)
  cat_q <- rlang::enquo(cat_var)

  df_rows <- select_numeric(df, !!rows_q)
  df_cols <- select_numeric(df, !!cols_q)

  has_cat <- rlang::as_label(cat_q) != "NULL"
  if (has_cat) {
    cat_vec <- dplyr::pull(select_non_numeric(df, !!cat_q,
                               stop_msg="`cat_var` must be non-numeric"), !!cat_q)
    cat_vec <- factor(cat_vec)
  } else {
    cat_vec <- factor(rep("all", nrow(df)))
  }

  long_df <- purrr::imap_dfr(
    df_cols, function(x, xc) {
      purrr::imap_dfr(
        df_rows, function(y, yr) {
          tibble::tibble(
            x     = x,
            y     = y,
            col_v = xc,
            row_v = yr,
            colour = cat_vec
          )
        })
    })

  # combined faceted scatterplot
  p_combined <- ggplot2::ggplot(long_df) +
    ggplot2::geom_point(
      ggplot2::aes(x = x, y = y,
                   colour = if (has_cat) colour else NULL),
      alpha = 0.6, size = 1.2
    ) +
    ggplot2::facet_grid(row_v ~ col_v, scales = "free") +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text  = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(color="gray", fill=NA, linewidth=0.5)
    )

  if (has_cat) {
    p_combined <- p_combined + ggplot2::labs(colour = rlang::as_label(cat_q))
  } else {
    p_combined <- p_combined + ggplot2::guides(colour = "none")
  }

  print(p_combined)

  # ---- individual scatter plots in a matrix-like data frame
  row_names <- colnames(df_rows)
  col_names <- colnames(df_cols)

  plots_mat <- matrix(list(), nrow = length(row_names), ncol = length(col_names),
                      dimnames = list(row_names, col_names))
  for (i in seq_along(row_names)) {
    yr <- row_names[i]
    for (j in seq_along(col_names)) {
      xc <- col_names[j]
      df_pair <- tibble::tibble(
        x      = df_cols[[xc]],
        y      = df_rows[[yr]],
        colour = cat_vec
      )
      g <- ggplot2::ggplot(df_pair) +
        ggplot2::geom_point(
          ggplot2::aes(x = x, y = y,
                       colour = if (has_cat) colour else NULL),
          alpha = 0.6, size = 1.2
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x = xc, y = yr, colour = rlang::as_label(cat_q))
      plots_mat[i, j] <- list(g)
    }
  }

  plots_df <- as.data.frame(plots_mat, stringsAsFactors = FALSE)

  invisible(plots_df)
}

# TESTS
# scatter1 = scatter(dplyr::sample_n(ggplot2::diamonds, size=1000), rows = c(x, y, z), cols=c(carat, price, depth, table))
# scatter1["x", "carat"]
#
# cars <- dplyr::mutate(mtcars, cyl=as.factor(cyl))
# scatter2 = scatter(cars, rows=c(drat, disp, hp))
# scatter2["disp", "hp"]
#
# scatter3 = scatter(dplyr::sample_n(ggplot2::diamonds, size=1000), rows = c(x, y, z), cols=c(carat, price, depth, table), cat_var=cut)
# scatter3["x", "carat"]
#
# scatter4 = scatter(cars, rows=c(drat, disp, hp), cat_var=cyl)
# scatter4["disp", "hp"]
#
# scatter(mtcars[, 1:4])["disp", "hp"]
