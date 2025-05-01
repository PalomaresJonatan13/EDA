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
heatmap_cor <- function(cor_df, title = "Correlation heatmap", limits=c(-1, 1)) {
  # melt into long format
  melted_df <- reshape2::melt(
    as.matrix(cor_df),
    varnames   = c("Var1", "Var2"),
    value.name = "correlation"
  )

  # ensure Var1 is a factor in the original order, then reverse for plotting
  melted_df$Var1 <- factor(melted_df$Var1, levels = rownames(cor_df))
  rev_levels     <- rev(levels(melted_df$Var1))

  p <- ggplot2::ggplot(
    melted_df,
    ggplot2::aes(x = Var2, y = Var1, fill = correlation)
  ) +
    ggplot2::geom_tile(color = "white") +
    ggplot2::scale_fill_gradient2(
      low      = "blue",
      mid      = "white",
      high     = "red",
      midpoint = 0,
      limits   = limits,
      name     = "Correlation"
    ) +
    ggplot2::geom_text(
      ggplot2::aes(label = round(correlation, 2)),
      size = 3
    ) +
    ggplot2::scale_y_discrete(limits = rev_levels) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
      axis.title  = ggplot2::element_blank()
    ) +
    ggplot2::ggtitle(title)

  print(p)
  invisible(NULL)
}
