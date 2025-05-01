#' Multiple Boxplots with Patchwork Layout
#'
#' `boxplots()` generates individual boxplots for each selected numeric variable,
#' arranges them in an approximately square grid using **patchwork**, and returns
#' the list of ggplot objects invisibly.
#'
#' @param df A data frame or tibble.
#' @param vars Optional unquoted tidy-select specification of numeric columns to
#'   plot (e.g. `c(a, b, starts_with("x"))`).  If `NULL` (default), all numeric
#'   columns in `df` are plotted.
#'
#' @return Invisibly returns a named list of `ggplot` objects, one per variable.
#'   The primary side effect is printing the combined grid of boxplots.
#'
#' @details
#' - Each subplot is a boxplot (no outlier labels) with a centered title (font
#'   size = 8).
#' - Subplots share no common axis beyond the vertical scale per facet.
#' - Composite layout uses `patchwork::wrap_plots()` with `ncol = ceiling(sqrt(k))`
#'   and `nrow = ceiling(k / ncol)`, where `k` is the number of variables.
#'
#' @examples
#' # All numeric variables in mtcars
#' plots <- boxplots(mtcars)
#'
#' # Subset of variables
#' plots2 <- boxplots(iris, vars = c(Sepal.Length, Sepal.Width))
#'
#' @export
boxplots <- function(df, vars = NULL) {
  df <- select_numeric(df, !!rlang::enquo(vars))
  k  <- ncol(df)

  # build individual boxplots
  plot_list <- purrr::imap(df, function(v, nm) {
    ggplot2::ggplot(data.frame(value = v), ggplot2::aes(x = "", y = value)) +
      ggplot2::geom_boxplot(outlier.colour = "#800", outlier.size = 1) +
      ggplot2::labs(title = nm, x = NULL, y = NULL) +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        plot.margin  = ggplot2::margin(5, 5, 5, 5, "pt"),
        axis.text.x  = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank(),
        plot.title   = ggplot2::element_text(
          size   = 8,      # smaller font size
          hjust  = 0.5
        )
      )
  })

  names(plot_list) <- colnames(df)

  # Optimal grid
  ncol <- ceiling(sqrt(k))
  nrow <- ceiling(k / ncol)

  # combine with patchwork
  combined <- patchwork::wrap_plots(plot_list, ncol = ncol, nrow = nrow) +
    patchwork::plot_annotation(theme = ggplot2::theme(
      plot.margin = ggplot2::margin(5, 5, 5, 5, "pt")
    ))

  print(combined)
  invisible(plot_list)
}

# TESTS
# boxplots(tea)
# boxplots(tea, vars=c(-age))
# cars_boxplots = boxplots(mtcars)
# cars_boxplots$drat
