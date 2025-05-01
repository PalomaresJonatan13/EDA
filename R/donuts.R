#' Donut Charts for Categorical Variables
#'
#' `donuts()` creates individual donut (ring-pie) charts for each selected
#' non-numeric (categorical) variable, arranges them in a grid via **patchwork**,
#' and returns the list of ggplot objects invisibly.
#'
#' @param df   A data frame or tibble.
#' @param vars Optional unquoted tidy-select specification of columns to plot
#'   (e.g. `c(a, b, starts_with("x"))`).  If `NULL` (default), all non-numeric
#'   columns in `df` are used.
#'
#' @return Invisibly returns a named list of `ggplot` objects (one per variable).
#'   The primary side effect is printing the combined grid of donut charts.
#'
#' @details
#' - Each donut shows percentage segments for each level (including `NA`),
#'   labeled with rounded percent values at the mid-arc position.
#' - Inner hole is created by setting `xlim(0.5, 2.5)` after a `coord_polar()`.
#' - Colours are drawn from the **RColorBrewer** `"Set1"` palette.
#' - Legends (level labels) appear to the right of each donut.
#' - Composite layout uses `patchwork::wrap_plots()` with
#'   `ncol = ceiling(sqrt(k))`, `nrow = ceiling(k / ncol)`, where `k` is the
#'   number of variables.
#'
#' @examples
#' # All categorical variables in iris (only Species here)
#' donuts(iris)
#'
#' # Specific variables in a custom data frame
#' df <- data.frame(
#'   gender  = sample(c("M", "F"), 50, TRUE),
#'   outcome = sample(c("yes", "no"), 50, TRUE),
#'   score   = rnorm(50)
#' )
#' donuts(df, c(gender, outcome))
#'
#' @export
donuts <- function(df, vars = NULL) {
  cats <- select_non_numeric(df, !!rlang::enquo(vars))

  build_donut <- function(vec, nm) {
    seg <- tibble::tibble(level = vec) |>
      dplyr::count(level, name = "n") |>
      dplyr::mutate(pct = 100 * n / sum(n))

    ggplot2::ggplot(seg, ggplot2::aes(x = 2, y = pct, fill = level)) +
      ggplot2::geom_col(width = 1, colour = "white") +
      ggplot2::coord_polar(theta = "y") +
      ggplot2::scale_fill_brewer(palette = "Set1") +
      ggplot2::geom_text(
        ggplot2::aes(label = paste0(round(pct), "%")),
        position = ggplot2::position_stack(vjust = 0.5),
        colour   = "black",
        size     = 3
      ) +
      ggplot2::xlim(0.5, 2.5) +                # inner hole
      ggplot2::theme_void() +
      ggplot2::theme(
        legend.position = "right",
        plot.title      = ggplot2::element_text(hjust = 0.5)
      ) +
      ggplot2::labs(title = nm, fill = NULL)
  }


  plot_list <- purrr::imap(cats, build_donut)
  names(plot_list) <- colnames(cats)


  k     <- length(plot_list)
  ncolg <- ceiling(sqrt(k))
  nrowg <- ceiling(k / ncolg)

  combined <- patchwork::wrap_plots(
    plotlist = plot_list,
    ncol     = ncolg,
    nrow     = nrowg
  )

  print(combined)             # display all donuts at once
  invisible(plot_list)
}


# TESTS
# donuts(mtcars)
# donuts(tea, vars=c(dplyr::starts_with("s")))
# donuts(tea, vars=c(dplyr::starts_with("a")))
