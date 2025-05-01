#' Boxplots by Categorical Levels with Summary Statistics
#'
#' `level_boxplots()` creates boxplots of a numeric variable stratified by each
#' level of a categorical variable.  All level‐specific boxplots share one
#' combined display, and individual boxplots plus descriptive statistics are
#' returned invisibly.
#'
#' @param df A data frame or tibble.
#' @param num_var Unquoted name of a numeric column in `df` to plot.
#' @param cat_var Unquoted name of a non-numeric (categorical) column in `df`
#'   used to define levels.
#' @param horizontal Logical; if `FALSE` (default), boxplots are vertical
#'   (`x = level, y = value`); if `TRUE`, they are horizontal
#'   (`x = value, y = level`).
#'
#' @return Invisibly returns a list with elements:
#' \describe{
#'   \item{boxplots}{A named list of `ggplot` boxplot objects, one per level.}
#'   \item{stats}{A tibble of descriptive statistics (mean, sd, etc.) for each
#'     level, with a `category` column.}
#' }
#'
#' @details
#' - Requires that `num_var` selects a numeric column and `cat_var` selects a
#'   non-numeric (factor or character) column, otherwise an error is raised.
#' - The combined plot shows boxplots for all levels in one panel.
#' - Summary statistics are computed via `describe_data()` and include only
#'   the numeric summary table.
#'
#' @examples
#' # Vertical boxplots of Sepal.Length by Species in iris
#' level_boxplots(iris, Sepal.Length, Species)
#'
#' # Horizontal boxplots of Petal.Width by Species
#' level_boxplots(iris, Petal.Width, Species, horizontal = TRUE)
#'
#' @export
level_boxplots <- function(df, num_var, cat_var, horizontal = FALSE) {
  num_q <- rlang::enquo(num_var)
  cat_q <- rlang::enquo(cat_var)

  num_vec <- dplyr::pull(select_numeric(df, !!num_q,
                           stop_msg="`num_var` must be numeric"), !!num_q)
  cat_vec <- dplyr::pull(select_non_numeric(df, !!cat_q,
                               stop_msg="`cat_var` must be non-numeric"), !!cat_q)

  cat_vec <- as.factor(cat_vec)
  df_plot <- tibble::tibble(value = num_vec, level = cat_vec)

  p_combined <- ggplot2::ggplot(df_plot) +
    { if (horizontal)
      ggplot2::geom_boxplot(
        ggplot2::aes(x = value, y = level),
        outlier.colour = "#800", fill = "#ddd"
      )
      else
        ggplot2::geom_boxplot(
          ggplot2::aes(x = level, y = value),
          outlier.colour = "#800", fill = "#ddd"
        )
    } +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank()
    )

  print(p_combined)

  # ── stats and individual boxplots
  lvl_names <- levels(cat_vec)
  box_list  <- list()
  stat_list <- list()

  for (lev in lvl_names) {
    df_sub <- dplyr::filter(df_plot, level == lev)

    g <- ggplot2::ggplot(df_sub) +
      { if (horizontal)
        ggplot2::geom_boxplot(
          ggplot2::aes(x = value, y = level),
          outlier.colour = "#800", fill = "#ddd"
        )
        else
          ggplot2::geom_boxplot(
            ggplot2::aes(x = level, y = value),
            outlier.colour = "#800", fill = "#ddd"
          )
      } +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_blank(),
        axis.title.y = ggplot2::element_blank()
      )

    box_list[[lev]] <- g

    desc <- describe_data(df_sub, value)$summary_numeric
    stat_list[[lev]] <- desc
  }

  # ── join stats' tibbles
  stats_tbl <- purrr::imap_dfr(
    stat_list,
    ~ dplyr::mutate(.x, category = .y) # BEFORE: level = .y
  ) |>
    dplyr::select(category, dplyr::everything(), -variable)

  invisible(list(boxplots = box_list, stats = stats_tbl))
}


# TESTS
# box1 = level_boxplots(tea, age, where)
# box1$boxplots$`chain store+tea shop`
# box1$stats
# level_boxplots(tea, slimming, where)
# level_boxplots(tea, age, age)
# box2 = level_boxplots(diamonds, price, cut, horizontal=T)
# box2$boxplots$`Very Good`
# box2$stats
# level_boxplots(tea)
