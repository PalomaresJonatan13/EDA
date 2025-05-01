#' Split or Combined Violin Plots by Category (and Optional Binary Split)
#'
#' `violin()` produces violin plots of a numeric variable stratified by levels
#' of a categorical variable, optionally splitting each violin in half by a
#' binary variable.  A combined plot of all levels is printed, and individual
#' ggplot objects plus descriptive statistics are returned invisibly.
#'
#' @param df A data frame or tibble.
#' @param num_var Unquoted name of a numeric column in `df` to visualize.
#' @param cat_var Unquoted name of a non-numeric (categorical) column in `df`
#'   whose levels define separate violins.
#' @param dich_var Optional unquoted name of a second categorical column in
#'   `df` with exactly two levels.  If provided, each violin is split into left
#'   and right halves colored by the two binary levels.  If `NULL` (default),
#'   whole symmetric violins are drawn.
#' @param horizontal Logical; if `FALSE` (default), violins are vertical
#'   (`x = category, y = value`); if `TRUE`, obtains horizontal violins by
#'   flipping coordinates (`coord_flip()`).
#'
#' @details
#' - `num_var` is first checked via `select_numeric()` and must be numeric.
#' - `cat_var` is checked via `select_non_numeric()` and coerced to factor for
#'   level ordering.
#' - If `dich_var` is supplied, it must have exactly two distinct non-missing
#'   levels; its values are used to split each category’s violin into two
#'   halves via `gghalves::geom_half_violin()`, with colors `"#111"` (first
#'   level) and `"#ccc"` (second level).
#' - If `horizontal = TRUE`, `coord_flip()` is applied to both combined and
#'   individual plots.
#' - A combined violin plot across all levels is printed.
#' - Individual violins for each level (and binary half when present) are
#'   stored in the returned list element `violins`.
#' - Descriptive statistics (mean, sd, quartiles, etc.) for each level (and
#'   for each binary subgroup when `dich_var` is set) are computed via
#'   `describe_data()` and returned as a tibble in the list element `stats`.
#'
#' @return Invisibly returns a list with two elements:
#'   \describe{
#'     \item{violins}{Named list of `ggplot` violin objects, one per level of
#'       `cat_var`.}
#'     \item{stats}{A tibble of descriptive statistics (from
#'       `describe_data()`) for each level of `cat_var` (and each binary level
#'       if `dich_var` is specified), with columns `category`, `binary`
#'       (when applicable), and the numeric summary metrics.}
#'   }
#'
#' @examples
#' # Vertical violins of Sepal.Length by Species
#' violin(iris, Sepal.Length, Species)
#'
#' # Horizontal violins
#' violin(iris, Petal.Width, Species, horizontal = TRUE)
#'
#' # Split violins of mpg by cyl and am in mtcars
#' mtcars$am <- factor(mtcars$am, labels = c("Auto", "Manual"))
#' violin(mtcars, mpg, cyl, dich_var = am)
#'
#' @export
violin <- function(df, num_var, cat_var, dich_var = NULL, horizontal = FALSE) {
  num_q  <- rlang::enquo(num_var)
  cat_q  <- rlang::enquo(cat_var)
  dich_q <- rlang::enquo(dich_var)

  num_vec <- dplyr::pull(select_numeric(df, !!num_q,
                                stop_msg="`num_var` must be numeric"), !!num_q)
  cat_vec <- dplyr::pull(select_non_numeric(df, !!cat_q,
                                stop_msg="`cat_var` must be non-numeric"), !!cat_q)
  cat_vec  <- as.factor(cat_vec)

  has_dich <- !rlang::quo_is_null(dich_q) && !rlang::quo_is_missing(dich_q)

  if (has_dich) {
    dich_vec <- factor(dplyr::pull(df, !!dich_q))
    if (dplyr::n_distinct(dich_vec, na.rm = TRUE) != 2)
      stop("`dich_var` must have exactly two distinct values.", call. = FALSE)
  } else {
    dich_vec <- factor(rep("all", length(num_vec)))
  }

  df_plot <- tibble::tibble(value = num_vec, class = cat_vec, dich  = dich_vec)
  cols  <- if (has_dich) c("#111", "#ccc") else "#111"
  lvl_d <- levels(dich_vec)

  p <- ggplot2::ggplot()

  if (has_dich) {
    data1 <- dplyr::filter(df_plot, dich == lvl_d[1])
    data2 <- dplyr::filter(df_plot, dich == lvl_d[2])
    p <- p +
      gghalves::geom_half_violin(data = data1,
                                 ggplot2::aes(x = class, y = value, fill = dich),
                                 side = "l", trim = FALSE, colour = "grey20") +
      gghalves::geom_half_violin(data = data2,
                                 ggplot2::aes(x = class, y = value, fill = dich),
                                 side = "r", trim = FALSE, colour = "grey20")

    p <- p + ggplot2::scale_fill_manual(values = cols, name = rlang::as_label(dich_q))
  } else {
    p <- ggplot2::ggplot(df_plot) +
      ggplot2::geom_violin(
        ggplot2::aes(x = class, y = value),
        trim = FALSE, fill = cols, colour = "grey20"
      ) +
      ggplot2::guides(fill = "none")
  }
  if (horizontal) p <- p + ggplot2::coord_flip()
  p <- p + ggplot2::labs(x = rlang::as_label(cat_q), y = rlang::as_label(num_q))

  p <- p + ggplot2::theme_minimal(base_size = 12)
  print(p)

  # individual violins & stats
  box_list  <- list()
  stat_list <- list()
  for (lev in levels(cat_vec)) {
    df_sub <- dplyr::filter(df_plot, class == lev)

    plot_violin <- function(data, color) {
      ggplot2::geom_violin(
        data = data,
        ggplot2::aes(x = dich, y = value, fill = dich),
        trim = FALSE, colour = "grey20"
      )
    }
    g <- if (has_dich) {
      ggplot2::ggplot() +
        plot_violin(dplyr::filter(df_sub, dich == lvl_d[1]), cols[1]) +
        plot_violin(dplyr::filter(df_sub, dich == lvl_d[2]), cols[2]) +
        ggplot2::scale_fill_manual(values = cols, guide = "none") +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::labs(x=rlang::as_label(dich_q),
                      y=rlang::as_label(num_q),
                      title=rlang::as_label(cat_q)) +
        ggplot2::theme(
          plot.title  = ggplot2::element_text(size = 12, hjust = 0.5)
        )
    } else {
      ggplot2::ggplot(df_sub) +
        ggplot2::geom_violin(
          ggplot2::aes(x = class, y = value),
          trim = FALSE, fill = cols, colour = "grey20"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::guides(fill = "none") +
        ggplot2::labs(x=rlang::as_label(cat_q),
                      y=rlang::as_label(num_q))
    }
    if (horizontal) g <- g + ggplot2::coord_flip()

    box_list[[lev]] <- g
    if (!has_dich) {
      stats_level <- describe_data(dplyr::select(df_sub, value))$summary_numeric
      stat_list[[lev]] <- dplyr::mutate(stats_level, category = lev)
    } else {
      stats_0 <- df_sub |>
        dplyr::filter(dich==names(table(dich))[1]) |>
        dplyr::select(value) |>
        describe_data()
      stats_1 <- df_sub |>
        dplyr::filter(dich==names(table(dich))[2]) |>
        dplyr::select(value) |>
        describe_data()
      stats_level <- cbind(tibble::tibble(category=rep(lev, 2),
                                          binary=names(table(df_sub$dich))),
                           rbind(stats_0$summary_numeric,
                                 stats_1$summary_numeric))
      stat_list[[lev]] <- stats_level
    }
  }

  stats_tbl <- dplyr::bind_rows(stat_list) |>
    dplyr::select(category, dplyr::everything(), -variable)

  invisible(list(violins = box_list, stats = stats_tbl))
}

# Tests
# violin(mtcars)
# violin(mtcars, cyl, drat)
#
# violin1 = violin(tea, num_var=age, cat_var=where, dich_var=slimming)
# violin1$violins$"chain store"
# violin1$stats
#
# violin2 = violin(tea, num_var=age, cat_var=where, dich_var=slimming, horizontal=T)
# violin2$violins$"chain store"
# violin2$stats
#
# violin3 = violin(tea, num_var=age, cat_var=where)
# violin3$violins$"chain store"
# violin3$stats
#
# violin4 = violin(tea, num_var=age, cat_var=where, horizontal=T)
# violin4$violins$"chain store"
# violin4$stats
