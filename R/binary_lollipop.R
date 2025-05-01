#' Binary Lollipop Chart
#'
#' `binary_lollipop()` creates a single horizontal lollipop plot for each
#' selected dichotomous (binary) variable in a data frame.  Levels are ordered
#' according to existing factor levels (if set) or by ascending frequency.
#'
#' @param df A data frame or tibble.
#' @param vars Optional unquoted tidy-select specification of columns to plot.
#'   If `NULL` (default) all binary (two-level) variables in `df` are used.
#' @param percentage Logical; if `FALSE` (default), absolute counts are shown
#'   with one stem per level.  If `TRUE`, values are converted to percentages
#'   and a single connector is drawn between the two points.
#'
#' @details
#' * Only variables with exactly two distinct non-missing values are retained.
#' * If a column is a factor, its `levels()` determine the order of the two
#'   categories; otherwise, levels are sorted by increasing frequency.
#' * When `percentage = TRUE`, each variable’s values are rescaled to percentages,
#'   and a grey segment connects the minimum and maximum point.
#' * When `percentage = FALSE`, stems are drawn from zero to each level’s value,
#'   colored by level (`"#008"` for first, `"#800"` for second), with the longer
#'   stem drawn first so both remain visible.
#'
#' @return Invisibly returns `NULL`.  The primary side effect is printing the
#'   lollipop plot.
#'
#' @examples
#' # Example with built-in mtcars (treat am as binary)
#' mtcars$am <- factor(mtcars$am, levels = c(0, 1), labels = c("Auto", "Manual"))
#' binary_lollipop(mtcars, vars = c(am), percentage = FALSE)
#'
#' # Percentage mode
#' binary_lollipop(mtcars, vars = am, percentage = TRUE)
#'
#' @export
binary_lollipop <- function(df, vars = NULL, percentage = FALSE) {
  vars_q <- rlang::enquo(vars)
  df_sel <- if (rlang::quo_is_null(vars_q) || rlang::quo_is_missing(vars_q)) {
    df
  } else {
    dplyr::select(df, !!vars_q)
  }

  is_binary <- function(x) dplyr::n_distinct(x[!is.na(x)]) == 2
  data_bin  <- dplyr::select(df_sel, dplyr::where(is_binary))
  if (ncol(data_bin) == 0) {
    stop("No dichotomous variables found.", call. = FALSE)
  }

  # ── determine level order per variable (use existing factor levels if set)
  order_list <- purrr::imap(data_bin, function(col, nm) {
    if (is.factor(col)) {
      levels(col)
    } else {
      names(sort(table(col, useNA = "no")))
    }
  })

  # ── build summary tibble ───────────────────────────────────────────────────
  summary_df <- purrr::imap_dfr(order_list, function(levels_vec, nm) {
    vec <- data_bin[[nm]][!is.na(data_bin[[nm]])]
    cnt <- table(factor(vec, levels = levels_vec))
    tibble::tibble(
      variable = nm,
      level    = levels_vec,
      n        = as.integer(cnt),
      idx      = factor(seq_along(levels_vec)-1, levels = seq_along(levels_vec)-1)
    )
  }) |>
    dplyr::group_by(variable) |>
    dplyr::mutate(
      value = if (percentage) 100 * n / sum(n) else n
    ) |>
    dplyr::ungroup()

  # ── prepare segments for stems or connector
  if (percentage) {
    seg_df <- summary_df |>
      dplyr::group_by(variable) |>
      dplyr::summarise(
        x    = min(value),
        xend = max(value),
        .groups = "drop"
      )
  } else {
    seg_df <- summary_df |>
      dplyr::arrange(variable, dplyr::desc(value)) |>
      dplyr::transmute(
        variable, idx, x = 0, xend = value
      )
  }

  # ── plotting parameters
  pal   <- c("#008", "#800")
  x_max <- if (percentage) 100 else max(summary_df$value, na.rm = TRUE)

  # ── build and display the lollipop plot
  p <- ggplot2::ggplot()
  if (percentage) {
    p <- p +
      ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(x = x, xend = xend, y = variable, yend = variable),
        colour = "#888", linewidth = 0.9
      )
  } else {
    p <- p +
      ggplot2::geom_segment(
        data = seg_df,
        ggplot2::aes(x = x, xend = xend, y = variable, yend = variable,
                     colour = idx),
        linewidth = 0.9
      )
  }
  p <- p +
    ggplot2::geom_point(
      data = summary_df,
      ggplot2::aes(x = value, y = variable, colour = idx),
      size = 3.2
    ) +
    ggplot2::scale_colour_manual(values = pal, name = NULL) +
    ggplot2::scale_x_continuous(
      limits = c(0, x_max),
      expand = ggplot2::expansion(mult = c(0, 0.05))
    ) +
    ggplot2::labs(
      x = if (percentage) "Percentage (%)" else "Count",
      y = NULL
    ) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      plot.margin  = ggplot2::margin(6, 6, 6, 6, "pt")
    )

  print(p)
  invisible(NULL)
}


# TESTS
# level_list = list(pub=c("pub", "Not.pub"),
#                   slimming=c("No.slimming", "slimming"))
# df1 <- tea |>
#   mutate(dplyr::across(
#     all_of(names(level_list)),               # select only those columns
#     ~ factor(.x, levels = level_list[[dplyr::cur_column()]]),
#     .names = "{.col}"                        # keep same names
#   ))
# binary_lollipop(df1, vars=c(slimming, pub, healthy, friends))
# binary_lollipop(tea, vars=c(slimming, pub, healthy, friends))
# binary_lollipop(df1, vars=c(slimming, pub, healthy, friends), percentage=T)
# binary_lollipop(mtcars)
# binary_lollipop(mtcars, vars=c(cyl, drat, hp, qsec, wt))
